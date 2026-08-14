//! Round-trip latency measurement, and the routing map that comes with it.
//!
//! This exists before any looping code because of one fact: audio recorded
//! through an interface lands late by the full output→input round trip, and on
//! a looper **that error compounds on every overdub**. Layer three arrives
//! three round trips behind layer one, and nothing in the sound tells you it is
//! happening — it just gradually stops feeling like it is in time. Guessing the
//! number, or taking the interface's published figure, is not good enough,
//! because the figure that matters includes the converters, the buffer size in
//! force right now, and whatever is patched in between.
//!
//! So: emit a click, listen for it coming back, and count.
//!
//! The measurement is only as good as its clock. cpal hands each callback a
//! `StreamInstant` — for output, when the first frame of this buffer will
//! *reach the converter*; for input, when the first frame was *captured*. Both
//! derive from the host clock, so subtracting one from the other gives a real
//! interval rather than a difference of two callback-scheduling accidents.
//! Wall-clock timing at callback entry would be off by a buffer or more.
//!
//! **Every input channel is listened to, not just the one expected to answer.**
//! Two reasons, and the second is the important one. An interface with more USB
//! channels than jacks does not say which is which, so hearing the click name
//! its own channel is the cheapest possible routing map. And if the interface
//! has any internal monitoring path, a channel can hear the click with no cable
//! attached at all — which would otherwise yield a confident, precise, entirely
//! fictional latency. Listening everywhere makes that visible instead.
//!
//! Two paths are worth measuring and they are different numbers:
//!
//!   - **interface only** — a cable from an output back to an input. This is
//!     the floor, and the number to compensate a dry loop by.
//!   - **through the board** — output → pedalboard → input. This is the number
//!     that matters for anything recorded wet, and it is larger by whatever the
//!     pedals' converters add.

use cpal::traits::{DeviceTrait, StreamTrait};
use std::error::Error;
use std::sync::atomic::{AtomicU32, AtomicU8, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Duration;

const PHASE_SETTLE: u8 = 0;
const PHASE_NOISE: u8 = 1;
const PHASE_ARMED: u8 = 2;
const PHASE_LISTENING: u8 = 3;

/// Long enough to carry through a converter, short enough that the leading edge
/// is still the leading edge. A single-sample impulse is too quiet to clear the
/// noise floor on a line input with anything patched in front of it.
const BURST_FRAMES: usize = 16;

/// How long to keep listening after each click. Every channel gets the whole
/// window rather than the first one to answer stopping the others.
const LISTEN_MS: u64 = 400;

pub struct Opts {
    pub device: String,
    pub out_ch: usize,
    pub repeats: usize,
    pub amplitude: f32,
    pub sample_rate: u32,
}

impl Default for Opts {
    fn default() -> Self {
        Opts {
            device: String::new(),
            out_ch: 0,
            repeats: 8,
            amplitude: 0.5,
            sample_rate: 48_000,
        }
    }
}

/// How to break ties once a config is wide enough and hits the target rate.
#[derive(Clone, Copy, PartialEq)]
pub enum Width {
    /// Fewest channels that still reach the one we need — open the plain stereo
    /// pair rather than a sixteen-channel config we will not use.
    Narrowest,
    /// Every channel the device has. Metering and routing discovery want this:
    /// the point is to find out which channel the signal is on, so hiding any
    /// of them defeats it.
    Widest,
}

/// Pick an f32 config with more than `min_index` channels, at the target rate
/// if the device will do it.
fn choose<I>(configs: I, min_index: usize, target: u32, width: Width) -> Option<cpal::StreamConfig>
where
    I: Iterator<Item = cpal::SupportedStreamConfigRange>,
{
    let takes = |c: &cpal::SupportedStreamConfigRange| {
        c.min_sample_rate().0 <= target && target <= c.max_sample_rate().0
    };

    let mut best: Option<cpal::SupportedStreamConfigRange> = None;
    for c in configs {
        if c.sample_format() != cpal::SampleFormat::F32 {
            continue;
        }
        if (c.channels() as usize) <= min_index {
            continue;
        }
        let better = match &best {
            None => true,
            Some(b) => {
                // Reaching the target rate outranks any channel-count preference:
                // a measurement across a resampler is not a measurement.
                if takes(&c) != takes(b) {
                    takes(&c)
                } else if width == Width::Narrowest {
                    c.channels() < b.channels()
                } else {
                    c.channels() > b.channels()
                }
            }
        };
        if better {
            best = Some(c);
        }
    }

    best.map(|c| {
        let rate = if takes(&c) { cpal::SampleRate(target) } else { c.max_sample_rate() };
        c.with_sample_rate(rate).into()
    })
}

pub fn choose_input(
    device: &cpal::Device,
    min_index: usize,
    target: u32,
    width: Width,
) -> Option<cpal::StreamConfig> {
    choose(device.supported_input_configs().ok()?, min_index, target, width)
}

pub fn choose_output(
    device: &cpal::Device,
    min_index: usize,
    target: u32,
    width: Width,
) -> Option<cpal::StreamConfig> {
    choose(device.supported_output_configs().ok()?, min_index, target, width)
}

/// What one input channel had to say across the whole run.
struct ChannelResult {
    latencies_ms: Vec<f64>,
    peak: f32,
}

pub fn run(opts: Opts) -> Result<(), Box<dyn Error>> {
    let candidate = crate::devices::find(&opts.device)?;
    let device = candidate.device;
    println!("Device: {}", candidate.name);

    let in_cfg = choose_input(&device, 0, opts.sample_rate, Width::Widest)
        .ok_or_else(|| format!("{} has no f32 input config", candidate.name))?;
    let out_cfg = choose_output(&device, opts.out_ch, opts.sample_rate, Width::Narrowest)
        .ok_or_else(|| {
            format!(
                "{} has no f32 output config with more than {} channels",
                candidate.name, opts.out_ch
            )
        })?;

    if in_cfg.sample_rate != out_cfg.sample_rate {
        return Err(format!(
            "input is at {} Hz and output at {} Hz; a measurement across a rate \
             conversion is meaningless",
            in_cfg.sample_rate.0, out_cfg.sample_rate.0
        )
        .into());
    }

    let sr = in_cfg.sample_rate.0;
    let in_channels = in_cfg.channels as usize;
    let out_channels = out_cfg.channels as usize;
    println!(
        "Streams: {} Hz   listening on all {} inputs   clicking on output {} of {}",
        sr, in_channels, opts.out_ch, out_channels
    );

    let phase = Arc::new(AtomicU8::new(PHASE_SETTLE));
    // f32 bits in atomics. Positive IEEE-754 floats order the same as their bit
    // patterns as integers, so fetch_max on the bits is a genuine max.
    let noise: Arc<Vec<AtomicU32>> = Arc::new((0..in_channels).map(|_| AtomicU32::new(0)).collect());
    let thresholds: Arc<Vec<AtomicU32>> =
        Arc::new((0..in_channels).map(|_| AtomicU32::new(1.0f32.to_bits())).collect());
    let peaks: Arc<Vec<AtomicU32>> = Arc::new((0..in_channels).map(|_| AtomicU32::new(0)).collect());
    // Locking in an audio callback is not something the engine will do. Here the
    // critical sections are one pointer store each, run a handful of times in a
    // measurement that lasts seconds, and the alternative is a lock-free queue
    // that would obscure what this file is for.
    let fired: Arc<Mutex<Option<cpal::StreamInstant>>> = Arc::new(Mutex::new(None));
    let heard: Arc<Vec<Mutex<Option<cpal::StreamInstant>>>> =
        Arc::new((0..in_channels).map(|_| Mutex::new(None)).collect());

    let err = |e| eprintln!("stream error: {}", e);

    let out_stream = {
        let phase = phase.clone();
        let fired = fired.clone();
        let ch = opts.out_ch;
        let amp = opts.amplitude;
        device.build_output_stream(
            &out_cfg,
            move |data: &mut [f32], info: &cpal::OutputCallbackInfo| {
                for s in data.iter_mut() {
                    *s = 0.0;
                }
                if phase.load(Ordering::Acquire) != PHASE_ARMED {
                    return;
                }
                let frames = data.len() / out_channels;
                for f in 0..BURST_FRAMES.min(frames) {
                    data[f * out_channels + ch] = amp;
                }
                // The burst starts at frame 0, so the buffer's own playback
                // instant is the click's instant — no offset to add.
                if let Ok(mut g) = fired.lock() {
                    *g = Some(info.timestamp().playback);
                }
                phase.store(PHASE_LISTENING, Ordering::Release);
            },
            err,
            None,
        )?
    };

    let in_stream = {
        let phase = phase.clone();
        let noise = noise.clone();
        let thresholds = thresholds.clone();
        let peaks = peaks.clone();
        let heard = heard.clone();
        device.build_input_stream(
            &in_cfg,
            move |data: &[f32], info: &cpal::InputCallbackInfo| {
                let frames = data.len() / in_channels;
                match phase.load(Ordering::Acquire) {
                    PHASE_NOISE => {
                        for ch in 0..in_channels {
                            let mut peak = 0f32;
                            for f in 0..frames {
                                peak = peak.max(data[f * in_channels + ch].abs());
                            }
                            noise[ch].fetch_max(peak.to_bits(), Ordering::AcqRel);
                        }
                    }
                    PHASE_LISTENING => {
                        for ch in 0..in_channels {
                            let thr = f32::from_bits(thresholds[ch].load(Ordering::Acquire));
                            let mut peak = 0f32;
                            let mut onset: Option<usize> = None;
                            for f in 0..frames {
                                let v = data[f * in_channels + ch].abs();
                                peak = peak.max(v);
                                if onset.is_none() && v > thr {
                                    onset = Some(f);
                                }
                            }
                            peaks[ch].fetch_max(peak.to_bits(), Ordering::AcqRel);

                            if let Some(f) = onset {
                                if let Ok(mut g) = heard[ch].lock() {
                                    // Only the first crossing in the window counts;
                                    // later buffers are the tail, or a reflection.
                                    if g.is_none() {
                                        // Frame f of this buffer, not frame 0 — this
                                        // is where the sample accuracy comes from.
                                        *g = info.timestamp().capture.add(Duration::from_nanos(
                                            f as u64 * 1_000_000_000 / sr as u64,
                                        ));
                                    }
                                }
                            }
                        }
                    }
                    _ => {}
                }
            },
            err,
            None,
        )?
    };

    out_stream.play()?;
    in_stream.play()?;

    // Let CoreAudio settle before believing anything it says.
    std::thread::sleep(Duration::from_millis(300));

    phase.store(PHASE_NOISE, Ordering::Release);
    std::thread::sleep(Duration::from_millis(500));
    phase.store(PHASE_SETTLE, Ordering::Release);

    println!("\nNoise floor per input, and the threshold set from it:");
    for ch in 0..in_channels {
        let floor = f32::from_bits(noise[ch].load(Ordering::Acquire));
        // Eight times the noise floor, with an absolute minimum so a silent
        // input does not give a threshold of zero and "detect" the first
        // dithered sample.
        let thr = (floor * 8.0).max(0.01);
        thresholds[ch].store(thr.to_bits(), Ordering::Release);
        println!(
            "  ch {:>2}  floor {:>8.1} dBFS   threshold {:>8.1} dBFS{}",
            ch,
            dbfs(floor),
            dbfs(thr),
            if floor > 0.05 { "   <- noisy; may trigger on the wrong thing" } else { "" }
        );
    }

    let mut results: Vec<ChannelResult> =
        (0..in_channels).map(|_| ChannelResult { latencies_ms: Vec::new(), peak: 0.0 }).collect();

    println!("\nClicking {} times:", opts.repeats);
    for i in 0..opts.repeats {
        *fired.lock().unwrap() = None;
        for ch in 0..in_channels {
            *heard[ch].lock().unwrap() = None;
            peaks[ch].store(0, Ordering::Release);
        }

        phase.store(PHASE_ARMED, Ordering::Release);
        std::thread::sleep(Duration::from_millis(LISTEN_MS));
        phase.store(PHASE_SETTLE, Ordering::Release);

        let f = fired.lock().unwrap().clone();
        let Some(f) = f else {
            println!("  {:>2}.  the click never went out", i + 1);
            continue;
        };

        let mut answered: Vec<String> = Vec::new();
        for ch in 0..in_channels {
            results[ch].peak = results[ch]
                .peak
                .max(f32::from_bits(peaks[ch].load(Ordering::Acquire)));
            let h = heard[ch].lock().unwrap().clone();
            if let Some(h) = h {
                let secs = signed_secs(&f, &h);
                results[ch].latencies_ms.push(secs * 1000.0);
                answered.push(format!("ch{} {:.2}ms", ch, secs * 1000.0));
            }
        }

        if answered.is_empty() {
            println!("  {:>2}.  silence on every input", i + 1);
        } else {
            println!("  {:>2}.  {}", i + 1, answered.join("   "));
        }

        // Let any tail decay so the next click is not detected against a ring.
        std::thread::sleep(Duration::from_millis(200));
    }

    drop(in_stream);
    drop(out_stream);

    report(&mut results, sr, opts.repeats)
}

fn dbfs(x: f32) -> f64 {
    20.0 * (x.max(1e-9) as f64).log10()
}

/// Signed, deliberately. A negative interval is not nonsense: it means the input
/// saw the click sooner than CoreAudio's reported playback instant, which is
/// exactly what a virtual loopback device does — it copies the buffer in
/// software, so the output latency it advertises is never actually spent. Real
/// converters with a real path between them give positive numbers.
fn signed_secs(fired: &cpal::StreamInstant, heard: &cpal::StreamInstant) -> f64 {
    match heard.duration_since(fired) {
        Some(d) => d.as_secs_f64(),
        None => -fired.duration_since(heard).map(|d| d.as_secs_f64()).unwrap_or(0.0),
    }
}

fn median(sorted: &[f64]) -> f64 {
    let n = sorted.len();
    if n % 2 == 0 {
        (sorted[n / 2 - 1] + sorted[n / 2]) / 2.0
    } else {
        sorted[n / 2]
    }
}

fn report(results: &mut [ChannelResult], sr: u32, repeats: usize) -> Result<(), Box<dyn Error>> {
    let responders: Vec<usize> = (0..results.len())
        .filter(|&ch| !results[ch].latencies_ms.is_empty())
        .collect();

    if responders.is_empty() {
        return Err("nothing came back on any input.\n\
             \n\
             The usual causes, in order of likelihood:\n\
               - no signal path from the output back to an input. The loopback is\n\
                 not implied; for an interface-only figure that means a cable.\n\
               - wrong output channel. Try --out-ch; it is zero-based.\n\
               - output muted, or its level at zero, in the interface's own mixer.\n\
               - amplitude too low for the input's gain staging. Try --amp 0.9."
            .into());
    }

    println!("\n  channel   heard   median latency        samples      peak");
    for &ch in &responders {
        results[ch]
            .latencies_ms
            .sort_by(|a, b| a.partial_cmp(b).unwrap());
        let m = median(&results[ch].latencies_ms);
        let n = results[ch].latencies_ms.len();
        let lo = results[ch].latencies_ms[0];
        let hi = results[ch].latencies_ms[n - 1];
        println!(
            "  ch {:>2}    {:>2}/{:<2}   {:>8.3} ms         {:>7.0}   {:>7.1} dBFS{}",
            ch,
            n,
            repeats,
            m,
            m / 1000.0 * sr as f64,
            dbfs(results[ch].peak),
            if hi - lo > 1.0 { "   (unstable)" } else { "" }
        );
    }

    // The routing map is the first thing to read off this. One responder is the
    // ordinary case and names the channel that cable arrives on.
    if responders.len() > 1 {
        println!(
            "\n  ! !  {} inputs heard the same click. If only one cable is patched,\n       \
             the others are an internal monitoring path inside the interface —\n       \
             and a latency measured down one of those is fiction, because no\n       \
             converter was crossed.",
            responders.len()
        );
    }

    // Then the number itself, from the strongest responder.
    let best = *responders
        .iter()
        .max_by(|&&a, &&b| results[a].peak.partial_cmp(&results[b].peak).unwrap())
        .unwrap();
    let lats = &results[best].latencies_ms;
    let m = median(lats);
    let spread = lats[lats.len() - 1] - lats[0];

    println!(
        "\n  Strongest responder: ch {}  —  {:.3} ms, {:.0} samples at {} Hz",
        best,
        m,
        m / 1000.0 * sr as f64,
        sr
    );
    println!("  spread {:.3} ms over {} clicks", spread, lats.len());

    if lats.len() < repeats {
        println!(
            "\n  Only {} of {} clicks were heard. An intermittent path is worse than\n  \
             a slow one — check the cable before trusting the median.",
            lats.len(),
            repeats
        );
    }

    // The spread is the part to read. A tight cluster means the number can be
    // used as a constant; a wide one means something is resampling, or the
    // buffer size is being renegotiated, and compensating by the median would
    // be compensating by a number that was never true.
    if spread > 1.0 {
        println!(
            "\n  ! !  {:.1} ms of spread is too much to compensate with a constant.\n       \
             Check that nothing is resampling and that the buffer size is fixed.",
            spread
        );
    } else if m < 0.0 {
        println!(
            "\n  Negative, so this is a virtual loopback rather than a signal path\n  \
             through converters. It proves the measurement works; it is not a\n  \
             latency you can compensate anything with."
        );
    } else {
        println!("\n  Tight enough to use as a constant.");
    }
    Ok(())
}
