//! Round-trip latency measurement.
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
use std::time::{Duration, Instant};

const PHASE_SETTLE: u8 = 0;
const PHASE_NOISE: u8 = 1;
const PHASE_ARMED: u8 = 2;
const PHASE_LISTENING: u8 = 3;
const PHASE_HEARD: u8 = 4;

/// Long enough to carry through a converter, short enough that the leading edge
/// is still the leading edge. A single-sample impulse is too quiet to clear the
/// noise floor on a line input with anything patched in front of it.
const BURST_FRAMES: usize = 16;

pub struct Opts {
    pub device: String,
    pub out_ch: usize,
    pub in_ch: usize,
    pub repeats: usize,
    pub amplitude: f32,
    pub sample_rate: u32,
}

impl Default for Opts {
    fn default() -> Self {
        Opts {
            device: String::new(),
            out_ch: 0,
            in_ch: 0,
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
    /// Every channel the device has. Metering wants this: the point is to find
    /// out which channel the signal is on, so hiding any of them defeats it.
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

pub fn run(opts: Opts) -> Result<(), Box<dyn Error>> {
    let candidate = crate::devices::find(&opts.device)?;
    let device = candidate.device;
    println!("Device: {}", candidate.name);

    let in_cfg = choose_input(&device, opts.in_ch, opts.sample_rate, Width::Narrowest)
        .ok_or_else(|| {
            format!(
                "{} has no f32 input config with more than {} channels",
                candidate.name, opts.in_ch
            )
        })?;
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
        "Streams: {} Hz   in {} ch (listening on {})   out {} ch (clicking on {})",
        sr, in_channels, opts.in_ch, out_channels, opts.out_ch
    );

    let phase = Arc::new(AtomicU8::new(PHASE_SETTLE));
    // f32 bits in an atomic. Positive IEEE-754 floats order the same as their
    // bit patterns as integers, so fetch_max on the bits is a genuine max.
    let noise = Arc::new(AtomicU32::new(0));
    let threshold = Arc::new(AtomicU32::new(1.0f32.to_bits()));
    // Locking in an audio callback is not something the engine will do. Here the
    // critical section is one pointer store, run a handful of times in a
    // measurement that lasts seconds, and the alternative is a lock-free queue
    // that would obscure what this file is for.
    let fired: Arc<Mutex<Option<cpal::StreamInstant>>> = Arc::new(Mutex::new(None));
    let heard: Arc<Mutex<Option<cpal::StreamInstant>>> = Arc::new(Mutex::new(None));

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
        let threshold = threshold.clone();
        let heard = heard.clone();
        let ch = opts.in_ch;
        device.build_input_stream(
            &in_cfg,
            move |data: &[f32], info: &cpal::InputCallbackInfo| {
                let frames = data.len() / in_channels;
                match phase.load(Ordering::Acquire) {
                    PHASE_NOISE => {
                        let mut peak = 0f32;
                        for f in 0..frames {
                            peak = peak.max(data[f * in_channels + ch].abs());
                        }
                        noise.fetch_max(peak.to_bits(), Ordering::AcqRel);
                    }
                    PHASE_LISTENING => {
                        let thr = f32::from_bits(threshold.load(Ordering::Acquire));
                        for f in 0..frames {
                            if data[f * in_channels + ch].abs() > thr {
                                // Frame f of this buffer, not frame 0 — this is
                                // where the sample accuracy comes from.
                                let at = info.timestamp().capture.add(Duration::from_nanos(
                                    f as u64 * 1_000_000_000 / sr as u64,
                                ));
                                if let Ok(mut g) = heard.lock() {
                                    *g = at;
                                }
                                phase.store(PHASE_HEARD, Ordering::Release);
                                break;
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

    let floor = f32::from_bits(noise.load(Ordering::Acquire));
    // Eight times the noise floor, with an absolute minimum so a silent input
    // does not give us a threshold of zero and a "detection" on the first
    // dithered sample.
    let thr = (floor * 8.0).max(0.01);
    threshold.store(thr.to_bits(), Ordering::Release);
    println!(
        "Noise floor {:.5} ({:.1} dBFS)   threshold {:.5}\n",
        floor,
        20.0 * floor.max(1e-9).log10(),
        thr
    );
    if floor > 0.05 {
        println!(
            "  !!  That is a noisy input. If something is playing into it, the \
             measurement\n      will trigger on the wrong thing.\n"
        );
    }

    let mut results: Vec<f64> = Vec::new();
    for i in 0..opts.repeats {
        *fired.lock().unwrap() = None;
        *heard.lock().unwrap() = None;
        phase.store(PHASE_ARMED, Ordering::Release);

        let started = Instant::now();
        while phase.load(Ordering::Acquire) != PHASE_HEARD {
            if started.elapsed() > Duration::from_millis(1500) {
                break;
            }
            std::thread::sleep(Duration::from_millis(1));
        }

        if phase.load(Ordering::Acquire) != PHASE_HEARD {
            println!("  {:>2}.  no click came back", i + 1);
            phase.store(PHASE_SETTLE, Ordering::Release);
        } else {
            let f = fired.lock().unwrap().clone();
            let h = heard.lock().unwrap().clone();
            phase.store(PHASE_SETTLE, Ordering::Release);
            match (f, h) {
                // Signed, deliberately. A negative interval is not nonsense: it
                // means the input saw the click sooner than CoreAudio's reported
                // playback instant, which is exactly what a virtual loopback
                // device does — it copies the buffer in software, so the output
                // latency it advertises is never actually spent. Real converters
                // with a real path between them give positive numbers.
                (Some(f), Some(h)) => {
                    let secs = match h.duration_since(&f) {
                        Some(d) => d.as_secs_f64(),
                        None => -f.duration_since(&h).map(|d| d.as_secs_f64()).unwrap_or(0.0),
                    };
                    let ms = secs * 1000.0;
                    println!(
                        "  {:>2}.  {:>8.3} ms   {:>6.0} samples",
                        i + 1,
                        ms,
                        secs * sr as f64
                    );
                    results.push(ms);
                }
                _ => println!("  {:>2}.  lost a timestamp", i + 1),
            }
        }
        // Let any tail decay so the next click is not detected against a ring.
        std::thread::sleep(Duration::from_millis(250));
    }

    drop(in_stream);
    drop(out_stream);

    report(&mut results, sr)
}

fn report(results: &mut Vec<f64>, sr: u32) -> Result<(), Box<dyn Error>> {
    if results.is_empty() {
        return Err("nothing came back at all.\n\
             \n\
             The usual causes, in order of likelihood:\n\
               - no signal path from the output back to the input. For an \
             interface-only\n\
                 measurement that means a physical cable; the loopback is not \
             implied.\n\
               - wrong channel. Try --out-ch / --in-ch; they are zero-based.\n\
               - output muted, or its level at zero, in the interface's own mixer.\n\
               - amplitude too low for the input's gain staging. Try --amp 0.9."
            .into());
    }

    results.sort_by(|a, b| a.partial_cmp(b).unwrap());
    let n = results.len();
    let median = if n % 2 == 0 {
        (results[n / 2 - 1] + results[n / 2]) / 2.0
    } else {
        results[n / 2]
    };
    let lo = results[0];
    let hi = results[n - 1];

    println!("\n  {} of {} clicks returned", n, n);
    println!(
        "  median  {:.3} ms   {:.0} samples at {} Hz",
        median,
        median / 1000.0 * sr as f64,
        sr
    );
    println!("  spread  {:.3} – {:.3} ms  ({:.3} ms)", lo, hi, hi - lo);

    if median < 0.0 {
        println!(
            "\n  Negative, so this is a virtual loopback rather than a signal path\n  \
             through converters. It proves the measurement works; it is not a\n  \
             latency you can compensate anything with."
        );
    }

    // The spread is the part to read. A tight cluster means the number can be
    // used as a constant; a wide one means something is resampling, or the
    // buffer size is being renegotiated, and compensating by the median would
    // be compensating by a number that was never true.
    if hi - lo > 1.0 {
        println!(
            "\n  ! !  {:.1} ms of spread is too much to compensate with a constant.\n       \
             Check that nothing is resampling and that the buffer size is fixed.",
            hi - lo
        );
    } else {
        println!("\n  Tight enough to use as a constant.");
    }
    Ok(())
}
