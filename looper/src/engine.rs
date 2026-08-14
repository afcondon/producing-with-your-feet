//! The looper proper: transport, layers, record, overdub, undo.
//!
//! Built on the arithmetic `align` proves. Two rules carry over and neither is
//! negotiable:
//!
//! - **Loop position is a device frame count**, never a host-clock instant. The
//!   two clocks differ by ~15.6 ppm here (DESIGN-LOOPER §10) and anything
//!   derived from the host clock walks away from the audio at 0.75 samples a
//!   second.
//! - **`out_frame = in_frame + K`**, with `K` established once at the first
//!   input callback and never recomputed. After that it is integer addition.
//!
//! ## Layers, not mixdown
//!
//! Every overdub is its own buffer, summed at playback (§4). Undo is then free,
//! and so is muting, reversing or re-rendering one layer while the rest play.
//!
//! ## How memory is handled, and why it looks odd
//!
//! Audio callbacks must not allocate, and two callbacks need access to the same
//! layer storage — the input side writes the layer being recorded while the
//! output side reads the ones already committed. The usual answers are unsafe
//! aliasing or a lock-free handoff of buffer ownership.
//!
//! Instead every sample is an `AtomicU32` holding f32 bits, accessed `Relaxed`.
//! On any machine this runs on that compiles to exactly the same load and store
//! as a plain `f32` — the atomics buy the absence of undefined behaviour, not
//! synchronisation, and cost nothing. The whole arena is allocated once at
//! startup, so no callback ever touches the allocator.
//!
//! The price is a fixed ceiling on loop length and layer count, which is what
//! `--max-secs` and `MAX_LAYERS` are. At the defaults the arena is 46 MB.

use cpal::traits::{DeviceTrait, StreamTrait};
use std::error::Error;
use std::io::BufRead;
use std::sync::atomic::{AtomicBool, AtomicI64, AtomicU32, AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Duration;

use crate::measure::{choose_input, choose_output, signed_secs, Width};

pub const MAX_LAYERS: usize = 8;

/// Transport states, as a `u8` because the audio thread reads it every buffer.
const IDLE: u8 = 0;
/// Waiting for the output callback to stamp the exact frame recording begins.
const ARMED: u8 = 1;
/// Recording the first loop: linear, and its length becomes the cycle.
const FIRST: u8 = 2;
/// Recording an overdub: modular, into a buffer one cycle long.
const OVERDUB: u8 = 3;
/// Playing, not recording.
const PLAYING: u8 = 4;

pub struct Opts {
    pub device: String,
    pub in_ch: usize,
    pub out_ch: usize,
    pub residual: f64,
    pub max_secs: f64,
    pub sample_rate: u32,
    pub buffer: Option<u32>,
    pub click: bool,
    pub selftest: Option<f64>,
}

impl Default for Opts {
    fn default() -> Self {
        Opts {
            device: String::new(),
            in_ch: 0,
            out_ch: 0,
            residual: 252.0,
            max_secs: 30.0,
            sample_rate: 48_000,
            buffer: None,
            click: false,
            selftest: None,
        }
    }
}

/// Everything both callbacks and the control thread touch.
struct Shared {
    arena: Vec<AtomicU32>,
    max_frames: usize,
    loop_len: AtomicUsize,
    n_layers: AtomicUsize,
    /// The output frame at which loop position zero sits.
    origin: AtomicI64,
    state: AtomicU8Wrapper,
    /// Set by the control thread, consumed by the output callback, which is the
    /// only place a transition can be stamped to an exact frame.
    request: AtomicU8Wrapper,
    out_frames: AtomicUsize,
    in_frames: AtomicUsize,
    k: AtomicI64,
    k_set: AtomicBool,
    p0: Mutex<Option<cpal::StreamInstant>>,
    buffer_frames: AtomicU32,
    click: AtomicBool,
    /// Highest position the first recording reached, so a loop can be closed at
    /// the right length even though the input trails the output.
    reached: AtomicUsize,
    overflowed: AtomicBool,
}

/// `AtomicU8` under a name that makes the intent obvious at the use sites.
struct AtomicU8Wrapper(std::sync::atomic::AtomicU8);
impl AtomicU8Wrapper {
    fn new(v: u8) -> Self {
        AtomicU8Wrapper(std::sync::atomic::AtomicU8::new(v))
    }
    fn get(&self) -> u8 {
        self.0.load(Ordering::Acquire)
    }
    fn set(&self, v: u8) {
        self.0.store(v, Ordering::Release)
    }
    fn take(&self) -> u8 {
        self.0.swap(0, Ordering::AcqRel)
    }
}

impl Shared {
    fn cell(&self, layer: usize, pos: usize) -> &AtomicU32 {
        &self.arena[layer * self.max_frames + pos]
    }
    fn read(&self, layer: usize, pos: usize) -> f32 {
        f32::from_bits(self.cell(layer, pos).load(Ordering::Relaxed))
    }
    fn write(&self, layer: usize, pos: usize, v: f32) {
        self.cell(layer, pos).store(v.to_bits(), Ordering::Relaxed)
    }
    fn add(&self, layer: usize, pos: usize, v: f32) {
        let c = self.cell(layer, pos);
        let cur = f32::from_bits(c.load(Ordering::Relaxed));
        c.store((cur + v).to_bits(), Ordering::Relaxed)
    }
    fn zero_layer(&self, layer: usize) {
        for i in 0..self.max_frames {
            self.cell(layer, i).store(0, Ordering::Relaxed);
        }
    }
}

pub fn run(opts: Opts) -> Result<(), Box<dyn Error>> {
    let candidate = crate::devices::find(&opts.device)?;
    let device = candidate.device;

    let mut in_cfg = choose_input(&device, opts.in_ch, opts.sample_rate, Width::Widest)
        .ok_or_else(|| format!("{} has no f32 input config", candidate.name))?;
    let mut out_cfg = choose_output(&device, opts.out_ch, opts.sample_rate, Width::Narrowest)
        .ok_or_else(|| format!("{} has no f32 output config", candidate.name))?;
    if let Some(n) = opts.buffer {
        in_cfg.buffer_size = cpal::BufferSize::Fixed(n);
        out_cfg.buffer_size = cpal::BufferSize::Fixed(n);
    }

    let sr = in_cfg.sample_rate.0;
    let sr_f = sr as f64;
    let in_channels = in_cfg.channels as usize;
    let out_channels = out_cfg.channels as usize;
    let max_frames = (opts.max_secs * sr_f).round() as usize;

    println!("Device: {}", candidate.name);
    println!(
        "Recording input {}, playing output {}, at {} Hz.",
        opts.in_ch, opts.out_ch, sr
    );
    println!(
        "Arena: {} layers x {:.0} s = {} MB\n",
        MAX_LAYERS,
        opts.max_secs,
        MAX_LAYERS * max_frames * 4 / 1_048_576
    );

    let sh = Arc::new(Shared {
        arena: (0..MAX_LAYERS * max_frames).map(|_| AtomicU32::new(0)).collect(),
        max_frames,
        loop_len: AtomicUsize::new(0),
        n_layers: AtomicUsize::new(0),
        origin: AtomicI64::new(0),
        state: AtomicU8Wrapper::new(IDLE),
        request: AtomicU8Wrapper::new(0),
        out_frames: AtomicUsize::new(0),
        in_frames: AtomicUsize::new(0),
        k: AtomicI64::new(0),
        k_set: AtomicBool::new(false),
        p0: Mutex::new(None),
        buffer_frames: AtomicU32::new(0),
        click: AtomicBool::new(opts.click || opts.selftest.is_some()),
        reached: AtomicUsize::new(0),
        overflowed: AtomicBool::new(false),
    });

    let err = |e| eprintln!("stream error: {}", e);

    let out_stream = {
        let sh = sh.clone();
        let ch = opts.out_ch;
        device.build_output_stream(
            &out_cfg,
            move |data: &mut [f32], info: &cpal::OutputCallbackInfo| {
                for s in data.iter_mut() {
                    *s = 0.0;
                }
                let frames = data.len() / out_channels;
                sh.buffer_frames.store(frames as u32, Ordering::Relaxed);

                let base = sh.out_frames.load(Ordering::Acquire);
                if base == 0 {
                    if let Ok(mut g) = sh.p0.lock() {
                        if g.is_none() {
                            *g = Some(info.timestamp().playback);
                        }
                    }
                }

                // Transitions are stamped here because this is the only thread
                // that knows the exact frame, and a loop boundary a buffer out
                // is a loop boundary that is audibly wrong.
                match sh.request.take() {
                    ARMED => {
                        sh.origin.store(base as i64, Ordering::Release);
                        sh.reached.store(0, Ordering::Release);
                        let n = sh.n_layers.load(Ordering::Acquire);
                        if n < MAX_LAYERS {
                            if sh.loop_len.load(Ordering::Acquire) == 0 {
                                sh.state.set(FIRST);
                            } else {
                                sh.state.set(OVERDUB);
                            }
                        }
                    }
                    PLAYING => sh.state.set(PLAYING),
                    IDLE => {}
                    _ => {}
                }

                let loop_len = sh.loop_len.load(Ordering::Acquire);
                let n = sh.n_layers.load(Ordering::Acquire);
                let origin = sh.origin.load(Ordering::Acquire);

                for f in 0..frames {
                    let out_frame = (base + f) as i64;
                    let mut v = 0.0f32;

                    if loop_len > 0 {
                        let pos = (out_frame - origin).rem_euclid(loop_len as i64) as usize;
                        for l in 0..n {
                            v += sh.read(l, pos);
                        }
                        if sh.click.load(Ordering::Relaxed) && pos < 16 {
                            v += 0.4;
                        }
                    }

                    data[f * out_channels + ch] = v;
                }
                sh.out_frames.store(base + frames, Ordering::Release);
            },
            err,
            None,
        )?
    };

    let in_stream = {
        let sh = sh.clone();
        let ch = opts.in_ch;
        let residual = opts.residual;
        device.build_input_stream(
            &in_cfg,
            move |data: &[f32], info: &cpal::InputCallbackInfo| {
                let frames = data.len() / in_channels;
                let base = sh.in_frames.load(Ordering::Acquire);

                if !sh.k_set.load(Ordering::Acquire) {
                    // The one consultation of the host clock in the whole engine.
                    let Ok(g) = sh.p0.try_lock() else {
                        sh.in_frames.store(base + frames, Ordering::Release);
                        return;
                    };
                    let Some(p0) = g.as_ref() else {
                        sh.in_frames.store(base + frames, Ordering::Release);
                        return;
                    };
                    let buffer = sh.buffer_frames.load(Ordering::Relaxed) as f64;
                    let offset = residual - 2.0 * buffer;
                    let c0 = signed_secs(p0, &info.timestamp().capture) * sr_f;
                    sh.k.store((c0 - base as f64 - offset).round() as i64, Ordering::Release);
                    sh.k_set.store(true, Ordering::Release);
                }

                let state = sh.state.get();
                if state != FIRST && state != OVERDUB {
                    sh.in_frames.store(base + frames, Ordering::Release);
                    return;
                }

                let k = sh.k.load(Ordering::Acquire);
                let origin = sh.origin.load(Ordering::Acquire);
                let loop_len = sh.loop_len.load(Ordering::Acquire);
                let layer = sh.n_layers.load(Ordering::Acquire);
                if layer >= MAX_LAYERS {
                    sh.in_frames.store(base + frames, Ordering::Release);
                    return;
                }

                for f in 0..frames {
                    let out_frame = (base + f) as i64 + k;
                    let rel = out_frame - origin;
                    if rel < 0 {
                        continue;
                    }
                    let v = data[f * in_channels + ch];

                    if state == FIRST {
                        // Linear. Its length becomes the cycle, so it must not
                        // wrap — and it stops rather than overwriting.
                        let pos = rel as usize;
                        if pos >= sh.max_frames {
                            sh.overflowed.store(true, Ordering::Relaxed);
                            continue;
                        }
                        sh.write(layer, pos, v);
                        sh.reached.fetch_max(pos + 1, Ordering::Relaxed);
                    } else {
                        // Modular: an overdub may go round as many times as it
                        // likes, summing into the same cycle.
                        if loop_len == 0 {
                            continue;
                        }
                        let pos = (rel % loop_len as i64) as usize;
                        sh.add(layer, pos, v);
                        sh.reached.fetch_max(loop_len, Ordering::Relaxed);
                    }
                }
                sh.in_frames.store(base + frames, Ordering::Release);
            },
            err,
            None,
        )?
    };

    out_stream.play()?;
    in_stream.play()?;
    std::thread::sleep(Duration::from_millis(300));

    if let Some(secs) = opts.selftest {
        let r = selftest(&sh, sr, secs);
        drop(in_stream);
        drop(out_stream);
        return r;
    }

    control_loop(&sh, sr);
    drop(in_stream);
    drop(out_stream);
    Ok(())
}

fn commit(sh: &Shared, sr: u32) {
    let state = sh.state.get();
    if state != FIRST && state != OVERDUB {
        return;
    }
    // Let the input drain: it trails the output by K, so the last frames of the
    // loop have not arrived yet. Without this the tail of every recording is
    // missing, which is exactly the kind of fault that sounds like "feel".
    sh.state.set(PLAYING);
    std::thread::sleep(Duration::from_millis(60));

    if state == FIRST {
        let len = sh.reached.load(Ordering::Acquire);
        if len == 0 {
            println!("  nothing recorded.");
            return;
        }
        sh.loop_len.store(len, Ordering::Release);
        println!(
            "  loop set: {} frames ({:.3} s), {:.1} bpm if that is one bar of 4/4",
            len,
            len as f64 / sr as f64,
            240.0 / (len as f64 / sr as f64)
        );
    }
    let n = sh.n_layers.fetch_add(1, Ordering::AcqRel) + 1;
    println!("  committed. {} layer{} playing.", n, if n == 1 { "" } else { "s" });
}

fn control_loop(sh: &Shared, sr: u32) {
    println!("Commands:  r = record/overdub toggle   u = undo   c = clear");
    println!("           k = click on/off   p = status   q = quit\n");

    let stdin = std::io::stdin();
    for line in stdin.lock().lines() {
        let Ok(line) = line else { break };
        match line.trim() {
            "r" => match sh.state.get() {
                FIRST | OVERDUB => commit(sh, sr),
                _ => {
                    if sh.n_layers.load(Ordering::Acquire) >= MAX_LAYERS {
                        println!("  {} layers is the ceiling; undo one first.", MAX_LAYERS);
                    } else {
                        sh.request.set(ARMED);
                        println!("  recording...");
                    }
                }
            },
            "u" => {
                let n = sh.n_layers.load(Ordering::Acquire);
                if n == 0 {
                    println!("  nothing to undo.");
                } else {
                    sh.n_layers.store(n - 1, Ordering::Release);
                    sh.zero_layer(n - 1);
                    println!("  layer {} removed, {} left.", n, n - 1);
                }
            }
            "c" => {
                sh.state.set(IDLE);
                sh.n_layers.store(0, Ordering::Release);
                sh.loop_len.store(0, Ordering::Release);
                for l in 0..MAX_LAYERS {
                    sh.zero_layer(l);
                }
                println!("  cleared.");
            }
            "k" => {
                let on = !sh.click.load(Ordering::Relaxed);
                sh.click.store(on, Ordering::Relaxed);
                println!("  click {}.", if on { "on" } else { "off" });
            }
            "p" => {
                let len = sh.loop_len.load(Ordering::Acquire);
                println!(
                    "  {} layers, loop {} frames ({:.3} s), state {}, K {:+}{}",
                    sh.n_layers.load(Ordering::Acquire),
                    len,
                    len as f64 / sr as f64,
                    match sh.state.get() {
                        FIRST => "recording first",
                        OVERDUB => "overdubbing",
                        PLAYING => "playing",
                        _ => "idle",
                    },
                    sh.k.load(Ordering::Acquire),
                    if sh.overflowed.load(Ordering::Relaxed) {
                        "   (a recording hit the arena ceiling)"
                    } else {
                        ""
                    }
                );
            }
            "q" | "" if line.trim() == "q" => break,
            "" => {}
            other => println!("  ? {:?}", other),
        }
    }
}

/// Record one cycle of the engine's own click through a loopback cable and ask
/// where it ended up. Same question `align` asks, but through the real transport
/// and the real layer storage — so it tests what will actually run.
fn selftest(sh: &Shared, sr: u32, secs: f64) -> Result<(), Box<dyn Error>> {
    let len = (secs * sr as f64).round() as usize;
    println!("Self-test: {} frame loop ({:.2} s), recording one cycle.", len, secs);

    sh.loop_len.store(len, Ordering::Release);
    sh.request.set(ARMED);
    std::thread::sleep(Duration::from_secs_f64(secs * 2.0 + 0.3));
    commit(sh, sr);
    std::thread::sleep(Duration::from_millis(200));

    let (e0, p0) = onset_of(sh, 0, len)
        .ok_or("nothing recorded — is the loopback cable patched from the output \
                jack to the input jack named by --out-ch / --in-ch?")?;
    println!(
        "  layer 0: click played at 0, recorded at {:+} samples ({:+.3} ms), peak {:.1} dBFS",
        e0,
        e0 as f64 / sr as f64 * 1000.0,
        20.0 * (p0.max(1e-9) as f64).log10()
    );

    // Now the property a looper actually stands on: that an overdub recorded
    // while listening to an existing layer lands on top of it. The click is
    // switched off, so the only thing going down the cable is layer 0 playing
    // back. If it returns to the same position, layers stack — and if it did
    // not, every overdub would sit a little further out than the last.
    println!("\nOverdub pass: click off, recording layer 0's own playback.");
    sh.click.store(false, Ordering::Relaxed);
    sh.request.set(ARMED);
    std::thread::sleep(Duration::from_secs_f64(secs * 2.0 + 0.3));
    commit(sh, sr);
    std::thread::sleep(Duration::from_millis(200));

    let (e1, p1) = onset_of(sh, 1, len)
        .ok_or("the overdub recorded nothing, though the first pass worked")?;
    println!(
        "  layer 1: layer 0's click returned at {:+} samples ({:+.3} ms), peak {:.1} dBFS",
        e1,
        e1 as f64 / sr as f64 * 1000.0,
        20.0 * (p1.max(1e-9) as f64).log10()
    );

    let slip = e1 - e0;
    println!("\n  Layer-to-layer slip: {:+} samples.", slip);

    if e0.abs() <= 2 && slip.abs() <= 2 {
        println!(
            "\n  Aligned through the real transport and the real layer storage, and\n  \
             overdubs stack on top of what they were recorded against. Eight layers\n  \
             deep will be as tight as one."
        );
        Ok(())
    } else if e0.abs() > 2 {
        Err(format!(
            "layer 0 is off by {} samples through the engine, though `align` passes — \
             the fault is in the transport, not the calibration",
            e0.abs()
        )
        .into())
    } else {
        Err(format!(
            "layer 0 lands correctly but the overdub slips {} samples against it. That \
             compounds: eight layers would end up {} samples apart",
            slip.abs(),
            slip.abs() * 8
        )
        .into())
    }
}

/// Onset position of the loudest thing in a layer, as a signed offset from loop
/// position zero, with its peak. Wrapping, because something landing slightly
/// early sits at the end of the loop rather than the start.
fn onset_of(sh: &Shared, layer: usize, len: usize) -> Option<(i64, f32)> {
    let mut peak = 0f32;
    let mut peak_at = 0usize;
    for i in 0..len {
        let v = sh.read(layer, i).abs();
        if v > peak {
            peak = v;
            peak_at = i;
        }
    }
    if peak < 0.01 {
        return None;
    }
    let mut onset = peak_at;
    for _ in 0..len {
        let prev = (onset + len - 1) % len;
        if sh.read(layer, prev).abs() <= 0.01 {
            break;
        }
        onset = prev;
    }
    let e = if onset > len / 2 { onset as i64 - len as i64 } else { onset as i64 };
    Some((e, peak))
}
