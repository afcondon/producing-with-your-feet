//! pwyf-looper — the looper engine for producing-with-your-feet.
//!
//! See `docs/DESIGN-LOOPER.md` for what this is going to be. Today it is the
//! metrology: §10 of that document lists three latencies that have to be
//! measured rather than guessed, and §15 says to measure the audio round trip
//! before writing any overdub code. This is that.

mod devices;
mod levels;
mod measure;

use std::process::ExitCode;

const USAGE: &str = "\
pwyf-looper — looper engine for producing-with-your-feet

USAGE
  pwyf-looper devices
      List the audio devices CoreAudio can see, with channel counts and the
      sample rates each will accept.

  pwyf-looper levels --device <name> [--seconds <n>]
      Live peak meter on every input channel, with a peak hold. Play into
      one jack at a time to find out which host channel it arrives on —
      an interface with more USB channels than physical jacks does not
      tell you this, and guessing wrong records silence.

  pwyf-looper sweep --device <name> [options]
      The calibration. Measures at several buffer sizes and separates the
      two things a single reading confuses: a real converter delay, and a
      bookkeeping error in the timestamps. Only one of them moves with the
      buffer, so varying it tells them apart.

      Reports the buffer-independent residual — the interface's own round
      trip, the number recordings are compensated by — and the slope, which
      is the correction to apply to raw timestamp arithmetic.

      Same options as `measure`, minus --buffer, which it varies itself.

  pwyf-looper measure --device <name> [options]
      Measure output→input round-trip latency by clicking and listening on
      every input at once. Needs a signal path from an output back to an
      input: a cable for the interface-only figure, or out → pedalboard →
      in for the figure that applies to anything recorded wet.

      Because it listens everywhere, one run also says which input channel
      that cable arrives on — and exposes any internal monitoring path,
      where a channel hears the click having crossed no converter at all.

      --device <name>   substring of the device name, case-insensitive
      --out-ch <n>      output channel to click on   (default 0, zero-based)
      --repeats <n>     how many clicks               (default 8)
      --amp <0..1>      click amplitude               (default 0.5)
      --rate <hz>       preferred sample rate         (default 48000)
      --buffer <n>      ask for a fixed callback size (default: device's own)
                        Diagnostic: if the measured offset moves with this,
                        it is buffer accounting rather than a property of
                        the interface, and must be stored per buffer size.

  This emits a short, loud click. Take headphones off and turn amps down.
";

fn main() -> ExitCode {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let cmd = args.first().map(String::as_str).unwrap_or("help");

    match cmd {
        "devices" => {
            devices::list();
            ExitCode::SUCCESS
        }
        "levels" => match parse_levels(&args[1..]) {
            Ok(opts) => match levels::run(opts) {
                Ok(()) => ExitCode::SUCCESS,
                Err(e) => {
                    eprintln!("\n{}", e);
                    ExitCode::FAILURE
                }
            },
            Err(e) => {
                eprintln!("{}\n\n{}", e, USAGE);
                ExitCode::FAILURE
            }
        },
        "sweep" => match parse_measure(&args[1..]) {
            Ok(opts) => match measure::sweep(opts) {
                Ok(()) => ExitCode::SUCCESS,
                Err(e) => {
                    eprintln!("\n{}", e);
                    ExitCode::FAILURE
                }
            },
            Err(e) => {
                eprintln!("{}\n\n{}", e, USAGE);
                ExitCode::FAILURE
            }
        },
        "measure" => match parse_measure(&args[1..]) {
            Ok(opts) => match measure::run(opts) {
                Ok(()) => ExitCode::SUCCESS,
                Err(e) => {
                    eprintln!("\n{}", e);
                    ExitCode::FAILURE
                }
            },
            Err(e) => {
                eprintln!("{}\n\n{}", e, USAGE);
                ExitCode::FAILURE
            }
        },
        "help" | "-h" | "--help" => {
            print!("{}", USAGE);
            ExitCode::SUCCESS
        }
        other => {
            eprintln!("unknown command {:?}\n\n{}", other, USAGE);
            ExitCode::FAILURE
        }
    }
}

fn parse_levels(args: &[String]) -> Result<levels::Opts, String> {
    let mut opts = levels::Opts::default();
    let mut i = 0;
    while i < args.len() {
        let flag = args[i].as_str();
        let value = args
            .get(i + 1)
            .cloned()
            .ok_or_else(|| format!("{} needs a value", flag))?;
        match flag {
            "--device" => opts.device = value,
            "--seconds" => {
                opts.seconds = value.parse().map_err(|_| "--seconds wants an integer")?
            }
            "--rate" => opts.sample_rate = value.parse().map_err(|_| "--rate wants an integer")?,
            other => return Err(format!("unknown option {:?}", other)),
        }
        i += 2;
    }
    if opts.device.is_empty() {
        return Err("levels needs --device".into());
    }
    Ok(opts)
}

fn parse_measure(args: &[String]) -> Result<measure::Opts, String> {
    let mut opts = measure::Opts::default();
    let mut i = 0;
    while i < args.len() {
        let flag = args[i].as_str();
        let value = || {
            args.get(i + 1)
                .cloned()
                .ok_or_else(|| format!("{} needs a value", flag))
        };
        match flag {
            "--device" => opts.device = value()?,
            "--out-ch" => opts.out_ch = value()?.parse().map_err(|_| "--out-ch wants an integer")?,
            "--repeats" => {
                opts.repeats = value()?.parse().map_err(|_| "--repeats wants an integer")?
            }
            "--amp" => opts.amplitude = value()?.parse().map_err(|_| "--amp wants a number")?,
            "--buffer" => {
                opts.buffer =
                    Some(value()?.parse().map_err(|_| "--buffer wants an integer")?)
            }
            "--rate" => {
                opts.sample_rate = value()?.parse().map_err(|_| "--rate wants an integer")?
            }
            other => return Err(format!("unknown option {:?}", other)),
        }
        i += 2;
    }

    if opts.device.is_empty() {
        return Err("measure needs --device".into());
    }
    if !(0.0..=1.0).contains(&opts.amplitude) {
        return Err("--amp must be between 0 and 1".into());
    }
    if opts.repeats == 0 {
        return Err("--repeats must be at least 1".into());
    }
    Ok(opts)
}
