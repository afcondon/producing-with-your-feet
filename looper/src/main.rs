//! pwyf-looper — the looper engine for producing-with-your-feet.
//!
//! See `docs/DESIGN-LOOPER.md` for what this is going to be. Today it is the
//! metrology: §10 of that document lists three latencies that have to be
//! measured rather than guessed, and §15 says to measure the audio round trip
//! before writing any overdub code. This is that.

mod devices;
mod measure;

use std::process::ExitCode;

const USAGE: &str = "\
pwyf-looper — looper engine for producing-with-your-feet

USAGE
  pwyf-looper devices
      List the audio devices CoreAudio can see, with channel counts and the
      sample rates each will accept.

  pwyf-looper measure --device <name> [options]
      Measure output→input round-trip latency by clicking and listening.
      Needs a signal path from an output back to an input: a cable for the
      interface-only figure, or out → pedalboard → in for the figure that
      applies to anything recorded wet.

      --device <name>   substring of the device name, case-insensitive
      --out-ch <n>      output channel to click on   (default 0, zero-based)
      --in-ch <n>       input channel to listen on   (default 0, zero-based)
      --repeats <n>     how many clicks               (default 8)
      --amp <0..1>      click amplitude               (default 0.5)
      --rate <hz>       preferred sample rate         (default 48000)

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
            "--in-ch" => opts.in_ch = value()?.parse().map_err(|_| "--in-ch wants an integer")?,
            "--repeats" => {
                opts.repeats = value()?.parse().map_err(|_| "--repeats wants an integer")?
            }
            "--amp" => opts.amplitude = value()?.parse().map_err(|_| "--amp wants a number")?,
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
