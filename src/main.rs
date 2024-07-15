#![doc = include_str!("../README.md")]
use std::process;

fn main() {
    if let Err(e) = poinciana::config::run() {
        eprintln!("Error: {e:?}");
        process::exit(1);
    }
}
