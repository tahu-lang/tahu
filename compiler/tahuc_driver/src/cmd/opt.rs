use std::collections::VecDeque;

use tahuc_module::config::{Config, OptLevel};

use crate::cmd::Command;

pub struct Opt;

impl<'a> Command<'a> for Opt {
    fn name(&self) -> &str {
        "-O[0-3]"
    }

    fn description(&self) -> String {
        format!(
            "Set the optimization level.\n-O0 (no optimizations)\n-O1 (some optimizations)\n-O2 (default, good balance)\n-O3 (all optimizations)"
        )
    }

    fn commands(&self) -> Vec<&str> {
        vec!["-O0", "-O1", "-O2", "-O3"]
    }

    fn help(&self) -> &str {
        "Usage: tahuc -O[0-3]\n\nSet the optimization level.\n-O0 (no optimizations)\n-O1 (some optimizations)\n-O2 (default, good balance)\n-O3 (all optimizations)"
    }

    fn run(&self, args: &mut VecDeque<String>, config: &mut Config) -> Result<(), String> {
        match args[0].as_str() {
            "-O0" => config.set_opt(OptLevel::None),
            "-O1" => config.set_opt(OptLevel::Low),
            "-O2" => config.set_opt(OptLevel::Medium),
            "-O3" => config.set_opt(OptLevel::High),
            _ => return Err(format!("Unknown command: {}", args[0])),
        }

        args.pop_front();

        Ok(())
    }
}
