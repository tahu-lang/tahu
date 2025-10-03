use std::collections::VecDeque;

use tahuc_module::config::Config;

use crate::cmd::Command;

pub struct VersionCommand;

impl<'a> Command<'a> for VersionCommand {
    fn name(&self) -> &str {
        "-V, --version"
    }

    fn commands(&self) -> Vec<&str> {
        vec!["--version", "-V"]
    }

    fn description(&self) -> String {
        format!("Show version information")
    }

    fn help(&self) -> &str {
        "Usage: tahuc --version\n\nShow version information."
    }

    fn run(&self, _args: &mut VecDeque<String>, _config: &mut Config) -> Result<(), String> {
        println!("0.1.0");

        Ok(())
    }
}
