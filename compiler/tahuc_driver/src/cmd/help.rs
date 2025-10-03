use std::collections::VecDeque;

use tahuc_module::config::Config;

use crate::cmd::Command;

pub struct HelpCommand;

impl<'a> Command<'a> for HelpCommand {
    fn name(&self) -> &str {
        "-H, --help"
    }

    fn commands(&self) -> Vec<&str> {
        vec!["--help", "-H"]
    }

    fn description(&self) -> String {
        format!("Show help information")
    }

    fn help(&self) -> &str {
        "Usage: tahuc help [command]\n\nShow help information for a specific command or list all commands."
    }

    fn run(&self, _args: &mut VecDeque<String>, _config: &mut Config) -> Result<(), String> {
        Err("not implemented".to_string())
    }
}
