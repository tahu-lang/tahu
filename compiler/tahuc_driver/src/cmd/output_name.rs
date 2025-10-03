use std::collections::VecDeque;

use tahuc_module::config::Config;

use crate::cmd::Command;

pub struct OutputName;

impl<'a> Command<'a> for OutputName {
    fn name(&self) -> &str {
        "-o"
    }

    fn description(&self) -> String {
        format!("Set the output file name")
    }

    fn help(&self) -> &str {
        "Usage: tahuc -o <file>\n\nSet the output file name"
    }

    fn commands(&self) -> Vec<&str> {
        vec!["-o"]
    }

    fn run(&self, args: &mut VecDeque<String>, config: &mut Config) -> Result<(), String> {
        if args[1].is_empty() {
            return Err(format!("Missing output file name"));
        }
        config.output_name = Some(args[1].clone());
        args.pop_front();
        args.pop_front();

        Ok(())
    }
}
