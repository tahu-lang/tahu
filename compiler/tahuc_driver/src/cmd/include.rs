use std::collections::VecDeque;

use tahuc_module::config::Config;

use crate::cmd::Command;

pub struct Include;

impl<'a> Command<'a> for Include {
    fn name(&self) -> &str {
        "-I"
    }

    fn commands(&self) -> Vec<&str> {
        vec!["-I"]
    }

    fn use_prefix(&self) -> bool {
        true
    }

    fn description(&self) -> String {
        format!("include directory to search files .tahu")
    }

    fn help(&self) -> &str {
        "Usage: tahuc -I<path>\n\nInclude directory to search files .tahu"
    }

    fn run(&self, args: &mut VecDeque<String>, config: &mut Config) -> Result<(), String> {
        if let Some(path) = args.get(0) {
            let path: Vec<&str> = path.split("-I").collect();

            if path.len() != 2 {
                return Err(format!("Invalid path: {}", args[0]));
            }

            let path = path[1];
            if path.is_empty() {
                return Err(format!("Invalid path: {}", args[0]));
            }

            config.include_dirs.push(path.to_string());
            args.pop_front();
            return Ok(());
        }
        Err(format!("args {:?}", args))
    }
}
