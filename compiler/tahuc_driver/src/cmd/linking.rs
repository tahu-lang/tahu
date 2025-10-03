use std::collections::VecDeque;

use tahuc_module::config::Config;

use crate::cmd::Command;

pub struct Linking;

pub struct LinkDir;

impl<'a> Command<'a> for Linking {
    fn name(&self) -> &str {
        "-l"
    }

    fn commands(&self) -> Vec<&str> {
        vec!["-l"]
    }

    fn use_prefix(&self) -> bool {
        true
    }

    fn description(&self) -> String {
        format!("include library to link")
    }

    fn help(&self) -> &str {
        "Usage: -l<lib>\n\nfile lib to link"
    }

    fn run(&self, args: &mut VecDeque<String>, config: &mut Config) -> Result<(), String> {
        if let Some(lib) = args.get(0) {
            let lib: Vec<&str> = lib.split("-l").collect();

            if lib.len() != 2 {
                return Err(format!("Invalid library: {}", args[0]));
            }

            let lib = lib[1];
            if lib.is_empty() {
                return Err(format!("Invalid library: {}", args[0]));
            }

            config.linking.push(lib.to_string());
            args.pop_front();
            return Ok(());
        }
        Err(format!("args {:?}", args))
    }
}

impl<'a> Command<'a> for LinkDir {
    fn name(&self) -> &str {
        "-L"
    }

    fn commands(&self) -> Vec<&str> {
        vec!["-L"]
    }

    fn use_prefix(&self) -> bool {
        true
    }

    fn description(&self) -> String {
        format!("search directory for libraries")
    }

    fn help(&self) -> &str {
        "Usage: -L<path>\n\npath to search libraries"
    }

    fn run(&self, args: &mut VecDeque<String>, config: &mut Config) -> Result<(), String> {
        if let Some(lib) = args.get(0) {
            let lib: Vec<&str> = lib.split("-L").collect();

            if lib.len() != 2 {
                return Err(format!("Invalid path: {}", args[0]));
            }

            let lib = lib[1];
            if lib.is_empty() {
                return Err(format!("Invalid path: {}", args[0]));
            }

            config.link_dir.push(lib.to_string());
            args.pop_front();
            return Ok(());
        }
        Err(format!("args {:?}", args))
    }
}
