use crate::cmd::Command;

pub struct Test;

impl<'a> Command<'a> for Test {
    fn name(&self) -> &str {
        "--test"
    }

    fn commands(&self) -> Vec<&str> {
        vec!["--test"]
    }

    fn description(&self) -> String {
        format!("running compiler without writing executable or object file")
    }

    fn help(&self) -> &str {
        "Usage: tahuc --test"
    }

    fn run(&self, args: &mut std::collections::VecDeque<String>, config: &mut tahuc_module::config::Config) -> Result<(), String> {
        args.pop_front();
        config.is_test = true;
        Ok(())
    }
}