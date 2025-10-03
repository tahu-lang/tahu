mod help;
mod include;
mod linking;
mod opt;
mod output_name;
mod version;
mod test;

use std::{collections::{HashSet, VecDeque}, fs, path::PathBuf, process::exit};

use help::HelpCommand;
use tahuc_module::config::{Config, FileInput};
use version::VersionCommand;

use crate::cmd::{output_name::OutputName, test::Test};

pub trait Command<'a> {
    fn name(&self) -> &str;
    fn commands(&self) -> Vec<&str>;
    fn use_prefix(&self) -> bool {
        false
    }
    fn description(&self) -> String;
    fn help(&self) -> &str;
    fn run(&self, args: &mut VecDeque<String>, config: &mut Config) -> Result<(), String>;
}

pub struct Cli<'a> {
    commands: Vec<Box<dyn Command<'a>>>,
    seen_flag: HashSet<String>,
}

impl<'a> Cli<'a> {
    pub fn new() -> Self {
        Self {
            commands: Vec::new(),
            seen_flag: HashSet::new(),
        }
    }

    pub fn add_command(&mut self, command: Box<dyn Command<'a>>) {
        self.commands.push(command);
    }

    fn is_help_command(&self, command: &String) -> bool {
        command == "-H" || command == "--help"
    }

    pub fn run(&mut self, args: VecDeque<String>, config: &mut Config) -> Result<(), String> {
        if args.is_empty() {
            self.print_help();
            return Ok(());
        }

        // trap if help is first
        if let Some(command) = args.get(1) {
            if self.is_help_command(command) {
                self.print_help();
                return Ok(());
            }
        }

        for arg in &args {
            if arg.starts_with("-") {
                if self.seen_flag.contains(arg) {
                    println!("Duplicate flag: {}", arg);
                    std::process::exit(1);
                }

                for command in &self.commands {
                    if command.commands().contains(&arg.as_str()) {
                        for seen in command.commands() {
                            self.seen_flag.insert(seen.to_string());
                        }
                        break;
                    }
                }
            }
        }

        match self.dispatch(&args, config) {
            Ok(_) => return Ok(()),
            Err(err) => {
                self.print_help();
                return Err(err);
            }
        }
    }

    fn resolve_file(&self, file: &str) -> (PathBuf, PathBuf) {
        let abs = fs::canonicalize(file).expect("file not found");
        let dir = abs.parent().unwrap().to_path_buf();
        (abs, dir)
    }

    fn dispatch(&self, args: &VecDeque<String>, config: &mut Config) -> Result<(), String> {
        let mut local_args = args.clone();

        for command in local_args.clone() {
            if command.ends_with(".tahu") {
                let (abs, dir) = self.resolve_file(&command);

                config.current_dir = dir.clone();
                config.add_file(FileInput {
                    name: command.clone(),
                    dir,
                    path: abs,
                });
                local_args.pop_front();
                continue;
            }

            for c in &self.commands {
                if c.use_prefix() && command.starts_with(c.name()) {
                    if command.len() == c.name().len() {
                        println!("Missing argument for command: {}", c.name());
                        println!("{}", c.help());
                        std::process::exit(1);
                    }
                    if let Err(err) = self.run_command(c, &mut local_args, config) {
                        return Err(err);
                    }
                }

                if c.commands().contains(&command.as_str()) {
                    if let Err(err) = self.run_command(c, &mut local_args, config) {
                        return Err(err);
                    }
                }
            }
        }
        Ok(())
    }

    fn run_command(
        &self,
        c: &Box<dyn Command<'a>>,
        args: &mut VecDeque<String>,
        config: &mut Config,
    ) -> Result<(), String> {
        if let Some(command) = args.get(1) {
            if self.is_help_command(command) {
                println!("{}", c.help());
                return Ok(());
            }
        }

        return c.run(args, config);
    }

    pub fn print_help(&self) {
        println!("Usage: tahuc <command> [options]");
        println!("Available options:");

        let max_name_len = self
            .commands
            .iter()
            .map(|c| c.name().len())
            .max()
            .unwrap_or(0);

        for command in &self.commands {
            let description = command.description();
            let lines: Vec<&str> = description.lines().collect();

            if let Some((first, rest)) = lines.split_first() {
                println!(
                    "  {:<width$} - {}",
                    command.name(),
                    first,
                    width = max_name_len
                );

                for line in rest {
                    println!("  {:width$}   {}", "", line, width = max_name_len);
                }
            }
        }

        exit(0);
    }
}

pub fn parse_command() -> Config {
    // get location compiler
    let compiler_path = std::env::current_exe()
        .expect("Failed to get current executable path");
    let compiler_dir = compiler_path.parent()
        .expect("Failed to get compiler directory")
        .to_path_buf();

    let mut config = Config::new();
    config.compiler_dir = compiler_dir;
    
    let mut args = std::env::args().collect::<VecDeque<String>>();
    let mut cli = Cli::new();

    cli.add_command(Box::new(opt::Opt));
    cli.add_command(Box::new(OutputName));
    cli.add_command(Box::new(include::Include));
    cli.add_command(Box::new(linking::Linking));
    cli.add_command(Box::new(linking::LinkDir));
    cli.add_command(Box::new(Test));
    cli.add_command(Box::new(VersionCommand));
    cli.add_command(Box::new(HelpCommand));

    args.pop_front();
    if let Err(err) = cli.run(args, &mut config) {
        eprintln!("Error: {}", err);
        std::process::exit(1);
    }

    config
}
