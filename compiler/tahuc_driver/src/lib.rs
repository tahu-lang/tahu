use std::{thread::sleep, time::{Duration, Instant}};

fn run_compiler() {
    println!("running compiler");
    sleep(Duration::from_millis(100));
}

pub fn main() {
    let start_time = Instant::now();

    run_compiler();
    println!("Compilation success {:?}", start_time.elapsed());
}