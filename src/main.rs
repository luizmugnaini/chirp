mod graphics;
mod interpreter;
mod keyboard;
mod rom;
mod sound;

use crate::interpreter::Interpreter;
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = &args[1];
    let mut chip8 = Interpreter::new(file_path);
    chip8.run();
}
