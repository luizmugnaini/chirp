mod graphics;
mod interpreter;
mod keyboard;
mod rom;

use crate::interpreter::Interpreter;
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = &args[1];
    let sdl_context = sdl2::init().unwrap();
    let mut chip8 = Interpreter::new(sdl_context, file_path);
    chip8.run();
}
