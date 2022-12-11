use std::env;
use std::time::Duration;

mod graphics;
mod interpreter;
mod keyboard;
mod rom;

use crate::graphics::SCALE_FACTOR;
use crate::interpreter::{Interpreter, DISPLAY_HEIGHT, DISPLAY_WIDTH};
use crate::rom::Rom;

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = &args[1];

    // Initialize SDL system
    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();
    let window = video_subsystem
        .window(
            "CHIP-8",
            (SCALE_FACTOR * DISPLAY_WIDTH).try_into().unwrap(),
            (SCALE_FACTOR * DISPLAY_HEIGHT).try_into().unwrap(),
        )
        .position_centered()
        .build()
        .unwrap();
    let canvas = window.into_canvas().build().unwrap();
    // std::thread::sleep(Duration::from_secs(10));

    // Load ROM into the interpreter
    let rom = Rom::new(file_path);
    let mut chip8 = Interpreter::new(&sdl_context, canvas);
    chip8.load_rom(rom);
    chip8.run();
}
