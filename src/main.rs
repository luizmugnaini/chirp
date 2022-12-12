mod graphics;
mod interpreter;
mod keyboard;
mod rom;

use crate::{
    graphics::SCALE_FACTOR,
    interpreter::{Interpreter, DISPLAY_HEIGHT, DISPLAY_WIDTH},
    rom::Rom,
};
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = &args[1];

    // NOTE: should this not be handled here? Maybe put this as part of the creation
    // of a `DisplayHandler`.

    // Initialize SDL system
    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();
    let window = video_subsystem
        .window(
            format!("CHIP-8 interpreter: {file_path}").as_str(),
            SCALE_FACTOR * DISPLAY_WIDTH as u32,
            SCALE_FACTOR * DISPLAY_HEIGHT as u32,
        )
        .position_centered()
        .build()
        .unwrap();
    let canvas = window.into_canvas().build().unwrap();

    // Load ROM into the interpreter
    let rom = Rom::new(file_path);
    let mut chip8 = Interpreter::new(&sdl_context, canvas);
    chip8.load_rom(rom);
    chip8.run();
}
