use crate::interpreter::Interpreter;
use crate::rom::Rom;
use std::{env, thread, time::Duration};

mod display;
mod interpreter;
mod rom;

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = &args[1];

    let rom = Rom::new(file_path);
    let chip8 = Interpreter::new().load_rom(rom);

    loop {
        // Fetch stage:
        // Read the instruction PC is pointing. The instruction consists of 2
        // bytes. We then store the instruction as a single u16 and increment
        // the PC by two in order for it to read the next instruction.

        // Decode stage:
        // The first nibble of the instruction decides which instruction should
        // be processed

        // Sleep in order to match the timing of CHIP-8 games
        thread::sleep(Duration::new(0, 1_000_000_000u32 / 60));
    }
}
