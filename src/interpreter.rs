use std::{thread, time::Duration};

use crate::{
    graphics::DisplayHandler,
    keyboard::{KbdHandler, KbdHandlerState},
    rom::Rom,
};

extern crate sdl2;
use sdl2::{render::Canvas, video::Window, Sdl};

extern crate rand;
use rand::Rng;

/// The CHIP-8 intepreter is capable of accessing 4KB of RAM.
const MEMORY_SIZE: usize = 4_096;

/// Standard available fonts: used for programs to draw to the screen a given
/// object.
const FONTS: [u8; 80] = [
    0xF0, 0x90, 0x90, 0x90, 0xF0, // 0
    0x20, 0x60, 0x20, 0x20, 0x70, // 1
    0xF0, 0x10, 0xF0, 0x80, 0xF0, // 2
    0xF0, 0x10, 0xF0, 0x10, 0xF0, // 3
    0x90, 0x90, 0xF0, 0x10, 0x10, // 4
    0xF0, 0x80, 0xF0, 0x10, 0xF0, // 5
    0xF0, 0x80, 0xF0, 0x90, 0xF0, // 6
    0xF0, 0x10, 0x20, 0x40, 0x40, // 7
    0xF0, 0x90, 0xF0, 0x90, 0xF0, // 8
    0xF0, 0x90, 0xF0, 0x10, 0xF0, // 9
    0xF0, 0x90, 0xF0, 0x90, 0x90, // A
    0xE0, 0x90, 0xE0, 0x90, 0xE0, // B
    0xF0, 0x80, 0x80, 0x80, 0xF0, // C
    0xE0, 0x90, 0x90, 0x90, 0xE0, // D
    0xF0, 0x80, 0xF0, 0x80, 0xF0, // E
    0xF0, 0x80, 0xF0, 0x80, 0x80, // F
];

/// Number of fonts used per character.
const FONTS_PER_CHAR: usize = 5;

/// Sprite width measured in bites.
const SPRITE_WIDTH_BITES: usize = 8;

/// Original CHIP-8 screen width available for display.
pub const DISPLAY_WIDTH: usize = 64;

/// Original CHIP-8 screen height available for display.
pub const DISPLAY_HEIGHT: usize = 32;

/// Types of possible actions with the program counter after opcode execution.
enum PCUpdate {
    /// Update the program counter to the next opcode in memory, that is,
    /// increment it by 2.
    Next,

    /// The update should move the program counter so that it skips the next
    /// opcode in memory. For that, we increment the program counter by 4.
    SkipNext,

    /// Given an adress (specified by the opcode), assign the program counter
    /// to this adress.
    JumpTo(usize),
}

pub struct Interpreter {
    /// CHIP-8 has a total of 4KB of RAM. The interpreter memory is located
    /// below `0x200` while the ROM memory is anything from `0x200`.
    memory: [u8; MEMORY_SIZE],

    /// The `program_counter` (also referred to as "PC") points at the current
    /// instruction in memory.
    ///
    /// Most CHIP-8 programs start at memory adress `0x200` (`512` in
    /// decimals). Some programs however start at `0x600` (`1536`). The memory
    /// located from `0` to `511` is destined for the interpreter itself.
    program_counter: usize,

    /// The `index_register` (also referred to as "I") is used to point at
    /// location in memory.
    index_register: usize,

    /// The `vregister` array accounts for the values of the Vx registers,
    /// where x is a hexadecimal ranging from 0 to F. The VF register should
    /// not be touched by any program, its purpose is to serve as a flag for
    /// the interpreter.
    vregister: [u8; 16],

    /// The `stack` is used to call subroutines and return from them. CHIP-8
    /// allows for up to 16 levels of nested subroutines.
    stack: [u16; 16],

    /// The `stack_pointer` (also referred to as "SP") is used to point to the
    /// topmost level of the `stack`.
    stack_pointer: usize,

    /// The delay timer should be decreased by one 60 times per second.
    delay_timer: u8,

    /// The sound_timer should be decreased by one 60 times per second. If its
    /// value is positive, the computer should produce a "beep" sound.
    sound_timer: u8,

    /// The keypad register the state of the currently pressed keys, its
    /// values are
    /// ```
    /// 1 2 3 4
    /// q w e r
    /// a s d f
    /// z x c v
    /// ```
    keypad: [bool; 16],

    /// Display screen containing the fonts
    display: [[u8; DISPLAY_WIDTH]; DISPLAY_HEIGHT],

    /// Interpreter handler for keyboard input.
    kbd_handler: KbdHandler,

    /// Interpreter display handler.
    display_handler: DisplayHandler,
}

// -----------------------------------------------------------------------------
//   - Initialization setup methods  -
// -----------------------------------------------------------------------------
impl Interpreter {
    /// Creates a new CHIP-8 intepreter
    pub fn new(contex: &Sdl, canvas: Canvas<Window>) -> Self {
        // Load font set to memory
        let mut memory = [0u8; MEMORY_SIZE];
        memory[..80].copy_from_slice(&FONTS[..80]);

        Interpreter {
            memory,
            program_counter: 0x200,
            index_register: 0,
            stack_pointer: 0,
            stack: [0; 16],
            vregister: [0; 16],
            delay_timer: 0,
            sound_timer: 0,
            keypad: [false; 16],
            display: [[0u8; DISPLAY_WIDTH]; DISPLAY_HEIGHT],
            kbd_handler: KbdHandler::new(contex),
            display_handler: DisplayHandler::new(canvas),
        }
    }

    /// Writes ROM data to the memory starting at `0x200` address.
    pub fn load_rom(&mut self, rom: Rom) {
        // Check if the program counter is set to the initial address,
        // otherwise the memory can be already in used by a different
        // ROM.
        if self.program_counter != 0x200 {
            self.clear_rom_memory();
        }
        self.memory[0x200..].copy_from_slice(&rom.get_data());
    }
}

// -----------------------------------------------------------------------------
//   - Intepreter execution -
// -----------------------------------------------------------------------------
impl Interpreter {
    // Main loop of execution
    pub fn run(&mut self) {
        'running: loop {
            // Setup the display.
            self.display_handler.draw_to_canvas(&self.display);
            self.display_handler.show();

            'waiting_kbd_input: loop {
                eprintln!("Waiting for key");
                match self.kbd_handler.poll_keypad() {
                    Ok(k) => self.keypad = k,
                    Err(()) => break 'running,
                }

                match self.kbd_handler.state() {
                    KbdHandlerState::Waiting(x) => {
                        // Check if some key got pressed and, if so, store its
                        // value into the regiter Vx.
                        if let Some(key_idx) = self.keypad.iter().position(|&b| b) {
                            self.vregister[x] = key_idx as u8;
                            break 'waiting_kbd_input;
                        }
                    }
                    KbdHandlerState::Free => break 'waiting_kbd_input,
                }
            }
            eprintln!("executing");
            self.execute(self.current_opcode());

            // Handling timers.
            if self.delay_timer > 0 {
                self.delay_timer -= 1;
            }
            if self.sound_timer > 0 {
                // TODO: beep
                self.sound_timer -= 1;
            }

            thread::sleep(Duration::new(0, 1_000_000_000u32 / 60));
        }
        eprintln!("Stopping execution");
    }

    /// Given an `upcode`, the interpreter executes a single cycle of the
    /// instruction.
    fn execute(&mut self, opcode: u16) {
        // `nnn`: opcode address
        let nnn = (opcode & 0x0fff) as usize;

        // `kk`: lowest 8-bits of opcode
        let kk = (opcode & 0x00ff) as u8;

        // Collection of nibbles (4-bites) from opcode, from highest to lowest
        let opcode_nibbles = [
            ((opcode & 0xf000) >> 12) as u8,
            ((opcode & 0x0f00) >> 8) as u8,
            ((opcode & 0x00f0) >> 4) as u8,
            (opcode & 0x000f) as u8,
        ];

        // `x`: high 4-bits of the instruction
        let x = opcode_nibbles[1] as usize;

        // `y`: upper 4-bits of the low byte of the instruction
        let y = opcode_nibbles[2] as usize;

        // `n` nibble: lowest 4-bits of the instruction
        let n = opcode_nibbles[3] as usize;

        match opcode_nibbles {
            [0x00, 0x00, 0x0e, 0x00] => self.op_00e0(),
            [0x00, 0x00, 0x0e, 0x0e] => self.op_00ee(),
            [0x01, _, _, _] => self.op_1nnn(nnn),
            [0x02, _, _, _] => self.op_2nnn(nnn),
            [0x03, _, _, _] => self.op_3xkk(x, kk),
            [0x04, _, _, _] => self.op_4xkk(x, kk),
            [0x05, _, _, 0x00] => self.op_5xy0(x, y),
            [0x06, _, _, _] => self.op_6xkk(x, kk),
            [0x07, _, _, _] => self.op_7xkk(x, kk),
            [0x08, _, _, 0x00] => self.op_8xy0(x, y),
            [0x08, _, _, 0x01] => self.op_8xy1(x, y),
            [0x08, _, _, 0x02] => self.op_8xy2(x, y),
            [0x08, _, _, 0x03] => self.op_8xy3(x, y),
            [0x08, _, _, 0x04] => self.op_8xy4(x, y),
            [0x08, _, _, 0x05] => self.op_8xy5(x, y),
            [0x08, _, _, 0x06] => self.op_8xy6(x, y),
            [0x08, _, _, 0x07] => self.op_8xy7(x, y),
            [0x08, _, _, 0x0e] => self.op_8xye(x, y),
            [0x09, _, _, 0x00] => self.op_9xy0(x, y),
            [0x0a, _, _, _] => self.op_annn(nnn),
            [0x0b, _, _, _] => self.op_bnnn(nnn),
            [0x0c, _, _, _] => self.op_cxkk(x, kk),
            [0x0d, _, _, _] => self.op_dxyn(x, y, n),
            [0x0e, _, 0x09, 0x0e] => self.op_ex9e(x),
            [0x0e, _, 0x0a, 0x01] => self.op_exa1(x),
            [0x0f, _, 0x00, 0x07] => self.op_fx07(x),
            [0x0f, _, 0x00, 0x0a] => self.op_fx0a(x),
            [0x0f, _, 0x01, 0x05] => self.op_fx15(x),
            [0x0f, _, 0x01, 0x08] => self.op_fx18(x),
            [0x0f, _, 0x01, 0x0e] => self.op_fx1e(x),
            [0x0f, _, 0x02, 0x09] => self.op_fx29(x),
            [0x0f, _, 0x03, 0x03] => self.op_fx33(x),
            [0x0f, _, 0x05, 0x05] => self.op_fx55(x),
            [0x0f, _, 0x06, 0x05] => self.op_fx65(x),
            _ => self.update_pc(PCUpdate::Next),
        }
    }
}

// -----------------------------------------------------------------------------
//   - Collection of auxiliary methods -
// -----------------------------------------------------------------------------
impl Interpreter {
    /// Clear the interpreter memory starting at `0x200`.
    fn clear_rom_memory(&mut self) {
        self.memory[0x200..].copy_from_slice(&[0u8; MEMORY_SIZE - 0x200]);
    }

    /// Returns the current instruction the `program_counter` is pointing
    /// at. This instruction consists of a single `u16` which is obtained by
    /// merging the `u8` located at the pointing address and the `u8` next to
    /// it.
    fn current_opcode(&self) -> u16 {
        ((self.memory[self.program_counter] as u16) << 8)
            | (self.memory[self.program_counter + 1] as u16)
    }

    /// Updates the program counter according to the given enum type
    /// `PCUpdate`.
    fn update_pc(&mut self, updater: PCUpdate) {
        match updater {
            PCUpdate::Next => self.program_counter += 2,
            PCUpdate::SkipNext => self.program_counter += 4,
            PCUpdate::JumpTo(addr) => self.program_counter = addr,
        }
    }

    /// If the `condition` passes, sets the flag `vregister[0xf]` to `1`,
    /// otherwise the flag is set to `0`.
    fn up_flag_if(&mut self, condition: bool) {
        if condition {
            self.vregister[0xf] = 1;
        } else {
            self.vregister[0xf] = 0;
        }
    }

    /// Erases all pixels from the display screen, set all to zero.
    fn clear_display(&mut self) {
        self.display = [[0u8; DISPLAY_WIDTH]; DISPLAY_HEIGHT];
    }
}

// -----------------------------------------------------------------------------
//   - Implementation of all opcodes -
// -----------------------------------------------------------------------------
impl Interpreter {
    /// CLS: clear the display.
    fn op_00e0(&mut self) {
        self.clear_display();
    }

    /// RET: return from a subroutine.
    ///
    /// The interpreter sets the program counter to the address at the top of
    /// the stack, then subtracts 1 from the stack pointer.
    fn op_00ee(&mut self) {
        // FIXME: thread 'main' panicked at 'index out of bounds: the len is 16 but the
        // index is 16', src/interpreter.rs:311:41
        self.update_pc(PCUpdate::JumpTo(self.stack[self.stack_pointer] as usize));
        self.stack_pointer -= 1;
    }

    /// JP addr: jump to location `nnn`.
    fn op_1nnn(&mut self, nnn: usize) {
        self.update_pc(PCUpdate::JumpTo(nnn));
    }

    /// CALL addr: call subroutine at `nnn`.
    ///
    /// The interpreter increments the stack pointer, then puts the current PC
    /// on the top of the stack. The PC is then set to nnn.
    fn op_2nnn(&mut self, nnn: usize) {
        self.stack[self.stack_pointer] = self.program_counter as u16;
        self.stack_pointer += 1;
        self.update_pc(PCUpdate::JumpTo(nnn));
    }

    /// SE Vx, byte: skip next instruction if `Vx == kk`.
    fn op_3xkk(&mut self, x: usize, kk: u8) {
        if self.vregister[x] == kk {
            self.update_pc(PCUpdate::SkipNext);
        } else {
            self.update_pc(PCUpdate::Next);
        }
    }

    /// SNE Vx, byte: skip next instruction if `Vx != kk`.
    fn op_4xkk(&mut self, x: usize, kk: u8) {
        if self.vregister[x] != kk {
            self.update_pc(PCUpdate::SkipNext);
        } else {
            self.update_pc(PCUpdate::Next);
        }
    }

    /// SE Vx, Vy: skip next instruction if `Vx == Vy`.
    fn op_5xy0(&mut self, x: usize, y: usize) {
        if self.vregister[x] == self.vregister[y] {
            self.update_pc(PCUpdate::SkipNext);
        } else {
            self.update_pc(PCUpdate::Next);
        }
    }

    /// LD Vx, byte: set `Vx = kk`.
    fn op_6xkk(&mut self, x: usize, kk: u8) {
        self.vregister[x] = kk;
        self.update_pc(PCUpdate::Next);
    }

    /// ADD Vx, byte: set `Vx = Vx + kk`.
    fn op_7xkk(&mut self, x: usize, kk: u8) {
        self.vregister[x] = self.vregister[x].wrapping_add(kk);
        self.update_pc(PCUpdate::Next);
    }

    /// LD Vx, Vy: set `Vx = Vy`.
    fn op_8xy0(&mut self, x: usize, y: usize) {
        self.vregister[x] = self.vregister[y];
        self.update_pc(PCUpdate::Next);
    }

    /// OR Vx, Vy: set `Vx = Vx OR Vy` (bitwise OR).
    fn op_8xy1(&mut self, x: usize, y: usize) {
        self.vregister[x] |= self.vregister[y];
        self.update_pc(PCUpdate::Next);
    }

    /// AND Vx, Vy: set `Vx = Vx AND Vy` (bitwise AND).
    fn op_8xy2(&mut self, x: usize, y: usize) {
        self.vregister[x] &= self.vregister[y];
        self.update_pc(PCUpdate::Next);
    }

    /// XOR Vx, Vy: set `Vx = Vx XOR Vy` (bitwise XOR).
    fn op_8xy3(&mut self, x: usize, y: usize) {
        self.vregister[x] ^= self.vregister[y];
        self.update_pc(PCUpdate::Next);
    }

    /// ADD Vx, Vy: set `Vx = Vx + Vy`.
    ///
    /// The values of Vx and Vy are added together. If the result is greater
    /// than 8 bits (i.e., > 255,) VF is set to 1, otherwise 0. Only the lowest
    /// 8 bits of the result are kept, and stored in Vx.
    fn op_8xy4(&mut self, x: usize, y: usize) {
        let sum = self.vregister[x] as u16 + self.vregister[y] as u16;
        self.up_flag_if(sum > 0xff);
        self.vregister[x] = sum as u8;
        self.update_pc(PCUpdate::Next);
    }

    /// SUB Vx, Vy:
    /// Set Vx = Vx - Vy, set VF = NOT borrow.
    ///
    /// If Vx > Vy, then VF is set to 1, otherwise 0. Then Vy is subtracted
    /// from Vx, and the results stored in Vx.
    fn op_8xy5(&mut self, x: usize, y: usize) {
        self.up_flag_if(self.vregister[x] > self.vregister[y]);
        self.vregister[x] = self.vregister[x].wrapping_sub(self.vregister[y]);
        self.update_pc(PCUpdate::Next);
    }

    /// SHR Vx {, Vy}:
    ///
    /// If the least-significant bit of Vx is 1, then VF is set to 1, otherwise
    /// 0. Then Vx is divided by 2.
    fn op_8xy6(&mut self, x: usize, _y: usize) {
        self.up_flag_if((self.vregister[x] & 1) == 1);
        self.vregister[x] >>= 1;
        self.update_pc(PCUpdate::Next);
    }

    /// SUBN Vx, Vy:
    /// Set Vx = Vy - Vx, set VF = NOT borrow.
    ///
    /// If Vy > Vx, then VF is set to 1, otherwise 0. Then Vx is subtracted
    /// from Vy, and the results stored in Vx.
    fn op_8xy7(&mut self, x: usize, y: usize) {
        self.up_flag_if(self.vregister[y] > self.vregister[x]);
        self.vregister[x] = self.vregister[y].wrapping_sub(self.vregister[x]);
        self.update_pc(PCUpdate::Next);
    }

    /// SHL Vx {, Vy}:
    ///
    /// If the most-significant bit of Vx is 1, then VF is set to 1, otherwise
    /// to 0. Then Vx is multiplied by 2.
    fn op_8xye(&mut self, x: usize, _y: usize) {
        self.up_flag_if((self.vregister[x] >> 7) & 1 == 1);
        self.vregister[x] <<= 1;
        self.update_pc(PCUpdate::Next);
    }

    /// SNE Vx, Vy: skip next instruction if Vx != Vy.
    fn op_9xy0(&mut self, x: usize, y: usize) {
        if self.vregister[x] != self.vregister[y] {
            self.update_pc(PCUpdate::SkipNext);
        } else {
            self.update_pc(PCUpdate::Next);
        }
    }

    /// LD I, addr: set I = nnn (I is the index register).
    fn op_annn(&mut self, nnn: usize) {
        self.index_register = nnn;
        self.update_pc(PCUpdate::Next);
    }

    /// JP V0, addr: jump to location nnn + V0.
    ///
    /// The program counter is set to nnn plus the value of V0.
    fn op_bnnn(&mut self, nnn: usize) {
        self.update_pc(PCUpdate::JumpTo(nnn + self.vregister[0] as usize));
    }

    /// RND Vx, byte: set Vx = random byte AND kk
    ///
    /// The interpreter generates a random number from 0 to 255, which is then
    /// ANDed with the value kk. The results are stored in Vx.
    fn op_cxkk(&mut self, x: usize, kk: u8) {
        self.vregister[x] = rand::thread_rng().gen::<u8>() & kk;
        self.update_pc(PCUpdate::Next);
    }

    /// DRW Vx, Vy, nibble:
    /// Display n-byte sprite starting at memory location I at (Vx, Vy), set
    /// VF = collision.
    ///
    /// The interpreter reads n bytes from memory, starting at the address
    /// stored in I. These bytes are then displayed as sprites on screen at
    /// coordinates (Vx, Vy). Sprites are XORed onto the existing screen. If
    /// this causes any pixels to be erased, VF is set to 1, otherwise it is
    /// set to 0. If the sprite is positioned so part of it is outside the
    /// coordinates of the display, it wraps around to the opposite side of the
    /// screen.
    fn op_dxyn(&mut self, x: usize, y: usize, n: usize) {
        for byte_idx in 0..n {
            // Sprite byte to be inserted.
            let sprite_byte = self.memory[self.index_register + byte_idx];

            // Column the sprite byte is to be put, always wrapping the screen.
            let col = (self.vregister[y] as usize + byte_idx) % DISPLAY_HEIGHT;

            for bit_idx in 0..SPRITE_WIDTH_BITES {
                // Row the sprite bit is to be put, always wrapping the screen.
                let row = (self.vregister[x] as usize + bit_idx) % DISPLAY_WIDTH;

                // Currently displayed bit in the screen at the location.
                let current_bit = self.display[col][row];

                // Sprite bit to be inserted.
                let sprite_bit = (sprite_byte >> bit_idx) & 0x1;

                // Check for collision.
                self.up_flag_if(sprite_bit == 1 && current_bit == 1);

                // Puts the sprite into the screen.
                self.display[col][row] ^= sprite_bit;
            }
        }
        self.update_pc(PCUpdate::Next);
    }

    /// SKP Vx:
    /// Skip next instruction if key with the value of Vx is pressed.
    ///
    /// Checks the keyboard, and if the key corresponding to the value of Vx is
    /// currently in the down position, PC is increased by 2.
    fn op_ex9e(&mut self, x: usize) {
        if self.keypad[self.vregister[x] as usize] {
            self.update_pc(PCUpdate::SkipNext);
        } else {
            self.update_pc(PCUpdate::Next);
        }
    }

    /// SKNP Vx:
    /// Skip next instruction if key with the value of Vx is not pressed.
    ///
    /// Checks the keyboard, and if the key corresponding to the value of Vx is
    /// currently in the up position, PC is increased by 2.
    fn op_exa1(&mut self, x: usize) {
        if !self.keypad[self.vregister[x] as usize] {
            self.update_pc(PCUpdate::SkipNext);
        } else {
            self.update_pc(PCUpdate::Next);
        }
    }

    /// LD Vx, DT: set Vx = delay timer value.
    fn op_fx07(&mut self, x: usize) {
        self.vregister[x] = self.delay_timer;
        self.update_pc(PCUpdate::Next);
    }

    /// LD Vx, K:
    /// Wait for a key press, store the value of the key in Vx.
    ///
    /// All execution stops until a key is pressed, then the value of that key
    /// is stored in Vx.
    fn op_fx0a(&mut self, x: usize) {
        eprintln!("HERE WE ARE BITCHES");
        match self.keypad.iter().position(|&b| b) {
            Some(key_idx) => self.vregister[x] = key_idx as u8,
            None => self.kbd_handler.wait_for_key(x),
        }
        self.update_pc(PCUpdate::Next);
    }

    /// LD DT, Vx: set delay timer = Vx.
    fn op_fx15(&mut self, x: usize) {
        self.delay_timer = self.vregister[x];
        self.update_pc(PCUpdate::Next);
    }

    /// LD ST, Vx: set sound timer = Vx.
    fn op_fx18(&mut self, x: usize) {
        self.sound_timer = self.vregister[x];
        self.update_pc(PCUpdate::Next);
    }

    /// ADD I, Vx: set I = I + Vx.
    ///
    /// If the sum I + Vx exceeds `0xfff` then the flag is set to `1`,
    /// otherwise `0`.
    fn op_fx1e(&mut self, x: usize) {
        let sum = self.index_register + self.vregister[x] as usize;
        self.up_flag_if(sum > 0xfff);
        self.index_register = sum;
        self.update_pc(PCUpdate::Next);
    }

    /// LD F, Vx: set I = location of sprite for digit Vx.
    ///
    /// The value of I is set to the location for the hexadecimal sprite
    /// corresponding to the value of Vx.
    fn op_fx29(&mut self, x: usize) {
        self.index_register = (self.vregister[x] as usize) * FONTS_PER_CHAR;
        self.update_pc(PCUpdate::Next);
    }

    /// LD B, Vx:
    /// Store BCD representation of Vx in memory locations I, I+1, and I+2.
    ///
    /// The interpreter takes the decimal value of Vx, and places the hundreds
    /// digit in memory at location in I, the tens digit at location I+1, and
    /// the ones digit at location I+2.
    fn op_fx33(&mut self, x: usize) {
        self.memory[self.index_register] = self.vregister[x] / 100;
        self.memory[self.index_register + 1] = (self.vregister[x] % 100) / 10;
        self.memory[self.index_register + 1] = self.vregister[x] % 10;
        self.update_pc(PCUpdate::Next);
    }

    /// LD I, Vx:
    /// Store registers V0 through Vx in memory starting at location I.
    ///
    /// The interpreter copies the values of registers V0 through Vx into
    /// memory, starting at the address in I.
    fn op_fx55(&mut self, x: usize) {
        self.memory[self.index_register..(self.index_register + x)]
            .copy_from_slice(&self.vregister[..x]);
        self.update_pc(PCUpdate::Next);
    }

    /// LD Vx, I:
    /// Read registers V0 through Vx from memory starting at location I.
    ///
    /// The interpreter reads values from memory starting at location I into
    /// registers V0 through Vx.
    fn op_fx65(&mut self, x: usize) {
        self.vregister[..x]
            .copy_from_slice(&self.memory[self.index_register..(self.index_register + x)]);
        self.update_pc(PCUpdate::Next);
    }
}

// NOTE: This is temporary code and should be only used for purposes of
// debugging.
impl Interpreter {
    pub fn manually_press_key(&mut self, key: u8) {
        self.keypad[key as usize] = true;
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{graphics::SCALE_FACTOR, rom::Rom};
    extern crate sdl2;

    fn simulate_initiation_of_environment(r: Rom) -> Interpreter {
        let sdl_context = sdl2::init().unwrap();
        let video_subsystem = sdl_context.video().unwrap();
        let window = video_subsystem
            .window(
                "CHIP-8",
                SCALE_FACTOR * DISPLAY_WIDTH as u32,
                SCALE_FACTOR * DISPLAY_HEIGHT as u32,
            )
            .position_centered()
            .build()
            .unwrap();
        let canvas = window.into_canvas().build().unwrap();
        let rom = r;
        let mut chip8 = Interpreter::new(&sdl_context, canvas);
        chip8.load_rom(rom);
        chip8
    }

    #[test]
    fn op_fx0a_wait_for_a_keyboard_input() {
        let test_rom = Rom::from_slice(&[0u8; 3584]);
        let mut chip8 = simulate_initiation_of_environment(test_rom);
        chip8.execute(0xf40a);
        assert_eq!(chip8.kbd_handler.is_waiting(), Some(4));
        assert_eq!(chip8.keypad, [false; 16]);
        chip8.manually_press_key(6u8);
    }
}
