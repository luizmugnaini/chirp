use crate::rom::Rom;

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

// const KEYPAD: [&str; 16] = [
//     "1", "2", "3", "4", // 1
//     "q", "w", "e", "r", // 2
//     "a", "s", "d", "f", // 3
//     "z", "x", "c", "v", // 4
// ];

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
    program_counter: u16,

    /// The `index_register` (also referred to as "I") is used to point at
    /// location in memory.
    index_register: u16,

    /// The `vregister` array accounts for the values of the Vx registers,
    /// where x is a hexadecimal ranging from 0 to F. The VF register should
    /// not be touched by any program, its purpose is to serve as a flag for
    /// the interpreter.
    vregister: [u16; 16],

    /// The `stack` is used to call subroutines and return from them. CHIP-8
    /// allows for up to 16 levels of nested subroutines.
    stack: [u16; 16],

    /// The `stack_pointer` (also referred to as "SP") is used to point to the
    /// topmost level of the `stack`.
    stack_pointer: u8,

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
}

impl Interpreter {
    pub fn new() -> Self {
        let mut memory = [0u8; MEMORY_SIZE];
        // Load font set to memory
        for idx in 0..80 {
            memory[idx] = FONTS[idx];
        }

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
        }
    }

    /// Writes ROM data to the memory starting at `0x200` address.
    pub fn load_rom(&mut self, rom: Rom) {
        for (idx, &x) in rom.get_data().iter().enumerate() {
            let mem_pos = 0x200 + idx;
            if mem_pos < MEMORY_SIZE {
                self.memory[mem_pos] = x;
            }
        }
    }

    /// CLS: clear the display.
    fn op_00e0() {
        todo!();
    }

    /// RET: return from a subroutine.
    fn op_00ee() {
        todo!();
    }

    /// JP addr: jump to location `nnn`.
    fn op_1nnn() {
        todo!();
    }

    /// CALL addr: call subroutine at `nnn`.
    fn op_2nnn() {
        todo!();
    }

    /// SE Vx, byte: skip next instruction if `Vx == kk`.
    fn op_3xkk() {
        todo!();
    }

    /// SNE Vx, byte: skip next instruction if `Vx != kk`.
    fn op_4xkk() {
        todo!();
    }

    /// SE Vx, Vy: skip next instruction if `Vx == Vy`.
    fn op_5xy0() {
        todo!();
    }

    /// LD Vx, byte: set `Vx = kk`.
    fn op_6kxx() {
        todo!();
    }

    /// ADD Vx, byte: set `Vx = Vx + kk`.
    fn op_7kxx() {
        todo!();
    }

    /// LD Vx, Vy: set `Vx = Vy`.
    fn op_8xy0() {
        todo!();
    }

    /// OR Vx, Vy: set `Vx = Vx OR Vy` (bitwise OR).
    fn op_8xy1() {
        todo!();
    }

    /// AND Vx, Vy: set `Vx = Vx AND Vy` (bitwise AND).
    fn op_8xy2() {
        todo!();
    }

    /// XOR Vx, Vy: set `Vx = Vx XOR Vy` (bitwise XOR).
    fn op_8xy3() {
        todo!();
    }

    /// ADD Vx, Vy: set `Vx = Vx + Vy`.
    ///
    /// The values of Vx and Vy are added together. If the result is greater
    /// than 8 bits (i.e., > 255,) VF is set to 1, otherwise 0. Only the lowest
    /// 8 bits of the result are kept, and stored in Vx.
    fn op_8xy4() {
        todo!();
    }

    /// SUB Vx, Vy:
    /// Set Vx = Vx - Vy, set VF = NOT borrow.
    ///
    /// If Vx > Vy, then VF is set to 1, otherwise 0. Then Vy is subtracted
    /// from Vx, and the results stored in Vx.
    fn op_8xy5() {
        todo!();
    }

    /// SHR Vx {, Vy}:
    ///
    /// If the least-significant bit of Vx is 1, then VF is set to 1, otherwise
    /// 0. Then Vx is divided by 2.
    fn op_8xy6() {
        todo!();
    }

    /// SUBN Vx, Vy:
    /// Set Vx = Vy - Vx, set VF = NOT borrow.
    ///
    /// If Vy > Vx, then VF is set to 1, otherwise 0. Then Vx is subtracted
    /// from Vy, and the results stored in Vx.
    fn op_8xy7() {
        todo!();
    }

    /// SHL Vx {, Vy}:
    ///
    /// If the most-significant bit of Vx is 1, then VF is set to 1, otherwise
    /// to 0. Then Vx is multiplied by 2.
    fn op_8xye() {
        todo!();
    }

    /// SNE Vx, Vy: skip next instruction if Vx != Vy.
    fn op_9xy0() {
        todo!();
    }

    /// LD I, addr: set I = nnn (I is the index register).
    fn op_annn() {
        todo!();
    }

    /// JP V0, addr: jump to location nnn + V0.
    ///
    /// The program counter is set to nnn plus the value of V0.
    fn op_bnnn() {
        todo!();
    }

    /// RND Vx, byte: set Vx = random byte AND kk
    ///
    /// The interpreter generates a random number from 0 to 255, which is then
    /// ANDed with the value kk. The results are stored in Vx.
    fn op_cxkk() {
        todo!();
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
    fn op_dxyn() {
        todo!();
    }

    /// SKP Vx:
    /// Skip next instruction if key with the value of Vx is pressed.
    ///
    /// Checks the keyboard, and if the key corresponding to the value of Vx is
    /// currently in the down position, PC is increased by 2.
    fn op_ex9e() {
        todo!();
    }

    /// SKNP Vx:
    /// Skip next instruction if key with the value of Vx is not pressed.
    ///
    /// Checks the keyboard, and if the key corresponding to the value of Vx is
    /// currently in the up position, PC is increased by 2.
    fn op_exa1() {
        todo!();
    }

    /// LD Vx, DT: set Vx = delay timer value.
    fn op_fx07() {
        todo!();
    }

    /// LD Vx, K:
    /// Wait for a key press, store the value of the key in Vx.
    ///
    /// All execution stops until a key is pressed, then the value of that key
    /// is stored in Vx.
    fn op_fx0a() {
        todo!();
    }

    /// LD DT, Vx: set delay timer = Vx.
    fn op_fx15() {
        todo!();
    }

    /// LD ST, Vx: set sound timer = Vx.
    fn op_fx18() {
        todo!();
    }

    /// ADD I, Vx: set I = I + Vx.
    fn op_fx1e() {
        todo!();
    }

    /// LD F, Vx: set I = location of sprite for digit Vx.
    ///
    /// The value of I is set to the location for the hexadecimal sprite
    /// corresponding to the value of Vx.
    fn op_fx29() {
        todo!();
    }

    /// LD B, Vx:
    /// Store BCD representation of Vx in memory locations I, I+1, and I+2.
    ///
    /// The interpreter takes the decimal value of Vx, and places the hundreds
    /// digit in memory at location in I, the tens digit at location I+1, and
    /// the ones digit at location I+2.
    fn op_fx33() {
        todo!();
    }

    /// LD [I], Vx:
    /// Store registers V0 through Vx in memory starting at location I.
    ///
    /// The interpreter copies the values of registers V0 through Vx into
    /// memory, starting at the address in I.
    fn op_fx55() {
        todo!();
    }

    /// LD Vx, [I]:
    /// Read registers V0 through Vx from memory starting at location I.
    ///
    /// The interpreter reads values from memory starting at location I into
    /// registers V0 through Vx.
    fn op_fx65() {
        todo!();
    }
}
