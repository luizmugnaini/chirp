use std::fs::File;
use std::io::Read;

const ROM_MEM_SIZE: usize = 3584;

/// Struct containing the game read-only-memory
pub struct Rom {
    /// Available memory for the game to store its content. The total RAM of
    /// CHIP-8 is 4KB (`4096`) but `0x200` (`512`) bites are reserved to the
    /// interpreter, lefting the game with an available space of `3584`
    mem: [u8; ROM_MEM_SIZE],

    /// Stores the ROM size. This is done in order to prevent the interpreter
    /// from reading unwanted memory that may have been left by previous roms
    /// that wheren't cleaned from memory.
    size: usize,
}

impl Rom {
    /// To create a new `Rom` object a `path` to the ROM file should be
    /// provided.
    pub fn new(path: &str) -> Rom {
        let mut raw_rom = File::open(path).expect("Could not open ROM file");
        let mut mem = [0u8; ROM_MEM_SIZE];

        match raw_rom.read(&mut mem) {
            Err(e) => panic!("Could not read ROM file: {e}"),
            Ok(0) => panic!("ROM file is empty!"),
            Ok(size) => Rom { mem, size },
        }
    }

    /// Returns the stored memory of the ROM.
    pub fn get_data(&self) -> [u8; ROM_MEM_SIZE] {
        self.mem
    }

    /// Returns the size of the stored ROM.
    pub fn size(&self) -> usize {
        self.size
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn create_new_rom() {
        let rom_pong_data: [u16; 123] = [
            0x6a02, 0x6b0c, 0x6c3f, 0x6d0c, 0xa2ea, 0xdab6, 0xdcd6, 0x6e00,
            0x22d4, 0x6603, 0x6802, 0x6060, 0xf015, 0xf007, 0x3000, 0x121a,
            0xc717, 0x7708, 0x69ff, 0xa2f0, 0xd671, 0xa2ea, 0xdab6, 0xdcd6,
            0x6001, 0xe0a1, 0x7bfe, 0x6004, 0xe0a1, 0x7b02, 0x601f, 0x8b02,
            0xdab6, 0x600c, 0xe0a1, 0x7dfe, 0x600d, 0xe0a1, 0x7d02, 0x601f,
            0x8d02, 0xdcd6, 0xa2f0, 0xd671, 0x8684, 0x8794, 0x603f, 0x8602,
            0x611f, 0x8712, 0x4602, 0x1278, 0x463f, 0x1282, 0x471f, 0x69ff,
            0x4700, 0x6901, 0xd671, 0x122a, 0x6802, 0x6301, 0x8070, 0x80b5,
            0x128a, 0x68fe, 0x630a, 0x8070, 0x80d5, 0x3f01, 0x12a2, 0x6102,
            0x8015, 0x3f01, 0x12ba, 0x8015, 0x3f01, 0x12c8, 0x8015, 0x3f01,
            0x12c2, 0x6020, 0xf018, 0x22d4, 0x8e34, 0x22d4, 0x663e, 0x3301,
            0x6603, 0x68fe, 0x3301, 0x6802, 0x1216, 0x79ff, 0x49fe, 0x69ff,
            0x12c8, 0x7901, 0x4902, 0x6901, 0x6004, 0xf018, 0x7601, 0x4640,
            0x76fe, 0x126c, 0xa2f2, 0xfe33, 0xf265, 0xf129, 0x6414, 0x6500,
            0xd455, 0x7415, 0xf229, 0xd455, 0x00ee, 0x8080, 0x8080, 0x8080,
            0x8000, 0x0000, 0x0000,
        ];
        let rom = Rom::new("games/PONG");
        let rom_mem = rom.get_data();

        assert_eq!(rom.size(), 246);
        for addr in 0..123 {
            let x = rom_pong_data[addr];
            // Check upper byte
            assert_eq!(rom_mem[2 * addr], ((x >> 8) & 0xff) as u8);

            // Check lower byte
            assert_eq!(rom_mem[2 * addr + 1], (x & 0xff) as u8);
        }
    }
}
