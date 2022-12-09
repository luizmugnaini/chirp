use std::fs::File;
use std::io::Read;

/// Struct containing the game read-only-memory
pub struct Rom {
    /// Available memory for the game to store its content. The total RAM of
    /// CHIP-8 is 4KB (`4096`) but `0x200` (`512`) bites are reserved to the
    /// interpreter, lefting the game with an available space of `3584`
    mem: [u8; 3584],
}

impl Rom {
    /// To create a new `Rom` object a `path` to the ROM file should be
    /// provided.
    pub fn new(path: &String) -> Rom {
        let mut raw_rom = File::open(path).expect("Could not open ROM file");
        let mut buf = [0u8; 3584];

        match raw_rom.read(&mut buf) {
            Err(e) => panic!("Could not read ROM file: {e}"),
            Ok(0) => panic!("ROM file is empty!"),
            _ => Rom { mem: buf },
        }
    }

    pub fn get_data(&self) -> [u8; 3584] {
        self.mem
    }
}
