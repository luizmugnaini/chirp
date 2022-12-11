extern crate sdl2;
use sdl2::render::Canvas;
use sdl2::video::Window;

/// Factor used to scale the original size of the CHIP-8 screen.
pub const SCALE_FACTOR: usize = 5;

pub struct DisplayHandler {
    canvas: Canvas<Window>,
}

impl DisplayHandler {
    pub fn new(canvas: Canvas<Window>) -> Self {
        DisplayHandler { canvas }
    }
}
