extern crate sdl2;
use sdl2::{pixels::Color, rect::Rect, render::Canvas, video::Window};

use crate::{DISPLAY_HEIGHT, DISPLAY_WIDTH};

/// Factor used to scale the original size of the CHIP-8 screen.
pub const SCALE_FACTOR: u32 = 20;

pub struct DisplayHandler {
    canvas: Canvas<Window>,
}

impl DisplayHandler {
    pub fn new(canvas: Canvas<Window>) -> Self {
        DisplayHandler { canvas }
    }

    pub fn draw_to_canvas(&mut self, new_canvas: &[[u8; DISPLAY_WIDTH]; DISPLAY_HEIGHT]) {
        self.canvas.set_draw_color(Color::BLACK);
        self.canvas.clear();
        for (j, row) in new_canvas.iter().enumerate() {
            for (i, col) in row.iter().enumerate() {
                match col {
                    &0u8 => {}
                    _ => {
                        self.canvas.set_draw_color(Color::MAGENTA);
                        self.canvas
                            .fill_rect(Rect::new(
                                (i as i32) * SCALE_FACTOR as i32,
                                (j as i32) * SCALE_FACTOR as i32,
                                SCALE_FACTOR,
                                SCALE_FACTOR,
                            ))
                            .expect("Unable to draw to canvas.")
                    }
                }
            }
        }
    }

    pub fn show(&mut self) {
        self.canvas.present();
    }
}
