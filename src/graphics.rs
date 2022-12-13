extern crate sdl2;
use sdl2::{pixels::Color, rect::Rect, render::Canvas, video::Window, Sdl};

use crate::interpreter::{DISPLAY_HEIGHT, DISPLAY_WIDTH};

/// Factor used to scale the original size of the CHIP-8 screen.
pub const SCALE_FACTOR: u32 = 20;

pub struct DisplayHandler {
    canvas: Canvas<Window>,
    draw: bool,
}

impl DisplayHandler {
    pub fn new(context: &Sdl, game_name: &str) -> Self {
        let video_subsystem = context.video().unwrap();
        let window = video_subsystem
            .window(
                format!("CHIP-8 interpreter: {game_name}").as_str(),
                SCALE_FACTOR * DISPLAY_WIDTH as u32,
                SCALE_FACTOR * DISPLAY_HEIGHT as u32,
            )
            .position_centered()
            .build()
            .unwrap();
        let canvas = window.into_canvas().build().unwrap();

        DisplayHandler {
            canvas,
            draw: false,
        }
    }

    pub fn up_draw_flag(&mut self) {
        self.draw = true;
    }

    pub fn draw_to_canvas(&mut self, new_canvas: &[[u8; DISPLAY_WIDTH]; DISPLAY_HEIGHT]) {
        self.canvas.set_draw_color(Color::BLACK);
        self.canvas.clear();

        // Draw the `new_canvas` to the screen.
        for (j, row) in new_canvas.iter().enumerate() {
            for (i, col) in row.iter().enumerate() {
                match col {
                    // Since the pixel is already black from cleaning the screen, we do nothing.
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

        // Reset the draw flag.
        self.draw = false;
    }

    pub fn show(&mut self) {
        self.canvas.present();
    }
}
