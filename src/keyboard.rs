extern crate sdl2;
use crate::interpreter::Interpreter;

use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::{EventPump, Sdl};

fn translate_key(key: &Keycode) -> Option<u8> {
    match key {
        Keycode::Num1 => Some(0x0),
        Keycode::Num2 => Some(0x1),
        Keycode::Num3 => Some(0x2),
        Keycode::Num4 => Some(0x3),
        Keycode::Q => Some(0x4),
        Keycode::W => Some(0x5),
        Keycode::E => Some(0x6),
        Keycode::R => Some(0x7),
        Keycode::A => Some(0x8),
        Keycode::S => Some(0x9),
        Keycode::D => Some(0xa),
        Keycode::F => Some(0xb),
        Keycode::Z => Some(0xc),
        Keycode::X => Some(0xd),
        Keycode::C => Some(0xe),
        Keycode::V => Some(0xf),
        _ => None,
    }
}

#[derive(Clone, Copy)]
pub enum KbdHandlerState {
    /// The `KbdHandler` is waiting for a key press. When the key event
    /// happens, the handler should insert the key into the `vregister` of
    /// the `Interpreter` at the wrapped usize.
    Waiting(usize),
    Free,
}

pub struct KbdHandler {
    event_pump: EventPump,
    state: KbdHandlerState,
}

impl KbdHandler {
    /// Creates a new keyboard handler given an SDL `context`.
    pub fn new(context: Sdl) -> Self {
        KbdHandler {
            event_pump: context.event_pump().unwrap(),
            state: KbdHandlerState::Free,
        }
    }

    /// Getter method for the state of the keyboard handler.
    pub fn state(&self) -> KbdHandlerState {
        self.state
    }

    /// Check for keypad input and return the `keypad` state.
    pub fn poll_keypad(&self) -> [bool; 16] {
        let keys: Vec<Keycode> = self
            .event_pump
            .keyboard_state()
            .pressed_scancodes()
            .filter_map(Keycode::from_scancode)
            .collect();

        // Updates the keypad state
        let mut keypad = [false; 16];
        for k in keys.iter() {
            if let Some(idx) = translate_key(k) {
                keypad[idx as usize] = true;
            }
        }
        keypad
    }

    /// All execution stops util a valid key event is observed.
    pub fn wait_valid_key(&mut self, x: usize) -> u8 {
        loop {
            match self.event_pump.wait_event() {
                Event::KeyDown {
                    keycode: Some(key), ..
                } => {
                    // If the key is valid, return.
                    // Otherwise continue waiting for a valid key press.
                    if let Some(key_idx) = translate_key(&key) {
                        return key_idx;
                    }
                    continue;
                }
                _ => continue,
            }
        }
    }
}
