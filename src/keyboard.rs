extern crate sdl2;
use sdl2::{event::Event, keyboard::Keycode, EventPump, Sdl};

fn keycode_to_index(key: &Keycode) -> Option<u8> {
    eprintln!("{key:?}");
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
    Waiting(usize),
    Free,
}

pub struct KbdHandler {
    event_pump: EventPump,
    state: KbdHandlerState,
}

impl KbdHandler {
    /// Creates a new keyboard handler given an SDL `context`.
    pub fn new(context: &Sdl) -> Self {
        KbdHandler {
            event_pump: context.event_pump().unwrap(),
            state: KbdHandlerState::Free,
        }
    }

    fn get_keycodes(&self) -> Vec<Keycode> {
        self.event_pump
            .keyboard_state()
            .pressed_scancodes()
            .filter_map(Keycode::from_scancode)
            .collect()
    }

    /// Check for keypad input and return the `keypad` state. Returns the new
    /// state of the keypad `Ok([bool; 16])` or an `Err(())`, indicating that
    /// the user demands the program to stop.
    pub fn poll_keypad(&mut self) -> Result<[bool; 16], ()> {
        // TODO: This is not a good behaviour, it would be nicer if we could merge the
        // look for keys with this first for loop through the events.
        for event in self.event_pump.poll_iter() {
            match event {
                // Either a quit event or pressing escape should immediatly stop the program from
                // running.
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => return Err(()),
                // TODO: maybe we could insert the check for the valid keys here.
                _ => {}
            }
        }

        // Update the keypad state.
        let mut new_keypad = [false; 16];
        let keys = self.get_keycodes();
        for k in keys.iter() {
            if let Some(key_idx) = keycode_to_index(k) {
                // Register the key into the keypad.
                new_keypad[key_idx as usize] = true;
                eprintln!("{new_keypad:?}");
            }
        }
        Ok(new_keypad)
    }

    /// All execution stops util a valid key event is observed.
    pub fn wait_for_key(&mut self, x: usize) {
        self.state = KbdHandlerState::Waiting(x);
    }

    /// Get the current state of the handler.
    pub fn state(&self) -> KbdHandlerState {
        self.state
    }

    /// Set the handler to a free state.
    pub fn free(&mut self) {
        self.state = KbdHandlerState::Free;
    }

    // NOTE: the purpose of this code is solely for debugging and should be later
    // deleted.
    pub fn is_waiting(&self) -> Option<usize> {
        match self.state {
            KbdHandlerState::Waiting(x) => Some(x),
            KbdHandlerState::Free => None,
        }
    }
}
