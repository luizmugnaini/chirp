use sdl2::{
    audio::{AudioCallback, AudioDevice, AudioSpecDesired, AudioStatus},
    Sdl,
};

pub struct AudioHandler {
    device: AudioDevice<Beep>,
}

/// The beep sound that CHIP-8 should emmit has the form of a square wave.
struct Beep {
    phase_inc: f32,
    phase: f32,
    volume: f32,
}

impl AudioCallback for Beep {
    type Channel = f32;

    fn callback(&mut self, out: &mut [f32]) {
        // Generate a square wave for the beep sound
        for x in out.iter_mut() {
            *x = if self.phase <= 0.5 {
                self.volume
            } else {
                -self.volume
            };
            self.phase = (self.phase + self.phase_inc) % 1.0;
        }
    }
}

impl AudioHandler {
    pub fn new(context: &Sdl) -> Self {
        let audio = context.audio().unwrap();
        let desired_spec = AudioSpecDesired {
            freq: Some(25000),
            channels: Some(1), // mono
            samples: None,     // default sample size
        };

        let device = audio
            .open_playback(None, &desired_spec, |spec| {
                // initialize the audio callback
                Beep {
                    phase_inc: 250.0 / spec.freq as f32,
                    phase: 0.0,
                    volume: 0.25,
                }
            })
            .unwrap();

        AudioHandler { device }
    }

    pub fn start_beep(&mut self) {
        self.device.resume();
    }

    pub fn stop_beep(&mut self) {
        if let AudioStatus::Playing = self.device.status() {
            self.device.pause();
        }
    }
}
