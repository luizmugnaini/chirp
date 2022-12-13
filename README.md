# `chirp` 🐣

This is a simple [CHIP-8](https://en.wikipedia.org/wiki/CHIP-8) interpreter I
built with the intention of learning the the ways of hardware emulation
(although CHIP-8 is merely an interpreter, not a piece of hardware).

# Dependencies

The display of the interpreter depends on SDL, so you may have to install
```
pacman -S sdl2
```
with this you are good to go.

# Playing a game

All games I tested are present in the `games/` directory. In order to play one
of them, you can simply run
```
cargo run games/name_of_the_game
```
You can also provide the path of another game you may have downloaded.

# References

The main resource for this project is [Cowgod's Chip-8 Technical Reference](http://devernay.free.fr/hacks/chip8/C8TECH10.HTM).
