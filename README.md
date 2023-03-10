# My XMonad configuration

This is the configuration that I, jemand2001, use on all my computers.

## To Use

Firtly, clone this repository into `~/.xmonad` or `~/.config/xmonad`.

### Build Requirements
- cabal (from [ghcup](https://www.haskell.org/ghcup/), or installed via your package manager -- the package is often called `cabal-install`).

*In theory*, cabal should take care of all other dependencies. If it does not, feel free to [open an issue](https://github.com/jemand2001/xmonad-config/issues/new/choose).

### Setup

If your architecture is `x86-64`, you don't need to do anything in this step.
If it is not, run `ln -sfT xmonad-config-cabal xmonad-$(uname -m)-linux`.
Don't worry that the target of that symlink doesn't exist yet, that will be fixed in the next step.

### Building

Simply run `./build`. Alternatively: configure xmonad as your desktop session, and log in. Xmonad *should* find the build script here and compile this configuration automatically.

### Runtime Requirements

- a desktop notification service (also DBus)
- `flameshot`: the best screenshot app
- `rofi`: a popup to switch between windows and run things
- the [`liskin-media`](https://github.com/liskin/dotfiles/blob/15c2cd83ce7297c38830053a9fd2be2f3678f4b0/bin/liskin-media) shell script somewhere in the PATH, if you want somewhat proper media key support (see also [the original blog post](https://work.lisk.in/2020/05/06/linux-media-control.html))

You should also take a look at `Conf.hs`: this file contains some details, such as the terminal emulator, background image, the path to an autorun script, and some others.
