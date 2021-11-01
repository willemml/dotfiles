# YADM Dotfiles
My dotfiles using [yadm](https://yadm.io), macOS only for now because I only have it set up with brew. Requires Xcode or the Xcode Command Line Tools to be installed beforehand. Also expects that you have your ssh and gpg keys setup already as this enables git commit signing by default.

Run the following in terminal to start the install:
```zsh
curl -fLo /tmp/yadm https://github.com/TheLocehiliosan/yadm/raw/master/yadm && chmod a+x /tmp/yadm

/tmp/yadm clone git@github.com:willemml/dotfiles

zsh "$HOME/.config/yadm/bootstrap"
```
