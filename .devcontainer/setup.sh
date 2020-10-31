#!/usr/bin/env bash

apt-get update
apt-get install -y \
  curl \
  git \
  gnupg2 \
  jq \
  sudo \
  zsh \
  nano \
  build-essential \
  openssl \
  openjdk-14-jdk

curl https://sh.rustup.rs -sSf | sh -s -- -y 
rustup install nightly
rustup component add --toolchain nightly rls-preview rust-analysis rust-src rustfmt
rustup component add rls-preview rust-analysis rust-src rustfmt
rustup default nightly

cargo install cargo-expand
cargo install cargo-edit

sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

cp .profile .zshrc $HOME/

chown -R $USER_UID:$USER_GID $HOME/.oh-my-zsh $HOME/.zshrc
