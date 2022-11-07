#!/usr/zsh
# [[file:docs/zshenv.org::*Locale][Locale:1]]
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
# Locale:1 ends here

# [[file:docs/zshenv.org::*Brew][Brew:1]]
if [ -f "/opt/homebrew/bin/brew" ]; then
	export HOMEBREW_NO_ANALYTICS=0
	eval "$(/opt/homebrew/bin/brew shellenv)"
fi
# Brew:1 ends here

# [[file:docs/zshenv.org::*Python][Python:1]]
if type pyenv > /dev/null; then
	eval "$(pyenv init --path)"
	eval "$(pyenv init -)"
fi
# Python:1 ends here

# [[file:docs/zshenv.org::*Rust][Rust:1]]
CARGOENV="$HOME/.cargo/env"
if [ -f "$CARGOENV" ]; then
	. $CARGOENV
fi
# Rust:1 ends here

# [[file:docs/zshenv.org::*Cross compiling][Cross compiling:1]]
if type x86_64-unknown-linux-gnu-gcc &>/dev/null; then
	export CC_x86_64_unknown_linux_gnu=x86_64-unknown-linux-gnu-gcc
	export CARGO_TARGET_X86_64_UNKNOWN_LINUX_GNU_LINKER=x86_64-unknown-linux-gnu-gcc
fi

if type x86_64-unknown-linux-gnu-g++ &>/dev/null; then
	export CXX_x86_64_unknown_linux_gnu=x86_64-unknown-linux-gnu-g++
fi

if type x86_64-unknown-linux-gnu-ar &>/dev/null; then
	export AR_x86_64_unknown_linux_gnu=x86_64-unknown-linux-gnu-ar
fi
# Cross compiling:1 ends here

# [[file:docs/zshenv.org::*Node][Node:1]]
export NVM_DIR="$HOME/.local/nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"                   # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion" # This loads nvm bash_completion
# Node:1 ends here

# [[file:docs/zshenv.org::*Node][Node:2]]
export PATH="$HOME/.yarn/bin:$PATH"
export PATH="$HOME/.config/yarn/global/node_modules/.bin:$PATH"
# Node:2 ends here

# [[file:docs/zshenv.org::*Java][Java:1]]
JHOME="/Library/Java/JavaVirtualMachines/temurin-17.jdk/Contents/Home"
if [ -d "$JHOME" ]; then
	export JAVA_HOME="$JHOME"
	export PATH="$JAVA_HOME/bin:$PATH"
	export CPPFLAGS="-I$JAVA_HOME/include"
fi
# Java:1 ends here

# [[file:docs/zshenv.org::*LLVM (C, C++, etc...)][LLVM (C, C++, etc...):1]]
export CPPFLAGS="$CPPFLAGS -I/usr/local/opt/llvm/include"
IARM="/Applications/ARM/arm-none-eabi/include"
if [ -f "$IARM" ]; then
	export CPPFLAGS="$CPPFLAGS -I$IARM"
fi
ILLVM="/usr/local/opt/llvm/include"
if [ -f "$ILLVM" ]; then
	export CPPFLAGS="$CPPFLAGS -I$ILLVM"
fi
FLLVM="/usr/local/opt/llvm/lib"
if [ -f "$FLLVM" ]; then
	export LDFLAGS="$FLLVM"
fi
# LLVM (C, C++, etc...):1 ends here

# [[file:docs/zshenv.org::*Nim][Nim:1]]
export PATH=/Users/willem/.nimble/bin:$PATH
# Nim:1 ends here

# [[file:docs/zshenv.org::*Editor][Editor:1]]
if type emacsclient &>/dev/null; then
	export EDITOR=('emacsclient')
	if type emacs &>/dev/null; then
		EDITOR+=('-a' 'emacs')
	fi
elif type nano &>/dev/null; then
	export EDITOR='nano'
else
	export EDITOR='vi'
fi
# Editor:1 ends here

# [[file:docs/zshenv.org::*Editor][Editor:2]]
if [[ -v EDITOR ]]; then
	export VISUAL="$EDITOR"
	export GIT_EDITOR="$EDITOR"
fi
# Editor:2 ends here

# [[file:docs/zshenv.org::*Editor][Editor:3]]
EMACS_BIN="/Applications/Emacs.app/Contents/MacOS/bin/"
[[ -d "$EMACS_BIN" ]] && export PATH="$EMACS_BIN:$PATH"
# Editor:3 ends here

# [[file:docs/zshenv.org::*Editor][Editor:4]]
export LSP_USE_PLISTS=true
# Editor:4 ends here

# [[file:docs/zshenv.org::*Setup time format][Setup time format:1]]
read -r -d '' TIMEFMT <<EOM
%J   %U  user %S system %P cpu %*E total
  avg shared (code):         %X KB
  avg unshared (data/stack): %D KB
  total (sum):               %K KB
  max memory:                %M KB
  page faults from disk:     %F
  other page faults:         %R
EOM
# Setup time format:1 ends here

# [[file:docs/zshenv.org::*Path][Path:1]]
[[ -d "$HOME/.local/bin/" ]] && export PATH="$HOME/.local/bin:$PATH"
# Path:1 ends here

# [[file:docs/zshenv.org::*Set XDG home][Set XDG home:1]]
export XDG_CONFIG_HOME="$HOME/.config/"
# Set XDG home:1 ends here

# [[file:docs/zshenv.org::*GPG][GPG:1]]
export GPG_TTY=$(tty)
# GPG:1 ends here
