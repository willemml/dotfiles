# Locale
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# GPG
export GPG_TTY=$(tty)

# Use nvim
if type nvim&>/dev/null; then
export EDITOR='nvim'
export VISUAL="$EDITOR"
export GIT_EDITOR="$EDITOR"
fi

export XDG_CONFIG_HOME="$HOME/.config/"

read -r -d '' TIMEFMT <<EOM
%J   %U  user %S system %P cpu %*E total
  avg shared (code):         %X KB
  avg unshared (data/stack): %D KB
  total (sum):               %K KB
  max memory:                %M KB
  page faults from disk:     %F
  other page faults:         %R
EOM

# Use ripgrep and FZF
if type rg &>/dev/null; then
  alias grep=rg
  export FZF_DEFAULT_COMMAND='rg --files'
  export FZF_DEFAULT_OPTS='-m --height 50% --border'
fi

# Java
JHOME="/Library/Java/JavaVirtualMachines/adoptopenjdk-15.jdk/Contents/Home"
if [ -f "$JHOME" ]; then
  export JAVA_HOME="$JHOME"
  export PATH="$JAVA_HOME/bin:$PATH"
  export CPPFLAGS="-I$JAVA_HOME/include"
fi

# Compilers
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

# Path
export PATH="/usr/local/opt/gnu-getopt/bin:$PATH"
export PATH="/usr/local/opt/sphinx-doc/bin:$PATH"
export PATH="/usr/local/opt/python@3.7/bin:$PATH"
export PATH="/usr/local/opt/llvm/bin:$PATH"

export PATH="$HOME/.emacs.d/bin:$PATH"
export PATH="$HOME/.yarn/bin:$PATH"
export PATH="$HOME/.config/yarn/global/node_modules/.bin:$PATH"

export PATH="$HOME/.scripts:$PATH"

. "$HOME/.cargo/env"
