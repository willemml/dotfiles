# Locale
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# GPG
export GPG_TTY=$(tty)

# Use nvim
export EDITOR='nvim'
export VISUAL="$EDITOR"
export GIT_EDITOR="$EDITOR"

# Use ripgrep and FZF
which rg > /dev/null && alias grep=rg
if type rg &> /dev/null; then
  export FZF_DEFAULT_COMMAND='rg --files'
  export FZF_DEFAULT_OPTS='-m --height 50% --border'
fi

# Java
export JAVA_HOME="/usr/local/opt/openjdk"
export PATH="$JAVA_HOME/bin:$PATH"

# Compilers
export CPPFLAGS="-I$JAVA_HOME/include -I/usr/local/opt/llvm/include"
export LDFLAGS="-L/usr/local/opt/llvm/lib"

# Path
export PATH="/usr/local/opt/avr-gcc@8/bin:$PATH"
export PATH="/usr/local/opt/gnu-getopt/bin:$PATH"
export PATH="/usr/local/opt/sphinx-doc/bin:$PATH"
export PATH="/usr/local/opt/python@3.7/bin:$PATH"
export PATH="/usr/local/opt/llvm/bin:$PATH"

export PATH="$HOME/.bin:$PATH"
export PATH="$HOME/.emacs.d/bin:$PATH"
export PATH="$HOME/.yarn/bin:$PATH"
export PATH="$HOME/.config/yarn/global/node_modules/.bin:$PATH"
