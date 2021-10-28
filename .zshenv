# Locale
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# GPG
export GPG_TTY=$(tty)

# Use nvim
export EDITOR='nvim'
export VISUAL="$EDITOR"
export GIT_EDITOR="$EDITOR"

MAX_MEMORY_UNITS=KB

TIMEFMT='%J   %U  user %S system %P cpu %*E total'$'\n'\
'avg shared (code):         %X KB'$'\n'\
'avg unshared (data/stack): %D KB'$'\n'\
'total (sum):               %K KB'$'\n'\
'max memory:                %M '$MAX_MEMORY_UNITS''$'\n'\
'page faults from disk:     %F'$'\n'\
'other page faults:         %R'

# Use ripgrep and FZF
which rg > /dev/null && alias grep=rg
if type rg &> /dev/null; then
  export FZF_DEFAULT_COMMAND='rg --files'
  export FZF_DEFAULT_OPTS='-m --height 50% --border'
fi

# Java
export JAVA_HOME="/Library/Java/JavaVirtualMachines/adoptopenjdk-15.jdk/Contents/Home"
export PATH="$JAVA_HOME/bin:$PATH"

# Compilers
export CPPFLAGS="-I$JAVA_HOME/include -I/usr/local/opt/llvm/include -I/Applications/ARM/arm-none-eabi/include"
export LDFLAGS="-L/usr/local/opt/llvm/lib"

# Path
export PATH="/Applications/ARM/bin:$PATH"
export PATH="/usr/local/opt/gnu-getopt/bin:$PATH"
export PATH="/usr/local/opt/sphinx-doc/bin:$PATH"
export PATH="/usr/local/opt/python@3.7/bin:$PATH"
export PATH="/usr/local/opt/llvm/bin:$PATH"

export PATH="$HOME/.bin:$PATH"
export PATH="$HOME/.emacs.d/bin:$PATH"
export PATH="$HOME/.yarn/bin:$PATH"
export PATH="$HOME/.config/yarn/global/node_modules/.bin:$PATH"

export WASMTIME_HOME="$HOME/.wasmtime"

export PATH="$WASMTIME_HOME/bin:$PATH"

