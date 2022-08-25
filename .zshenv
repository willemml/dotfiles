# Oh My ZSH
export ZSH="$HOME/.oh-my-zsh"

# Locale
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# Brew
if [ -f "/opt/homebrew/bin/brew" ]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi

# Rust
CARGOENV="$HOME/.cargo/env"
if [ -f "$CARGOENV" ]; then
. $CARGOENV
fi


# GPG
export GPG_TTY=$(tty)

# k8s
export KUBECONFIG="$HOME/homelab/kubespray-admin.conf"

# nvim UBC
export PATH="$PATH:$HOME/.local/nvim-linux64/bin"

# Use nvim
if type nvim&>/dev/null; then
  export EDITOR='nvim'
elif type vim&>/dev/null; then
  export EDITOR='vim'
elif type vi&>/dev/null; then
  export EXINIT="$HOME/.exrc"
  export EDITOR='vi'
elif type ex&>/dev/null; then
  export EXINIT="$HOME/.exrc"
  export EDITOR='ex'
elif type nano&>/dev/null; then
  export EDITOR='nano'
elif nc -z 1.1.1.1 53 >/dev/null 2>&1; then
  echo "You don't seem to have any text editors but you DO have an internet connection..."
  echo "You should probably install a text editor."
  export EDITOR="echo Why haven\'t you installed an editor yet?"
else
  echo "You probably DON'T have an internet connect OR a text editor..."
  echo "So you're probably completely fucked."
  export EDITOR='echo "I told you that you were fucked..."'
fi

if [[ -v EDITOR ]]; then
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
  if type fzf &>/dev/null; then
    export FZF_DEFAULT_COMMAND='rg --files'
    export FZF_DEFAULT_OPTS='-m --height 50% --border'
  fi
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

export PATH="$HOME/.yarn/bin:$PATH"
export PATH="$HOME/.config/yarn/global/node_modules/.bin:$PATH"

export PATH="$HOME/.scripts:$PATH"

