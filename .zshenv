# Oh My ZSH
export ZSH="$HOME/.oh-my-zsh"

# Locale
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

# Brew
if [ -f "/opt/homebrew/bin/brew" ]; then
	export HOMEBREW_NO_ANALYTICS=0
	eval "$(/opt/homebrew/bin/brew shellenv)"
fi

# Rust
CARGOENV="$HOME/.cargo/env"
if [ -f "$CARGOENV" ]; then
	. $CARGOENV
fi

# home local bin
[[ -d "$HOME/.local/bin/" ]] && export PATH="$HOME/.local/bin:$PATH"

# Node Version Manager
export NVM_DIR="$HOME/.local/nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"                   # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion" # This loads nvm bash_completion

# Yarn
export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"

# GPG
export GPG_TTY=$(tty)

# k8s
export KUBECONFIG="$HOME/homelab/kubespray-admin.conf"

EMACS_BIN="/Applications/Emacs.app/Contents/MacOS/bin/"
[[ -d "$EMACS_BIN" ]] && export PATH="$EMACS_BIN:$PATH"

if type emacsclient &>/dev/null; then
	export EDITOR=('emacsclient' '-c')
	if type emacs &>/dev/null; then
		EDITOR+=('-a' 'emacs')
	fi
elif type nano &>/dev/null; then
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
JHOME="/Library/Java/JavaVirtualMachines/temurin-17.jdk/Contents/Home"
#JHOME="/opt/homebrew/opt/openjdk"
if [ -d "$JHOME" ]; then
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

