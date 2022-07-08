export ZSH="$HOME/.oh-my-zsh"
OMZSH="$ZSH/oh-my-zsh.sh"

# Environment
source $HOME/.zshenv

if [ -f "/opt/homebrew/bin/brew" ]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi

eval $(thefuck --alias)

if type pyenv > /dev/null; then
  eval "$(pyenv init --path)"
  eval "$(pyenv init -)"
fi

# Rust Cargo
CARGO=$HOME/.cargo/env
if [ -f "$CARGO" ]; then
  source $CARGO
fi

if [ -f "$OMZSH" ]; then
  ZSH_THEME="robbyrussell"

  # Uncomment the following line to use hyphen-insensitive completion.
  # Case-sensitive completion must be off. _ and - will be interchangeable.
  HYPHEN_INSENSITIVE="true"

  DISABLE_UNTRACKED_FILES_DIRTY="true"

  # Uncomment the following line if you want to change the command execution time
  # stamp shown in the history command output.
  # You can set one of the optional three formats:
  # "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
  # or set a custom format using the strftime function format specifications,
  # see 'man strftime' for details.
  HIST_STAMPS="dd-mm-yyyy"

  plugins=(git history thefuck)
  setopt extendedglob

  # Oh My Zsh
  source $OMZSH
fi

# Aliases
source $HOME/.zsh_aliases

if type minikube > /dev/null; then
  eval $(minikube docker-env)
fi
