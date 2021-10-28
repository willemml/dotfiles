export ZSH="$HOME/.oh-my-zsh"

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

plugins=(git history)
setopt extendedglob

eval $(minikube docker-env)

# Oh My Zsh
source $ZSH/oh-my-zsh.sh

# Rust Cargo
source $HOME/.cargo/env

# Environment
source $HOME/.zshenv

# Aliases
source $HOME/.zsh_aliases
