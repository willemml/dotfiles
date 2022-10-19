# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Environment
[[ ! -f $HOME/.zshenv ]] || source $HOME/.zshenv

OMZSH="$ZSH/oh-my-zsh.sh"

if [ -f "$OMZSH" ]; then
  ZSH_THEME="powerlevel10k/powerlevel10k"

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

  # Oh My Zsh
  source $OMZSH
fi

if type pyenv > /dev/null; then
  eval "$(pyenv init --path)"
  eval "$(pyenv init -)"
fi

# kubectl completion
[[ $commands[kubectl] ]] && source <(kubectl completion zsh)

# Aliases
[[ ! -f $HOME/.zsh_aliases ]] || source $HOME/.zsh_aliases

if type minikube > /dev/null; then
  eval $(minikube docker-env)
fi

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
