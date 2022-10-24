# Enable Powerlevel10k instant prompt.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Environment
[[ ! -f $HOME/.zshenv ]] || source $HOME/.zshenv

OMZSH="$ZSH/oh-my-zsh.sh"

if [ -f "$OMZSH" ]; then
  ZSH_THEME="powerlevel10k/powerlevel10k"

  HIST_STAMPS="dd-mm-yyyy"

  plugins=(git history)

  source $OMZSH
fi

if type pyenv > /dev/null; then
  eval "$(pyenv init --path)"
  eval "$(pyenv init -)"
fi

# Aliases
[[ ! -f $HOME/.zsh_aliases ]] || source $HOME/.zsh_aliases

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
