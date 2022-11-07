# #!/bin/zsh
# zplug
test -e "${HOME}/.zplug/init.zsh" && source ${HOME}/.zplug/init.zsh

if ! zplug check; then
  printf "Install? [y/N]: "
  if read -q; then
    echo; zplug install
  fi
fi

zplug "zplug/zplug", hook-build:"zplug --self-manage", from:github
zplug "romkatv/powerlevel10k", as:theme, from:github, depth:1
zplug "zsh-users/zsh-syntax-highlighting", defer:2

zplug load

ZSH_THEME="powerlevel10k/powerlevel10k"

# Enable Powerlevel10k instant prompt.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Environment
[[ ! -f $HOME/.zshenv ]] || source $HOME/.zshenv

HIST_STAMPS="dd-mm-yyyy"

if type pyenv > /dev/null; then
  eval "$(pyenv init --path)"
  eval "$(pyenv init -)"
fi

# Search when using up/down arrows instead of just scrolling history
autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey "^[[A" up-line-or-beginning-search # Up
bindkey "^[[B" down-line-or-beginning-search # Down

# Aliases
[[ ! -f $HOME/.zsh_aliases ]] || source $HOME/.zsh_aliases

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
