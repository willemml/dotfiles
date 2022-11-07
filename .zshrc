#!/usr/zsh
# [[file:docs/zshrc.org::+begin_src shell][No heading:1]]
# This script was generated from an org-mode document see the docs
# directory.
# No heading:1 ends here

# [[file:docs/zshrc.org::*Init][Init:1]]
test -e "${HOME}/.zplug/init.zsh" && source ${HOME}/.zplug/init.zsh
# Init:1 ends here

# [[file:docs/zshrc.org::*Check][Check:1]]
if ! zplug check; then
	printf "Install? [y/N]: "
	if read -q; then
		echo; zplug install
	fi
fi
# Check:1 ends here

# [[file:docs/zshrc.org::*Plugins][Plugins:1]]
zplug "zplug/zplug", hook-build:"zplug --self-manage", from:github
zplug "romkatv/powerlevel10k", as:theme, from:github, depth:1
zplug "zsh-users/zsh-syntax-highlighting", defer:2
# Plugins:1 ends here

# [[file:docs/zshrc.org::*Load][Load:1]]
zplug load
# Load:1 ends here

# [[file:docs/zshrc.org::*powerlevel10k (theme + instant prompt)][powerlevel10k (theme + instant prompt):1]]
ZSH_THEME="powerlevel10k/powerlevel10k"

if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi
# powerlevel10k (theme + instant prompt):1 ends here

# [[file:docs/zshrc.org::*Better history search][Better history search:1]]
autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey "^[[A" up-line-or-beginning-search   # Up
bindkey "^[[B" down-line-or-beginning-search # Down
# Better history search:1 ends here

# [[file:docs/zshrc.org::*Load zsh aliases][Load zsh aliases:1]]
[[ ! -f $HOME/.zsh_aliases ]] || source $HOME/.zsh_aliases
# Load zsh aliases:1 ends here

# [[file:docs/zshrc.org::*Finish loading p10k][Finish loading p10k:1]]
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
# Finish loading p10k:1 ends here
