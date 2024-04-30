#!/usr/bin/env zsh
# Nice automatic settings
set -o vi
set -o autocd

# Configure plugins
if [ "$TERM" != "eterm-color" ]; then
  . "$HOME"/.agkozak-zsh-prompt/agkozak-zsh-prompt.plugin.zsh
fi

. "$HOME"/.zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh
. "$HOME"/.zsh-autosuggestions/zsh-autosuggestions.plugin.zsh

# Configure aliases
. "$HOME"/.aliases.sh

function man() {
	env \
		LESS_TERMCAP_md=$(tput bold; tput setaf 4) \
		LESS_TERMCAP_me=$(tput sgr0) \
		LESS_TERMCAP_mb=$(tput blink) \
		LESS_TERMCAP_us=$(tput setaf 2) \
		LESS_TERMCAP_ue=$(tput sgr0) \
		LESS_TERMCAP_so=$(tput smso) \
		LESS_TERMCAP_se=$(tput rmso) \
		PAGER="${commands[less]:-$PAGER}" \
		man "$@"
}

function gig() {
  curl https://www.toptal.com/developers/gitignore/api/"$1" >.gitignore
}


# Configure direnv
eval "$(direnv hook zsh)"

# Configure Zoxide
eval "$(zoxide init zsh --cmd cd)"

# Configure McFly
export MCFLY_LIGHT=TRUE
export MCFLY_KEY_SCHEME=vim
eval "$(mcfly init zsh)"

# Go to the current project
gp

# opam configuration
[[ ! -r /Users/joshuahoeflich/.opam/opam-init/init.zsh ]] || source /Users/joshuahoeflich/.opam/opam-init/init.zsh  > /dev/null 2> /dev/null
