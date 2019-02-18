source /usr/local/share/antigen/antigen.zsh

# Load the oh-my-zsh's library
antigen use oh-my-zsh

# from oh-my-zsh
antigen bundle cargo
antigen bundle colorize
antigen bundle extract
antigen bundle git
antigen bundle golang
antigen bundle rust
antigen bundle vscode
antigen bundle z

# Syntax highlighting bundle.
antigen bundle zsh-users/zsh-syntax-highlighting
# Extra zsh completions
antigen bundle zsh-users/zsh-completions
# Load the theme
antigen theme https://github.com/halfo/lambda-mod-zsh-theme lambda-mod

# Tell antigen that you're done
antigen apply

zstyle ':completion:*' rehash true

local aliasfile="${HOME}/.dotfiles/bash/aliases"

if [ -r ${aliasfile} ]; then
  source ${aliasfile}
fi

if [ -r ~/.private ]; then
  source ~/.private
fi

export LC_CTYPE=en_US.UTF-8
export LANG=en_US.UTF-8
export LESSCHARSET=utf-8

# added by travis gem
[ -f ~/.travis/travis.sh ] && source ~/.travis/travis.sh

# rbenv
if type "rbenv" > /dev/null; then
  eval "$(rbenv init -)"
fi

fg() {
    if [[ $# -eq 1 && $1 = - ]]; then
        builtin fg %-
    else
        builtin fg %"$@"
    fi
}

 function diff {
     colordiff -u "$@" | less -RF
 }

# precmd() { print "" }
# PS1="⟩⟩⟩ "
# RPS1="%{$fg[magenta]%}%20<...<%~%<<%{$reset_color%}"
