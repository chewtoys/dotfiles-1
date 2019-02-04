source /usr/local/share/antigen/antigen.zsh

# Load the oh-my-zsh's library
antigen use oh-my-zsh

# from oh-my-zsh
antigen bundle cargo
antigen bundle colorize
# antigen bundle command-not-found
antigen bundle extract
antigen bundle git
antigen bundle golang
#antigen bundle marked2
# antigen bundle rbenv
antigen bundle rust
# antigen bundle tmux
#antigen bundle vi-mode
antigen bundle vscode
antigen bundle z

# Syntax highlighting bundle.
antigen bundle zsh-users/zsh-syntax-highlighting
# Fish-like auto suggestions
#antigen bundle zsh-users/zsh-autosuggestions
# Extra zsh completions
antigen bundle zsh-users/zsh-completions
# Load the theme
antigen theme https://github.com/halfo/lambda-mod-zsh-theme lambda-mod

# Tell antigen that you're done
antigen apply

# use /etc/hosts and known_hosts for hostname completion
#[ -r ~/.ssh/known_hosts ] && _ssh_hosts=(${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[\|]*}%%\ *}%%,*}) || _ssh_hosts=()
#[ -r ~/.ssh/config ] && _ssh_config=($(cat ~/.ssh/config | sed -ne 's/Host[=\t ]//p')) || _ssh_config=()
#[ -r /etc/hosts ] && : ${(A)_etc_hosts:=${(s: :)${(ps:\t:)${${(f)~~"$(</etc/hosts)"}%%\#*}##[:blank:]#[^[:blank:]]#}}} || _etc_hosts=()
#hosts=(
  #"$_ssh_config[@]"
  #"$_global_ssh_hosts[@]"
  #"$_ssh_hosts[@]"
  #"$_etc_hosts[@]"
  #"$HOST"
  #localhost
#)
#zstyle ':completion:*:hosts' hosts $hosts
#zstyle ':completion:*' users off
zstyle ':completion:*' rehash true

local aliasfile="${HOME}/.dotfiles/bash/aliases"

if [ -r ${aliasfile} ]; then
  source ${aliasfile}
fi

export GOPATH=$HOME/go
export PATH=$GOPATH/bin:~/.local/bin:~/.cargo/bin/:~/.roswell/bin/:$PATH

if [ -r ~/.private ]; then
  source ~/.private
fi

export LC_CTYPE=en_US.UTF-8
export LANG=en_US.UTF-8
export LESSCHARSET=utf-8

export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -n"                  # $EDITOR opens in terminal
export VISUAL="emacsclient -n"         # $VISUAL opens in GUI mode

# added by travis gem
[ -f ~/.travis/travis.sh ] && source ~/.travis/travis.sh

# rbenv
export PATH="$HOME/.rbenv/bin:$PATH"
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
