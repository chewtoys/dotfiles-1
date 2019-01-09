source /usr/local/share/antigen/antigen.zsh

# Load the oh-my-zsh's library
antigen use oh-my-zsh

# from oh-my-zsh
# antigen bundle cargo
antigen bundle colorize
# antigen bundle command-not-found
antigen bundle extract
antigen bundle git
# antigen bundle golang
#antigen bundle marked2
# antigen bundle rbenv
# antigen bundle rust
# antigen bundle tmux
#antigen bundle vi-mode
#antigen bundle vscode
antigen bundle z

# Syntax highlighting bundle.
#antigen bundle zsh-users/zsh-syntax-highlighting
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

local aliasfile="${HOME}/.dotfiles/bash/aliases"

if [ -r ${aliasfile} ]; then
  source ${aliasfile}
fi

export GOPATH=$HOME/go
export PATH=$GOPATH/bin:~/.local/bin:~/.cargo/bin/:$PATH

if [ -r ~/.private ]; then
  source ~/.private
fi

export LANG="en_EN.UTF-8"
