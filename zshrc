# Path to your oh-my-zsh configuration.
#ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
# ZSH_THEME="robbyrussell"
#ZSH_THEME="gentoo"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
#DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
#COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
#plugins=(autojump)

#source $ZSH/oh-my-zsh.sh

# Customize to your needs...

DEFAULT_USER="juev"

# bindkey -v
# bindkey -M viins 'jj' vi-cmd-mode
setopt auto_pushd
setopt pushd_silent
setopt pushd_ignore_dups
setopt ignore_eof
setopt rm_star_silent
unsetopt nomatch
unsetopt correct_all

PROMPT="%~ $ "

if [ -e "$HOME/.aliases" ]; then
  source "$HOME/.aliases"
fi

# Change arrow key
bindkey "^[[A" history-beginning-search-backward
bindkey "^[[B" history-beginning-search-forward

fpath=(/usr/local/share/zsh-completions $fpath)
export PATH="/usr/local/bin:$PATH"

export EDITOR="vim"

unset LD_LIBRARY_PATH
unset DYLD_LIBRARY_PATH
export CLICOLOR=1

# Docker
export DOCKER_TLS_VERIFY=1
export DOCKER_HOST=tcp://192.168.59.103:2376
export DOCKER_CERT_PATH=/Users/juev/.boot2docker/certs/boot2docker-vm
export BYOBU_PREFIX=$(brew --prefix)

# NVM
export NVM_DIR="/Users/juev/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"
export PATH="/Users/juev/.cask/bin:$PATH"
