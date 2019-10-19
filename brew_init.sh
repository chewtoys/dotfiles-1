#!/bin/sh

echo "Installing HomeBrew"
if ! brew ls --versions bash > /dev/null; then
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

echo "Installing sofware"
brew install autojump bash bash-completion@2 bat coreutils fortune git go hunspell pandoc rbenv roswell pyenv ripgrep rlwrap rustup-init the_silver_searcher tmux tree upx wget xz findutils hledger ledger starship fd

echo "Installing go tools..."
if ! type "goimports" > /dev/null; then
    go get -u golang.org/x/tools/...
fi
