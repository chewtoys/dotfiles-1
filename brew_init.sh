#!/bin/sh

echo "Installing HomeBrew"
if ! brew ls --versions bash > /dev/null; then
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

echo "Installing sofware"
brew install cask bash bash-completion@2 bat coreutils fortune git go haskell-stack pandoc rbenv pyenv ripgrep rlwrap rustup-init the_silver_searcher tmux tree upx wget xz z

echo "Installing go tools..."
go get -u golang.org/x/tools/...
