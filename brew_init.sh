#!/bin/sh

echo "Installing HomeBrew"
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
echo "Installing sofware"
brew install bash bash-completion@2 bat coreutils fortune git go haskell-stack maven pandoc rbenv pyenv ripgrep rlwrap rustup-init the_silver_searcher tmux tree upx wget xz z
