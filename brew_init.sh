#!/bin/sh

echo "Installing HomeBrew"
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
echo "Installing sofware"
brew install autoconf autoenv autojump cmake emacs fortune gdbm git haskell-stack libyaml openssl perl pkg-config python rbenv readline ruby ruby-build sqlite ucl upx vim xz
