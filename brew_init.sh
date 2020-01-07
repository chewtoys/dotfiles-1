#!/bin/bash

if ! brew ls --versions bash > /dev/null; then
    echo "Installing HomeBrew"
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

echo "Installing sofware"
if ! type "autojump" > /dev/null; then brew install autojump; fi
if test ! -f "/usr/local/bin/bash" > /dev/null; then brew install bash; fi
if test ! -d "/usr/local/Cellar/bash-completion@2/" > /dev/null; then brew install bash-completion@2; fi
if ! type "bat" > /dev/null; then brew install bat; fi
if test ! -d "/usr/local/Cellar/coreutils/" > /dev/null; then brew install coreutils; fi
if ! type "clang-format" > /dev/null; then brew install clang-format; fi
if ! type "fortune" > /dev/null; then brew install fortune; fi
if test ! -d "/usr/local/Cellar/git/" > /dev/null; then brew install git; fi
if ! type "fortune" > /dev/null; then brew install go; fi
if ! type "hunspell" > /dev/null; then brew install hunspell; fi
if ! type "pandoc" > /dev/null; then brew install pandoc; fi
if ! type "rbenv" > /dev/null; then brew install rbenv; fi
if ! type "ros" > /dev/null; then brew install roswell; fi
if ! type "pyenv" > /dev/null; then brew install pyenv; fi
if ! type "rg" > /dev/null; then brew install ripgrep; fi
if ! type "rlwrap" > /dev/null; then brew install rlwrap; fi
if ! type "rustup" > /dev/null; then brew install rustup-init; fi
if ! type "ag" > /dev/null; then brew install the_silver_searcher; fi
if ! type "tmux" > /dev/null; then brew install tmux; fi
if ! type "tree" > /dev/null; then brew install tree; fi
if ! type "upx" > /dev/null; then brew install upx; fi
if ! type "wget" > /dev/null; then brew install wget; fi
if ! type "xz" > /dev/null; then brew install xz; fi
if test ! -d "/usr/local/Cellar/findutils/" > /dev/null; then brew install findutils; fi
if ! type "hledger" > /dev/null; then brew install hledger; fi
if ! type "ledger" > /dev/null; then brew install ledger; fi
if ! type "starship" > /dev/null; then brew install starship; fi
if ! type "fd" > /dev/null; then brew install fd; fi
if ! type "vale" > /dev/null; then brew install vale; fi
if ! type "ccls" > /dev/null; then brew install ccls; fi

if ! type "goimports" > /dev/null; then
    echo "Installing go tools..."
    go get -u golang.org/x/tools/...
fi

echo "Installing rls"
rustup component add rls rust-analysis rust-src

