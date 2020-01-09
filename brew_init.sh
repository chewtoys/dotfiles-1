#!/bin/bash

if ! brew ls --versions bash > /dev/null; then
    echo "Installing HomeBrew"
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

echo "Installing sofware"
if test ! -d "/usr/local/Cellar/autojump/" > /dev/null; then brew install autojump; fi
if test ! -d "/usr/local/Cellar/bash/" > /dev/null; then brew install bash; fi
if test ! -d "/usr/local/Cellar/bash-completion@2/" > /dev/null; then brew install bash-completion@2; fi
if test ! -d "/usr/local/Cellar/bat/" > /dev/null; then brew install bat; fi
if test ! -d "/usr/local/Cellar/coreutils/" > /dev/null; then brew install coreutils; fi
if test ! -d "/usr/local/Cellar/clang-format/" > /dev/null; then brew install clang-format; fi
if test ! -d "/usr/local/Cellar/fortune/" > /dev/null; then brew install fortune; fi
if test ! -d "/usr/local/Cellar/git/" > /dev/null; then brew install git; fi
if test ! -d "/usr/local/Cellar/go/" > /dev/null; then brew install go; fi
if test ! -d "/usr/local/Cellar/hunspell/" > /dev/null; then brew install hunspell; fi
if test ! -d "/usr/local/Cellar/pandoc/" > /dev/null; then brew install pandoc; fi
if test ! -d "/usr/local/Cellar/rbenv/" > /dev/null; then brew install rbenv; fi
if test ! -d "/usr/local/Cellar/roswell/" > /dev/null; then brew install roswell; fi
if test ! -d "/usr/local/Cellar/pyenv/" > /dev/null; then brew install pyenv; fi
if test ! -d "/usr/local/Cellar/ripgrep/" > /dev/null; then brew install ripgrep; fi
if test ! -d "/usr/local/Cellar/rlwrap/" > /dev/null; then brew install rlwrap; fi
if test ! -d "/usr/local/Cellar/rustup-init/" > /dev/null; then brew install rustup-init; fi
if test ! -d "/usr/local/Cellar/the_silver_searcher/" > /dev/null; then brew install the_silver_searcher; fi
if test ! -d "/usr/local/Cellar/tmux/" > /dev/null; then brew install tmux; fi
if test ! -d "/usr/local/Cellar/tree/" > /dev/null; then brew install tree; fi
if test ! -d "/usr/local/Cellar/upx/" > /dev/null; then brew install upx; fi
if test ! -d "/usr/local/Cellar/wget/" > /dev/null; then brew install wget; fi
if test ! -d "/usr/local/Cellar/xz/" > /dev/null; then brew install xz; fi
if test ! -d "/usr/local/Cellar/findutils/" > /dev/null; then brew install findutils; fi
if test ! -d "/usr/local/Cellar/hledger/" > /dev/null; then brew install hledger; fi
if test ! -d "/usr/local/Cellar/ledger/" > /dev/null; then brew install ledger; fi
if test ! -d "/usr/local/Cellar/starship/" > /dev/null; then brew install starship; fi
if test ! -d "/usr/local/Cellar/fd/" > /dev/null; then brew install fd; fi
if test ! -d "/usr/local/Cellar/vale/" > /dev/null; then brew install vale; fi
if test ! -d "/usr/local/Cellar/ccls/" > /dev/null; then brew install ccls; fi

if ! type "goimports" > /dev/null; then
    echo "Installing go tools..."
    go get -u golang.org/x/tools/...
fi

echo "Installing rls"
rustup component add rls rust-analysis rust-src

