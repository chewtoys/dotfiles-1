#!/bin/bash

if ! brew ls --versions bash > /dev/null; then
    echo "Installing HomeBrew"
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

echo "Installing sofware"
brew install autojump
brew install bash
brew install bash-completion@2
brew install bat
brew install coreutils
brew install clang-format
brew install fortune
brew install git
brew install go
brew install hunspell
brew install pandoc
brew install rbenv
brew install roswell
brew install pyenv
brew install ripgrep
brew install rlwrap
brew install rustup-init
brew install the_silver_searcher
brew install tmux
brew install tree
brew install upx
brew install wget
brew install xz
brew install findutils
brew install hledger
brew install ledger
brew install starship
brew install fd
brew install vale
brew install llvm

if ! type "goimports" > /dev/null; then
    echo "Installing go tools..."
    go get -u golang.org/x/tools/...
fi

go get -u github.com/klauspost/asmfmt/cmd/asmfmt
go get -u github.com/go-delve/delve/cmd/dlv
go get -u github.com/kisielk/errcheck
go get -u github.com/davidrjenni/reftools/cmd/fillstruct
go get -u github.com/mdempsky/gocode
go get -u github.com/stamblerre/gocode
go get -u github.com/rogpeppe/godef
go get -u github.com/zmb3/gogetdoc
go get -u golang.org/x/tools/cmd/goimports
go get -u golang.org/x/lint/golint
go get -u golang.org/x/tools/gopls
go get -u github.com/golangci/golangci-lint/cmd/golangci-lint
go get -u github.com/fatih/gomodifytags
go get -u golang.org/x/tools/cmd/gorename
go get -u github.com/jstemmer/gotags
go get -u golang.org/x/tools/cmd/guru
go get -u github.com/josharian/impl
go get -u honnef.co/go/tools/cmd/keyify
go get -u github.com/fatih/motion
go get -u github.com/koron/iferr

echo "Installing rls"
rustup component add rls rust-analysis rust-src
