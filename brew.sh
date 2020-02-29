#!/bin/bash

if ! brew ls --versions bash > /dev/null; then
    echo "Installing HomeBrew"
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

# commandline
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
if test ! -d "/usr/local/Cellar/pyenv/" > /dev/null; then brew install pyenv; fi
if test ! -d "/usr/local/Cellar/ripgrep/" > /dev/null; then brew install ripgrep; fi
if test ! -d "/usr/local/Cellar/rlwrap/" > /dev/null; then brew install rlwrap; fi
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
if test ! -d "/usr/local/Cellar/httpie/" > /dev/null; then brew install httpie; fi
if test ! -d "/usr/local/Cellar/neovim/" > /dev/null; then brew install neovim; fi
if test ! -d "/usr/local/Cellar/protobuf/" > /dev/null; then brew install protobuf; fi
if test ! -d "/usr/local/Cellar/rustup-init/" > /dev/null; then brew install rustup-init; fi

# Casks
if test ! -d "/usr/local/Caskroom/" > /dev/null; then brew tap caskroom/cask; fi
if test ! -d "/usr/local/Caskroom/joplin/" > /dev/null; then brew cask install joplin; fi
if test ! -d "/usr/local/Caskroom/alfred/" > /dev/null; then brew cask install alfred; fi
if test ! -d "/usr/local/Caskroom/firefox/" > /dev/null; then brew cask install firefox; fi
if test ! -d "/usr/local/Caskroom/google-chrome/" > /dev/null; then brew cask install google-chrome; fi
if test ! -d "/usr/local/Caskroom/font-source-code-pro/" > /dev/null; then brew cask install font-source-code-pro; fi
if test ! -d "/usr/local/Caskroom/emacs/" > /dev/null; then brew cask install emacs; fi
if test ! -d "/usr/local/Caskroom/java8/" > /dev/null; then brew cask install java8; fi
if test ! -d "/usr/local/Caskroom/mactex-no-gui/" > /dev/null; then brew cask install mactex-no-gui; fi
if test ! -d "/usr/local/Caskroom/macvim/" > /dev/null; then brew cask install macvim; fi
if test ! -d "/usr/local/Caskroom/podman/" > /dev/null; then brew cask install podman; fi
if test ! -d "/usr/local/Caskroom/visual-studio-code/" > /dev/null; then brew cask install visual-studio-code; fi
if test ! -d "/usr/local/Caskroom/bettertouchtool/" > /dev/null; then brew cask install bettertouchtool; fi
if test ! -d "/usr/local/Caskroom/appcleaner/" > /dev/null; then brew cask install appcleaner; fi
if test ! -d "/usr/local/Caskroom/sublime-text/" > /dev/null; then brew cask install sublime-text; fi
if test ! -d "/usr/local/Caskroom/sublime-merge/" > /dev/null; then brew cask install sublime-merge; fi
if test ! -d "/usr/local/Caskroom/imageoptim/" > /dev/null; then brew cask install imageoptim; fi
if test ! -d "/usr/local/Caskroom/anki/" > /dev/null; then brew cask install anki; fi
if test ! -d "/usr/local/Caskroom/dropbox/" > /dev/null; then brew cask install dropbox; fi
if test ! -d "/usr/local/Caskroom/bartender/" > /dev/null; then brew cask install bartender; fi
if test ! -d "/usr/local/Caskroom/bitwarden/" > /dev/null; then brew cask install bitwarden; fi
if test ! -d "/usr/local/Caskroom/1password/" > /dev/null; then brew cask install 1password; fi
if test ! -d "/usr/local/Caskroom/iterm2/" > /dev/null; then brew cask install iterm2; fi
if test ! -d "/usr/local/Caskroom/karabiner-elements/" > /dev/null; then brew cask install karabiner-elements; fi
if test ! -d "/usr/local/Caskroom/marked/" > /dev/null; then brew cask install marked; fi
if test ! -d "/usr/local/Caskroom/postman/" > /dev/null; then brew cask install postman; fi
if test ! -d "/usr/local/Caskroom/the-unarchiver/" > /dev/null; then brew cask install the-unarchiver; fi
if test ! -d "/usr/local/Caskroom/arq/" > /dev/null; then brew cask install arq; fi
if test ! -d "/usr/local/Caskroom/telegram-desktop/" > /dev/null; then brew cask install telegram-desktop; fi
if test ! -d "/usr/local/Caskroom/font-jetbrains-mono/" > /dev/null; then brew cask install homebrew/cask-fonts/font-jetbrains-mono; fi

if ! type "goimports" > /dev/null; then
    echo "Installing go tools..."
    go get -u golang.org/x/tools/...
fi
