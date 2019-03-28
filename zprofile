export GOPATH="$HOME/go"
export PATH="$GOPATH/bin:~/.local/bin:~/.cargo/bin/:$PATH:~/.roswell/bin/"
export PATH="/usr/local/bin:$HOME/.rbenv/bin:$PATH"

alias emacsclient="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"

export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t"         # $EDITOR opens in terminal
export VISUAL="emacsclient -c -a emacs"         # $VISUAL opens in GUI mode
