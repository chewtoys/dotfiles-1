#!/bin/sh

echo "Installing HomeBrew"
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
echo "Installing sofware"
brew install autoconf antigen bash bash-completion cmake coreutils emacs fish fortune gdbm gettext git glib gmp gnu-tar gnutls go haskell-stack jemalloc libevent libffi libidn2 libpng libssh2 libtasn1 libtermkey libunistring libuv libvterm luajit maven micro midnight-commander msgpack ncurses neovim nettle noah noahstrap openssl p11-kit pandoc pcre pcre2 pkg-config pv python@2 rbenv readline reattach-to-user-namespace ripgrep rlwrap ruby-build rustup-init s-lang sbcl sqlite telnet the_silver_searcher tmux tree unibilium upx wget xz z zsh
