# -*- mode: sh -*-
if [[ -f $HOME/.bashrc ]]; then
	source $HOME/.bashrc
fi

export PATH="$HOME/.cargo/bin:$PATH"
