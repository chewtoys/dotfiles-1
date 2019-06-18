# -*- mode: sh -*-
if [[ -f $HOME/.bashrc ]]; then
	source $HOME/.bashrc
fi

export PATH="$HOME/.cargo/bin:$PATH"
[[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && . "/usr/local/etc/profile.d/bash_completion.sh"
