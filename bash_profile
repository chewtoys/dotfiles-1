# -*- mode: sh -*-
if [[ -f $HOME/.bashrc ]]; then
	source $HOME/.bashrc
fi

[[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && . "/usr/local/etc/profile.d/bash_completion.sh"
export PATH="/usr/local/sbin:$PATH"
