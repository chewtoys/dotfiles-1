# -*- mode: sh -*-
if [[ -f $HOME/.bashrc ]]; then
	source $HOME/.bashrc
fi

# added by Anaconda2 4.2.0 installer
export PATH="/Users/juev/anaconda/bin:$PATH"
export PATH="$PATH:/usr/local/opt/go/libexec/bin"

if [ -e /Users/juev/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/juev/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
