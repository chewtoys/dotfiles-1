Juev dotfiles
=============

Install
-------

    $ git clone git@github.com:Juev/dotfiles.git ~/.dotfiles
    $ cd ~/.dotfiles
    $ ./install.sh

This will create symlinks for config files in your home directory.

You can safely run `./install.sh` multiple times to update.

I use bash and several other files from [jfrazelle/dotfiles](https://github.com/jfrazelle/dotfiles)

oh-my-zsh
---------

    sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

Theme
=====

    git clone https://github.com/denysdovhan/spaceship-prompt.git "$ZSH_CUSTOM/themes/spaceship-prompt"
    ln -s "$ZSH_CUSTOM/themes/spaceship-prompt/spaceship.zsh-theme" "$ZSH_CUSTOM/themes/spaceship.zsh-theme"

Set `ZSH_THEME="spaceship"` in your .zshrc.

via https://github.com/denysdovhan/spaceship-prompt#oh-my-zsh
