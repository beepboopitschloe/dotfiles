# set up symbolic links to files in this repo
ln -nfs $(pwd)/.vim ~/.vim
ln -nfs $(pwd)/.vimrc ~/.vimrc
mkdir -p ~/.config
ln -nfs $(pwd)/.vim ~/.config/nvim
ln -nfs $(pwd)/.vimrc ~/.config/nvim/init.vim
ln -nfs $(pwd)/.bash_profile ~/.bash_profile
ln -nfs $(pwd)/.tmux.conf ~/.tmux.conf
ln -nfs $(pwd)/.spacemacs ~/.spacemacs
ln -nfs $(pwd)/.emacs.d ~/.emacs.d
ln -nfs $(pwd)/layers ~/.emacs.d/private
ln -nfs $(pwd)/.hyper.js ~/.hyper.js

# make sure that .env_vars exists (it gets sourced by the bash profile)
touch ~/.env_vars.sh

