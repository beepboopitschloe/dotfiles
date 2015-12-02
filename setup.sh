# set up symbolic links to files in this repo
ln -s $(pwd)/.vim ~/.vim
ln -s $(pwd)/.vimrc ~/.vimrc
mkdir ~/.config
ln -s $(pwd)/.vim ~/.config/nvim
ln -s $(pwd)/.vimrc ~/.config/nvim/init.vim
ln -s $(pwd)/.bash_profile ~/.bash_profile

# make sure that .env_vars exists (it gets sourced by the bash profile)
touch ~/.env_vars.sh

