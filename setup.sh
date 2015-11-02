# set up symbolic links to files in this repo
ln -s $(pwd)/.vimrc ~/.vimrc
ln -s $(pwd)/.vim ~/.vim
ln -s $(pwd)/.bash_profile ~/.bash_profile
ln -s $(pwd)/.tmux.conf ~/.tmux.conf

# make sure that .env_vars exists (it gets sourced by the bash profile)
touch ~/.env_vars.sh

