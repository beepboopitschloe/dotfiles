# pretty colors
export COLOR_NC='\[\e[0m\]' # No Color
export COLOR_WHITE='\[\e[1;37m\]'
export COLOR_BLACK='\[\e[0;30m\]'
export COLOR_BLUE='\[\e[0;34m\]'
export COLOR_LIGHT_BLUE='\[\e[1;34m\]'
export COLOR_GREEN='\[\e[0;32m\]'
export COLOR_LIGHT_GREEN='\[\e[1;32m\]'
export COLOR_CYAN='\[\e[0;36m\]'
export COLOR_LIGHT_CYAN='\[\e[1;36m\]'
export COLOR_RED='\[\e[0;31m\]'
export COLOR_LIGHT_RED='\[\e[1;31m\]'
export COLOR_PURPLE='\[\e[0;35m\]'
export COLOR_LIGHT_PURPLE='\[\e[1;35m\]'
export COLOR_BROWN='\[\e[0;33m\]'
export COLOR_YELLOW='\[\e[1;33m\]'
export COLOR_GRAY='\[\e[0;30m\]'
export COLOR_LIGHT_GRAY='\[\e[0;37m\]'

# what terminal emulator are we in?
term_is_iterm2=$([[ $TERM_PROGRAM = "iTerm.app" ]])
term_is_emacs=$([[ $TERM = "eterm-color" ]])

# alias to reload the bash profile
alias reload="source ~/.bash_profile"

# source environment vars
source ~/.env_vars.sh

# pretty command prompt
export PS1="$COLOR_GREEN\u@\h: $COLOR_BLUE\W$COLOR_NC\$ "

# folders
export PRET=~/Projects

# source git-completion
source $PRET/dotfiles/git-completion.bash

##################
# define functions

# redo the last command as sudo
alias fuck='sudo $(history -p !!)'

# function to count lines of code in a directory, excluding node_modules or
# public/libs
function sloc() {
	find . -path ./node_modules -prune -o \
			-path ./bower_components -prune -o \
			-path ./public/lib -prune -o \
			-path ./public/libs -prune -o \
			-path ./*/lib -prune -o \
			-path ./*/dist -prune -o \
			-path ./*/build -prune -o \
			-name '*.js' -o \
			-name '*.scss' -o \
			-name '*.less' -o \
			-name '*.c' -o \
			-name '*.h' -o \
			-name '*.rb' -o \
			-name '*.lisp' \
		| xargs wc -l
}

function pfctl_block_rule() {
	echo "block drop quick on lo0 proto tcp from any to any port = $1"
}

# block a port
function pblock() {
	port=$1
	if [ -z "$port" ]; then
		echo "Usage: pblock <port>"
		return 1;
	fi

	(sudo pfctl -sr 2>/dev/null; echo "$(pfctl_block_rule $port)") | sudo pfctl -e -f -
	echo $(sudo pfctl -sr)
}

# unblock a port
function punblock() {
	port=$1

	if [ -z "$port" ]; then
		echo "Usage: punblock <port>"
		return 1;
	fi

	(sudo pfctl -sr 2>/dev/null | fgrep -v "$(pfctl_block_rule $port)") | sudo pfctl -e -f -
	echo $(sudo pfctl -sr)
}

# function to make a notification on OS X
function notify() {
	title=$1
	msg=$2

	osascript -e "display notification \"$msg\" with title \"$title\""
}

# which process is using port $1?
function who_is_using() {
	lsof -n -i4TCP:$1 | grep LISTEN
}

# friendly message
echo "Shiny. Let's be bad guys."

if [ $term_is_iterm2 ]; then
  source $HOME/.iterm2_shell_integration.bash
fi

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm

