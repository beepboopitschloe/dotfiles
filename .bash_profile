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

# source environment vars
source ~/.env_vars.sh

# pretty command prompt
export PS1="$COLOR_GREEN\u@\h: $COLOR_BLUE\W$COLOR_NC\$ "

# folders
export PRET=~/Projects

##################
# define functions

# redo the last command as sudo
alias fuck='sudo $(history -p !!)'

# function to count lines of code in a directory, excluding node_modules or
# public/libs
function sloc() {
	find . -path ./node_modules -prune -o \
			-path ./public/lib -prune -o \
			-path ./public/libs -prune -o \
			-path ./*/lib -prune -o \
			-path ./*/dist -prune -o \
			-name '*.js' -o \
			-name '*.scss' -o \
			-name '*.less' -o \
			-name '*.c' -o \
			-name '*.h' \
		| xargs wc -l
}

# friendly message
echo "Shiny. Let's be bad guys."


source /Users/noah.muth/.iterm2_shell_integration.bash

export NVM_DIR="/Users/noah.muth/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
