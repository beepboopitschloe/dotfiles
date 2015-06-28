# pretty colors
export COLOR_NC='\e[0m' # No Color
export COLOR_WHITE='\e[1;37m'
export COLOR_BLACK='\e[0;30m'
export COLOR_BLUE='\e[0;34m'
export COLOR_LIGHT_BLUE='\e[1;34m'
export COLOR_GREEN='\e[0;32m'
export COLOR_LIGHT_GREEN='\e[1;32m'
export COLOR_CYAN='\e[0;36m'
export COLOR_LIGHT_CYAN='\e[1;36m'
export COLOR_RED='\e[0;31m'
export COLOR_LIGHT_RED='\e[1;31m'
export COLOR_PURPLE='\e[0;35m'
export COLOR_LIGHT_PURPLE='\e[1;35m'
export COLOR_BROWN='\e[0;33m'
export COLOR_YELLOW='\e[1;33m'
export COLOR_GRAY='\e[0;30m'
export COLOR_LIGHT_GRAY='\e[0;37m'

# pretty command prompt
export PS1="$COLOR_GREEN\u@\h: $COLOR_BLUE\W$COLOR_NC\$ "

# folders
export PRET=~/Projects

# ant options from https://wiki.corp.code42.com/index.php/Development_Quickstart
export ANT_OPTS="-Xmx1024m -Xms512m"

# redo the last command as sudo
alias fuck='sudo $(history -p !!)'

# friendly message
echo "Shiny. Let's be bad guys."

