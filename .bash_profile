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

# pretty command prompt
export PS1="$(date '+%H:%M:%S') $COLOR_GREEN\u@\h: $COLOR_BLUE\W$COLOR_NC\$ "

# folders
export PRET=~/Projects

# source git-completion
source $PRET/dotfiles/git-completion.bash

# source local configuration
source ~/.env_vars.sh

##################
# define functions

# edit a file
alias e="$EDITOR"

# redo the last command as sudo
alias fuck='sudo $(history -p !!)'

# kubectl aliases
alias kc=kubectl

# function to count lines of code in a directory, excluding node_modules or
# public/libs
function m_sloc() {
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

# pretty-print json output
alias json='python -m json.tool'

alias alert='say -v victoria for the honor of greyskull'

# curl shortcuts
function curl-get() {
    curl -X GET -H "Content-Type: application/json" $@
}

function curl-post() {
    curl -X POST -H "Content-Type: application/json" $@
}

function curl-put() {
    curl -X PUT -H "Content-Type: application/json" $@
}

function curl-del() {
    curl -X DELETE -H "Content-Type: application/json" $@
}

# which process is using port $1?
function who_is_using() {
    lsof -n -i4TCP:$1 | grep LISTEN
}

# generate & open mermaidjs diagrams
function mermaid() {
    file=$(mktemp "$TMPDIR/$(uuidgen).png")
    mmdc -i $1 -o $file
    open $file
}

# do a thing if the last command was successful
function and() {
    if [ $? -eq 0 ]; then
        $@
    else
        return 1
    fi
}

# print whether the previous command was successful
function ok? {
    if [ $? -eq 0 ]; then
        echo '🆒👀🆗'
    else
        echo '🚫🙅🚫'
        false
    fi
}

alias vihosts='sudo vim /etc/hosts'

# load ssh key into agent if it's not already there
function ssh-add-assert-key() {
    path=$1
    pub=$(cat "${path}.pub")
    loaded=$(ssh-add -L | grep "$pub")

    if [[ -z "$loaded" ]]; then
        ssh-add $path
    fi
}
# friendly message
echo "MAIN SCREEN TURN ON"

if [[ -f ~/.ssh/id_rsa ]]; then
    ssh-add-assert-key ~/.ssh/id_rsa
fi

if [ $term_is_iterm2 ]; then
    source $HOME/.iterm2_shell_integration.bash
fi

PATH="/usr/local/bin:$PATH"

export NVM_DIR="$HOME/.nvm"
. "$(brew --prefix nvm)/nvm.sh"

function org_push {
    echo '(cd ~/org && git add -A && git commit -m "updated $(time)" && git push)'
}

function org_fetch {
    (cd ~/org && git pull)
}

###-tns-completion-start-###
if [ -f /Users/noah.muth/.tnsrc ]; then
    source /Users/noah.muth/.tnsrc
fi
###-tns-completion-end-###

export PATH="$HOME/.cargo/bin:$PATH"

# Setting PATH for Python 3.9
# The original version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.9/bin:${PATH}"
export PATH
