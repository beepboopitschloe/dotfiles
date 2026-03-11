source ~/.env_vars.sh

path+=("$HOME/Projects/dotfiles/bin")
export PATH

function activate_poetry_env_if_present {
  local poetry_dir="$PWD"

  # If inside a .worktrees/ directory, use the root repo's poetry env
  if [[ "$PWD" == */.worktrees/* ]]; then
    poetry_dir="${PWD%%/.worktrees/*}"
  fi

  if [[ -f "$poetry_dir/poetry.lock" ]]; then
    $(poetry --directory "$poetry_dir" env activate)
  fi
}

# load ssh key into agent if it's not already there
function ssh-add-assert-key {
    key_name=${1:?"a path to a key must be specified"}
    pub=$(cat "${key_name}.pub")
    loaded=$(ssh-add -L | grep "$pub")

    if [[ -z "$loaded" ]]; then
        ssh-add $key_name
    fi
}

function reload() {
  source ~/.zshrc
}

autoload -Uz add-zsh-hook
add-zsh-hook chpwd activate_poetry_env_if_present

activate_poetry_env_if_present

if [[ -f ~/.ssh/id_rsa ]]; then
    ssh-add-assert-key ~/.ssh/id_rsa
fi


# bun completions
[ -s "/Users/rose/.bun/_bun" ] && source "/Users/rose/.bun/_bun"

export EDITOR=hx

# set SSH_AUTH_SOCK env var to a fixed value
export SSH_AUTH_SOCK=~/.ssh/ssh-agent.sock

# test whether $SSH_AUTH_SOCK is valid
ssh-add -l 2>/dev/null >/dev/null

# if not valid, then start ssh-agent using $SSH_AUTH_SOCK
[ $? -ge 2 ] && ssh-agent -a "$SSH_AUTH_SOCK" >/dev/null
