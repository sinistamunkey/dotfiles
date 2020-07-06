# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

export ZSH="${HOME}/.oh-my-zsh"
ZSH_THEME="powerlevel10k/powerlevel10k"

# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git dotenv)

source $ZSH/oh-my-zsh.sh
export WORKON_HOME=~/.local/share/virtualenvs
export GOPATH=/Volumes/Development/go
export GO111MODULE=on
export ZSH_DOTENV_FILE=.envrc
PATH=$PATH:$GOPATH/bin
export NVM_DIR="$HOME/.nvm"
alias spip='source $(pipenv --venv)/bin/activate'
[ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
[ -s "/usr/local/opt/nvm/etc/bash_completion" ] && . "/usr/local/opt/nvm/etc/bash_completion"  # This loads nvm bash_completion

alias uuid='uuid=$(python -c "import uuid;print(uuid.uuid4())");echo $uuid | pbcopy;echo "Copied $uuid"'
alias gbpurge='git branch --merged | grep -v "\*" | xargs -n 1 git branch -d'
alias cdgo="pushd ${GOPATH}/src/github.com"
alias cdev="pushd /Volumes/Development"
alias dockerkill="docker ps -aq | xargs docker stop && docker container prune -f"

# Load pyenv automatically by appending
# the following to ~/.zshrc:

eval "$(pyenv init -)"

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# Load any secret variables
[[ -f ~/.env ]] && source ~/.env
