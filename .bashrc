# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth:erasedups

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE="INFINITE"
HISTFILESIZE="INFINITE"

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

[ -f ~/dotfiles/git-prompt.sh ] && . ~/dotfiles/git-prompt.sh

# Function to check if we're in a Jujutsu repo and display the branch info
jj_prompt() {
  if jj status &>/dev/null; then
    # Get branch name (or default to 'detached')
    jj_branch=$(jj log -r @ -T 'coalesce(bookmarks)' 2>/dev/null | head -n 1 | sed 's/^...//')
    [[ -z "$jj_branch" ]] && jj_branch="anonymous"

    # Get current commit hash (shortened)
    jj_commit_id=$(jj log -r @ -T "commit_id.short()" 2>/dev/null | head -n 1 | sed 's/^...//')

    # Get commit message, and skip the first 3 characters
    jj_commit_message=$(jj log -T "description" 2>/dev/null | head -n 1 | sed 's/^...//')

    # Return formatted prompt
    printf "\033[0;32m[jj: %s | %s] %s %s\033[0m" "$jj_branch" "$jj_commit_id" "$jj_commit_message"
  fi
}

# Function to determine if we should use Jujutsu or Git in the prompt
dynamic_vcs_prompt() {
  jj_output=$(jj_prompt)
  if [[ -n "$jj_output" ]]; then
    echo "$jj_output"
  else
    # If not in a JJ repo, fallback to Git
    __git_ps1 " (%s)"
  fi
}

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]$(dynamic_vcs_prompt) \n\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the ttile to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ls='ls -A --color'
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

GITHUB_USERNAME=danverbraganza

# I have tried to fork-bomb myself in the past.
ulimit -u 5000

export DEV=1
export ANDROID_HOME=/usr/local/android/sdk
export GOPATH=~/projects/go:~/projects
export PATH=$PATH:/usr/local/go/bin:$ANDROID_HOME/tools:$ANDROID_HOME/platform-tools:~/projects/go/bin:~/.local/bin
export EDITOR=emacs
IGNORED()
{
    echo IGNORED $@
}

highlight() { grep --color "$1\|\$"; }

alias node='nodejs'
#export DOCKER_HOST="unix://run/docker.sock"
alias awsume=". awsume"
alias emasc=emacs
alias danver="grep danver"
[ -f ~/.creds ] && . ~/.creds

export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWSTASHSTATE=1
export GIT_PS1_SHOWUNTRACKEDFILES=1
export GIT_PS1_SHOWUPSTREAM="auto"

# Custom AWS config file
export AWS_CONFIG_FILE=/Users/danver.branganza/github/ordering_services/build_tools/aws_configs/cloud_config

# Setup fzf
# ---------
if [[ ! "$PATH" == */usr/local/opt/fzf/bin* ]]; then
  export PATH="$PATH:/usr/local/opt/fzf/bin"
fi
# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/usr/local/opt/fzf/shell/completion.bash" 2> /dev/null
# Key bindings
# ------------
source "/usr/share/doc/fzf/examples/key-bindings.bash" 2> /dev/null # Linux
source "/opt/homebrew/Cellar/fzf/*/shell/completion.bash" 2> /dev/null # Mac

if [ -f ~/.git-completion.bash ]; then
  . ~/.git-completion.bash
fi

[ -f ~/.ziprc ] && . ~/.ziprc

# FZF shell bindings
eval "$(fzf --bash)"

# RUN ONCE on INSTALL
# git config --global core.excludesfile ~/.gitignore

# Mac homebrew
[ -x /opt/homebrew/bin/brew ] && eval "$(/opt/homebrew/bin/brew shellenv)"
# Custom AWS config file
export AWS_CONFIG_FILE=/Users/danver.branganza/github/ordering_services/build_tools/aws_configs/cloud_config

# Start the SSH agent if not running
if ! pgrep -u "$USER" ssh-agent > /dev/null; then
    eval "$(ssh-agent -s)"
fi

echo "" | setsid ssh-add >/dev/null 2>&1
[ -f personal_github_ed25519 ] && ssh-add ~/.ssh/personal_github_ed25519

. "$HOME/.cargo/env"
eval "$(uv generate-shell-completion bash)"

if command -v ngrok &>/dev/null; then
	eval "$(ngrok completion)"
fi

if command -v direnv &>/dev/null; then
	eval "$(direnv hook bash)"
fi

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm

# This is temporarily added for the moment. Consider deleting it perchance
# should your eye fall upon it, o sire.
if command -v nvm &>/dev/null; then
	nvm alias default 20.13.1
	nvm use
fi

# For interactive shells, trap SIGINT to avoid shell exit
# Only trap on MacOs
if [[ "$OSTYPE" == "darwin"* ]]; then
	if [[ $- == *i* ]]; then
		trap 'echo' SIGINT
	fi
fi

alias vogin="vault login -method oidc"

alias jjdirty='jj diff -f main --to @ | grep "regular file" | grep -v "^Removed regular file" | cut -d" " -f4 | tr ":" " "  | grep -e "\.py"'
