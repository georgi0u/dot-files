# Skip all this for non-interactive shells
[[ -z "$PS1" ]] && return

# Local ZSH file
LOCAL_OPTIONS=${HOME}"/.local_zshrc"

# Prompt Stuff
function get_production_level() { echo $PRODUCTION_DESC; }
function get_box_level_color() { echo '%F{green}' }
function get_dir_level_color() { echo "%F{6}" }

# Load Local rc file (overriding above functions)
if [[ -e $LOCAL_OPTIONS ]]; then source "$LOCAL_OPTIONS"; fi

function get_prompt() {
    username="%F{9}%n%f"
    hostname=`get_box_level_color`"%M%f"
    current_directory=`get_dir_level_color`"%~%f"
    background_jobs="%F{5}%(1j.(%j).)%f"
    echo "$username at $hostname in $current_directory $background_jobs\n$ ";
}

PROMPT=`get_prompt`

function precmd() {
    PROMPT=`get_prompt`;
}

# Set less options
if [[ -x $(which less 2> /dev/null) ]]
then
    export PAGER="less"
    export LESS="--ignore-case --LONG-PROMPT --QUIET --chop-long-lines -Sm --RAW-CONTROL-CHARS --quit-if-one-screen --no-init"
    export LESSHISTFILE='-'
fi

# Set default editor
if [[ -x $(which emacs 2> /dev/null) ]]
then
    export EDITOR="emacs"
    export USE_EDITOR=$EDITOR
    export VISUAL=$EDITOR
fi

# FAQ 3.10: Why does zsh not work in an Emacs shell mode any more?
# http://zsh.sourceforge.net/FAQ/zshfaq03.html#l26
[[ $EMACS = t ]] && unsetopt zle

# Zsh settings for history
export HISTIGNORE="&:ls:[bf]g:exit:reset:clear:cd:cd ..:cd.."
export HISTSIZE=25000
export HISTFILE=~/.zsh_history
export SAVEHIST=10000
export TERM="xterm-256color"
export GREP_OPTIONS="--color=auto"

setopt INC_APPEND_HISTORY
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_REDUCE_BLANKS
setopt HIST_VERIFY

# Say how long a command took, if it took more than 30 seconds
export REPORTTIME=3

# Zsh spelling correction options
setopt CORRECT

# Prompts for confirmation after 'rm *' etc
# Helps avoid mistakes like 'rm * o' when 'rm *.o' was intended
setopt RM_STAR_WAIT

# t write over existing files with >, use >! instead
setopt NOCLOBBER

# t nice background processes
setopt NO_BG_NICE

# Enable color support of ls
if [[ "$TERM" != "dumb" ]]; then
    if [[ -x `which dircolors 2> /dev/null` ]]; then
        eval `dircolors -b`
        alias 'ls=ls --color=auto'
    fi
fi

export TIME_STYLE="long-iso"

# Short command aliases
alias es="\emacs --daemon"
alias e="emacsclient -t -a 'emacs'"
alias eamcs="emacs"
alias emac="emacs"
alias eamc="emacs"
alias untar="tar xfz"
alias ls='ls --color=auto -X'
alias s='ls'
alias sl='ls'
alias sls='ls'
alias lsl='ls'
alias scd='cd'
alias cd..='cd ..'
alias ..="cd .."
alias la="ls --almost-all -l"
alias ll="ls -l"
alias l="ls -l"
alias tailf="tail --follow --sleep-interval=.5" 
alias wls="watch -n.2 ls"
alias ssh="ssh -q"
alias sym="symlinks -v . | sort"
alias tm="tmux -u"
alias ta="tmux -u attach -d"
alias curl="curl --silent"

alias 'mkdir=mkdir -p'
alias 'dus=du -ms * .*(N) | sort -n'
alias 'dus.=du -ms .* | sort -n'

# Quick find
f() {
    echo "find . -iname \"*$1*\""
    find . -iname "*$1*"
}

# The following lines were added by compinstall
zstyle ':completion:*' completer _expand _complete _match
zstyle ':completion:*' completions 0
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' glob 0
zstyle ':completion:*' group-name ''
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' matcher-list '+m:{a-z}={A-Z} r:|[._-]=** r:|=**' '' '' '+m:{a-z}={A-Z} r:|[._-]=** r:|=**'
zstyle ':completion:*' max-errors 1 numeric
zstyle ':completion:*' substitute 0
zstyle :compinstall filename "$HOME/.zshrc"

autoload -Uz compinit
compinit

# Completion selection by menu for kill
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*' force-list always
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'

# CD to never select parent directory
zstyle ':completion:*:cd:*' ignore-parents parent pwd

## Use cache
# Some functions, like _apt and _dpkg, are very slow. You can use a cache in
# order to proxy the list of results (like the list of available debian
# packages)
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache

# Get del key back
bindkey    "^[[3~"          delete-char
bindkey    "^[3;5~"         delete-char

# Color'd MAN output
man() {
    env \
        LESS_TERMCAP_mb=$(printf "\e[1;31m") \
        LESS_TERMCAP_md=$(printf "\e[1;31m") \
        LESS_TERMCAP_me=$(printf "\e[0m") \
        LESS_TERMCAP_se=$(printf "\e[0m") \
        LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
        LESS_TERMCAP_ue=$(printf "\e[0m") \
        LESS_TERMCAP_us=$(printf "\e[1;32m") \
        man "$@"
}

# Stop backward-word at directory delimiters
autoload -U select-word-style
select-word-style bash

clear
