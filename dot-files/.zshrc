# Skip all this for non-interactive shells
if [[ -z "$PS1" ]]; then
    exit;
fi

# Short command aliases
alias e="emacsclient -t -nw"
alias emacs="emacs -nw"
alias eamcs="emacs -nw"
alias emac="emacs -nw"
alias eamc="emacs -nw"
alias untar="tar xfz"
alias base_ls="ls -G --group-directories-first --sort=extension"
alias ls="base_ls"
alias s="ls"
alias sl="ls"
alias sls="ls"
alias lsl="ls"
alias la="ls -al"
alias al="ls -al"
alias ll="ls -l"
alias l="ls -l"
alias scd="cd"
alias cd..="cd .."
alias ..="cd .."
alias tailf="tail --follow --sleep-interval=.5" 
alias wls="watch -n.2 ls"
alias wll="watch -n.2 ls -l"
alias ssh="ssh -q"
alias tm="tmux -u"
alias ta="tmux -u attach -d -t _scratch || tmux -u new-session -s _scratch"
alias curl="curl --silent"

# Prompt Stuff
setopt prompt_subst
autoload -Uz vcs_info
zstyle ':vcs_info:*' actionformats '%F{155}%r%f:%F{155}%b%f %F{139}%a%f '
zstyle ':vcs_info:*' formats '%F{155}%r%f:%F{155}%b%f '
zstyle ':vcs_info:*' enable git p4 hg
function get_box_level_color() { echo '%F{220}' }
function get_dir_level_color() { echo '%F{12}' }
function get_hostname() { echo '%M' }
function get_vcs_info() { 
  vcs_info
  echo "${vcs_info_msg_0_}"
}
function get_dir_info() { echo '%d' }
function get_prompt() {
    hostname=`get_box_level_color``get_hostname`'%f'
    vcs_info=`get_vcs_info`
    current_directory=`get_dir_level_color``get_dir_info`'%f'
    prompt='%(1j.%F{5}%j.⤷)%f'

    if [ -z "$VIRTUAL_ENV" ]; then
        virtual_env=''
    else
        virtual_env='%F{1}vEnv%f '
    fi

    echo "$hostname ${virtual_env}$vcs_info%f$current_directory \n$prompt ";

    cd .
}

# Set less options
export PAGER="less"
export LESS="--ignore-case --LONG-PROMPT --QUIET -m --RAW-CONTROL-CHARS --quit-if-one-screen --no-init"
export LESSHISTFILE='-'

# Set EDITOR
export EDITOR="emacs -nw"
export ALTERNATE_EDITOR="emacs -nw"
export USE_EDITOR=$EDITOR
export VISUAL=$EDITOR

# FAQ 3.10: Why does zsh not work in an Emacs shell mode any more?
# http://zsh.sourceforge.net/FAQ/zshfaq03.html#l26
[[ $EMACS = t ]] && unsetopt zle

# Zsh settings for history
export HISTIGNORE="&:ls:[bf]g:exit:reset:clear:cd:cd ..:cd.."
export HISTSIZE=25000
export HISTFILE=~/.zsh_history
export SAVEHIST=10000
setopt histignorespace
setopt INC_APPEND_HISTORY
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_REDUCE_BLANKS
setopt HIST_VERIFY

export TERM=xterm-256color
#[ -n "$TMUX" ] && export TERM=screen-256color

# Say how long a command took, if it took more than 30 seconds
export REPORTTIME=3

# Zsh spelling correction options
setopt CORRECT

# 4Prompts for confirmation after 'rm *' etc
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
    fi
fi

export TIME_STYLE="long-iso"

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

function generate_pass(){
    < /dev/urandom tr -dc "%^&*()+=_A-Z-a-z-0-9" | head -c10; echo;
}

# turn off ctrl s/ctrl q bullshit
stty -ixon -ixoff    

autoload -U zmv

# Local ZSH file
LOCAL_OPTIONS=${HOME}"/.local_zshrc"
if [[ -e $LOCAL_OPTIONS ]]; then source "$LOCAL_OPTIONS"; fi

setopt prompt_subst
PS1=$(get_prompt)
precmd () { PS1=$(get_prompt) }
autoload -U promptinit
promptinit
