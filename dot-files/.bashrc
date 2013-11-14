# Skip all this for non-interactive shells
[[ -z "$PS1" ]] && return

export PATH="/home/user/ageorgiou/.my_software/bin":$PATH

if [[ -x `which --skip-alias zsh 2>/dev/null` ]]; then
    exec zsh;
    exit 0;
fi

LOCAL_OPTIONS=${HOME}"/.local_bashrc"

function add_to_path () {
    [ -e $1 ] && [[ "${PATH}" =~ ":${1}:" ]] || export PATH=$PATH:$1;
}

function prepend_to_path() {
   [ -e $1 ] && [[ "${PATH}" =~ ":${1}:" ]] || export PATH=$1:$PATH;
}

function add_to_lib_path () {
    [ -e $1 ] && [[ "${LD_LIBRARY_PATH}" =~ ":${1}:" ]] || export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$1;
}

function add_to_python_path () {
    [ -e $1 ] && [[ "${PYTHONPATH}" =~ ":${1}:" ]] || export PYTHONPATH=$PYTHONPATH:$1;
}

function get_box_level_color() { echo "\[\e[1;32m\]"; }
function get_dir_level_color() { echo "\[\e[0;36m\]"; }

if [[ -e $LOCAL_OPTIONS ]]; then source "$LOCAL_OPTIONS"; fi

function set_prompt() {
    box_color=`get_box_level_color`;
    dir_color=`get_dir_level_color`;
    PS1="\[\033[0m\](\T)\n$box_color\[\033[04m\]\u@\h\[\033[0m\] \[\e[1;35m\]jobs: \j $dir_color[\w]\n\[\e[1;37m\]>> \[\e[0;37m\]";
}

function cd() {
    builtin cd $@;
    set_prompt;
}

function ta() {
    tmux -u attach -d
}

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

if [ "$PS1" ]; then
    set_prompt;
    stty -ixon -ixoff    
    shopt -s checkwinsize

    export HISTFILESIZE=100000;
    export EDITOR="emacs -nw"
    export VISUAL="emacs -nw"
    export TERM="xterm-256color"
    [ -n "$TMUX" ] && export TERM=screen-256color
    export GREP_OPTIONS="--color=auto"

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
    alias cat*="head -n -1 *"
    alias tm="tmux -u"
    alias curl="curl --silent"

    [ -e "$HOME/.dircolors" ] && DIR_COLORS="$HOME/.dircolors";
    [ -e "$DIR_COLORS" ] || DIR_COLORS="";
    eval "`dircolors -b $DIR_COLORS`";    
fi

