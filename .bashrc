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
    PS1="$box_color\[\033[04m\]\u@\h\[\033[0m\] \[\e[1;35m\]jobs:\j $dir_color[\w]\n\[\033[0m\]$ ";
}

function cd() {
    builtin cd $@;
    set_prompt;
}

function ta() {
    tmux -u attach -d || tmux
}

if [ "$PS1" ]; then
    set_prompt;
    stty -ixon -ixoff    
    shopt -s checkwinsize

    export HISTFILESIZE=100000;
    export EDITOR="emacs"
    export VISUAL="emacs"
    export TERM="xterm-256color"
    export GREP_OPTIONS="--color=auto"

    alias eamcs="emacs"
    alias emac="emacs"
    alias eamc="emacs"
    alias untar="tar xfz"
    alias ls='ls --color=auto -X'
    alias s='ls'
    alias sl='ls'
    alias sls='ls'
    alias lsl='ls'
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

    if [[ -z $TMUX ]]; then
        ta;
    fi

    [ -e "$HOME/.dircolors" ] && DIR_COLORS="$HOME/.dircolors";
    [ -e "$DIR_COLORS" ] || DIR_COLORS="";
    eval "`dircolors -b $DIR_COLORS`";
fi
