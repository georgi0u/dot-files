export HISTFILESIZE=100000;
FDS_ENV=1;
CUSTOM_ALIASES=1;

PRODUCTION_DESC=$(ls /etc/fds | egrep "(^dev$)|(^stage$)|(^prod$)")

function get_box_level_color() {
    if [ "$PRODUCTION_DESC" = "dev" ]; then
        echo '\[\e[1;32m\]'
    elif [ "$PRODUCTION_DESC" = "stage" ]; then
        echo '\[\e[1;33m\]'
    elif [ "$PRODUCTION_DESC" = "prod" ]; then
	    echo '\[\e[7;31m\]'
    else
	    echo '\[\e[1;37m\]';
    fi
}

function get_dir_level_color() {
    this_dir=`pwd`;
    if [[ "$this_dir" =~ "/home/data/index" ||  "$this_dir" =~ "/home/docs" ]]; then
        echo '\[\e[0;31m\]';
    elif [[ "$this_dir" =~ "/home/staging" ]]; then
        echo '\[\e[1;33m\]';
    else
        echo "\e[0;36m\]";
    fi
}

function set_prompt() {
    box_color=`get_box_level_color`;
    dir_color=`get_dir_level_color`;
	PS1="$box_color\[\033[04m\]\u@\h\[\033[0m\] \[\e[1;35m\]jobs:\j $dir_color[\w]\n\[\033[0m\]$ ";
}

function cd()
{
    builtin cd $@
    set_prompt
}

function add_to_path () {
    [ -e $1 ] && [[ "${PATH}" =~ ":${1}:" ]] || export PATH=$PATH:$1;
}

function add_to_lib_path () {
    [ -e $1 ] && [[ "${LD_LIBRARY_PATH}" =~ ":${1}:" ]] || export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$1;
}

function add_to_python_path () {
    [ -e $1 ] && [[ "${PYTHONPATH}" =~ ":${1}:" ]] || export PYTHONPATH=$PYTHONPATH:$1;
}

function grepsource ()
{
    find . \( -name "*.cxx" -o -name "*.h" -o -name "*.py" \) -exec grep -H -i "${1}" {} \;
}

EID_UID_MAP=($(echo {0..9} {A..Z}|tr -d AEIOU));
function uid_to_eid ()
{
    U=${1:0:6};
    E=0;
    for i in {0..5};
    do
        j=$(expr index $(echo ${EID_UID_MAP[@]} | tr -d " ") ${U:i:1});
        j=$(expr $j - 1);
        E=$(echo "$E + (31^(5-$i))*$j" | bc);
    done;
    echo $E
}

function eid_to_uid ()
{
    E=$1;
    for i in {5..0};
    do
        tE=$(echo "$E/(31^$i)"|bc);
        E=$(echo "$E-$tE*31^$i"|bc);
        printf ${EID_UID_MAP[$tE]};
    done;
    echo -e '\055E'
}

function p4start () {
    if [[ ${1} == "" ]]; then
        echo "Name the project...";
        return;
    elif [ -d ${1} ]; then
        echo "Dir already exists";
        return;
    fi

    mkdir ${1};
    cd ${1};
    echo "P4CLIENT=ageorgiou__${1}" > .p4rc;
    p4 client;
    p4 sync;
}


function ver() {
    default_user="profiler";

    dir="/home/fds/";
    if [[ ${1} != "" ]]; then
        dir=$dir${1};
    elif [[ ${default_user} != "" ]]; then
        dir=$dir$default_user
    else
        echo "usage: dir <fds user>"
        return;
    fi

    if [[ ! -d $dir ]]; then
        echo "$dir doesn't exist"
        return;
    fi

    symlinks -v $dir | cut -d' ' -f4 | sort
}

if [ "$FDS_ENV" ]; then
    add_to_path /home/user/ageorgiou/.my_software/bin/
fi

# If running interactively, then:
if [ "$PS1" ]; then

    export EDITOR="emacs-24.1 -nw"
    export VISUAL="emacs-24.1 -nw"
    export TERM="xterm-256color"

    stty -ixon -ixoff
    
    if [ "$CUSTOM_ALIASES" ]; then         
	    alias emacs='/home/user/ageorgiou/.my_software/bin/emacsclient -t -a ""'
        alias eamcs="emacs"
        alias emac="emacs"
        alias eamc="emacs"
        alias untar="tar xfz"
	    alias ls='ls --color=auto'
	    alias s='ls'
	    alias sl='ls'
	    alias sls='ls'
	    alias lsl='ls'
	    alias cd..='cd ..'
        alias ..="cd .."
        alias la="ls -al"
        alias ll="ls -l"
        alias l="ls -l"
        alias grep="grep --color=auto"
        alias tailf="tail -f"        
        alias wls="watch -n.2 ls"
        alias tm="(tmux && exit)"
        alias ta="(tmux attach -d && exit)"
        alias sym="symlinks -v . | sort"
        alias !="echo -e \"DON'T USE !\""
	    alarm() { perl -e 'alarm shift; exec @ARGV' "$@"; }
    fi
        
    shopt -s checkwinsize
fi

#Is this a FactSet environment?
if [ "$FDS_ENV" ]; then

    add_to_path /sbin
    add_to_path /usr/sbin
    add_to_path /usr/kerberos/bin
    add_to_path /home/bin/
    add_to_path /home/user/ageorgiou/.my_software/scripts/
    add_to_path /home/user/ageorgiou/.my_software/scripts/cortex
    add_to_path /home/fds/mds_web/FDSmds_django/django/bin/
    add_to_python_path /home/fds/mds_web/FDSmds_django/
    add_to_python_path /home/user/ageorgiou/.my_software/lib/python2.6/site-packages/
    add_to_python_path /home/user/ageorgiou/.my_software/lib/python
    add_to_path /home/data/index/script/
    add_to_path /export/fds/Linux_RHEL5_x86_64/mds_news/FDSnewssqlutils_1.2.19/sms_mysql/bin/
    
    export FTOK_FILE_PATH=/home/data/idc/
    add_to_path /home/dev/build/bin
    add_to_path /home/dev/build/pub/bin
    add_to_path /home/fds/procman/FDSprocman/bin

    #Perforce (p4) environment variables
    export P4PORT="scm.factset.com:1666"
    export P4CONFIG=".p4rc"
    export P4USER=$USER
    export P4EDITOR="~/.my_software/bin/emacs-24.1"

    if [ "$CUSTOM_ALIASES" ]; then
        alias diff="colordiff"

        #Profiler Stuff
        alias sp="sudo -u profiler"
        alias sw="sudo -u news_web"
        
        # Misc.
	    alias ssh="ssh -q"	   
        alias mypman="/home/fds/procman/FDSprocman/bin/procman -p /home/user/ageorgiou/.procman/config"
	    alias rb="perl /home/data/index/script/common/submit_review_board.pl"
	    alias submit_change="/home/data/index/script/common/submit_change.py"
        alias pub="/home/dev/build/pub/bin/pub --parallel=20"
        alias clang='USE_LLVM="true" /home/dev/build/pub/bin/pub --parallel=20'

        alias python="/home/fds/freeware/python26/bin/python"
        alias screen="/home/user/ageorgiou/.my_software/bin/screen"

        alias pls="p4 opened ..."

        if [[ "$PRODUCTION_DESC" == "prod" || "$PRODUCTION_DESC" == "stage" ]]; then
            alias cdp="cd ~profiler"
        fi

        if [[ "$PRODUCTION_DESC" == "prod" ]]; then
            unalias mypman
        fi

        # Diff everything at this level of the client and up, ignoring 
        # whitespace, and adding color. 
        #
        # Assumes diff is set to GNU Diff, and that colordiff is installed.
        alias p4d="p4 diff -d-ignore-all-space ./... | colordiff"
        alias ctailf="ctail -f"

        alias fdsrpm="fdsrpm --norepackage"

        alias chg="p4 opened | grep -Po \"(change \d+|default change)\" | sort | uniq | grep -Po \"(\d+|default)\""
    fi
    
    if [ "$PS1" ]; then
	    set_prompt;
    fi
    
    have () { type -P $1 > /dev/null; }
    
    source ~/.bash_completion.d/procman
    source ~/.bash_completion.d/mypman
    source ~/.bash_completion.d/sudo    
    
fi


if [ "$PS1" ]; then
    thumbs_up_path="/home/user/jpizzini/thumbsup.ascii";

    if [ -e $thumbs_up_path ]; then
        cat $thumbs_up_path;
    fi

    if [ "$FDS_ENV" ]; then
	    echo -e ${LIGHTCYAN}`cat /etc/redhat-release` ;
    fi
    
    echo -e "Kernel Information: " `uname -smr`;
    echo -ne "${WHITE}";uptime | sed -n 's/.* up \(.*\)/\1/p'
fi
