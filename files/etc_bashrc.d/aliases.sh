[ -z "$NONINTERACT" -a -z "$PBS_ENVIRONMENT" ] || return 0

if ls --version 2>/dev/null | grep -E 'coreutils|fileutils' >/dev/null ; then
    export enablecolor="--color=auto"   #now enabled for everything, since darwin now has gnu fileutils
fi
export forceenablecolor="--color"

alias ncal='ncal -c'
alias cal='cal -c'
alias cal3='ncal -3'
alias read='read -e' # use readline when interactive anyway
alias which=: ; unalias which   #you can't unalias something that isn't aliased, and only some systems are stupid enough to alias which
alias slrn='slrn --kill-log $HOME/News/.kill-log'
alias ouci=pico   #so sue me
#    alias pico=nano # ~/bin/pico is a script for emacsclient -nw these days
alias grep='grep $enablecolor'
alias ip='ip -color=auto'
alias pin='echo "did you want pine?"'
alias ls-tr='ls -lAtr $enablecolor'
alias lsl='ls -lAL'
alias d='ls -F $forceenablecolor'
alias l.='ls -dl .[a-zA-Z]*'
alias lc='ls -lA $forceenablecolor'
alias l='ls -lA'
alias l-tr='ls -lAtr'
alias lc-tr='ls -lAtr $forceenablecolor'
alias ll='ls -lA'
alias ls='ls $enablecolor'
alias l-ft='ls -lA --time-style=full-iso'
alias l-lt='ls -lA --time-style=long-iso'
alias pavucontrol='DBUS_SESSION_BUS_ADDRESS= pavucontrol'  # https://gitlab.freedesktop.org/pulseaudio/pavucontrol/-/issues/75#note_1467143
alias bc='bc -l'
alias -- +='pushd .'
alias -- -='popd'
alias bd='popd'
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ......='cd ../../../../..'
alias rehash='hash -r'
alias crontab='crontab -i'
alias ecrontab='crontab -e'
alias info=pinfo

#safeify commands
#    alias rm='echo rm override deprecated 2>&1 ; command rm -i'
#    alias cp='echo cp override deprecated 2>&1 ; command cp -i'
#    alias mv='echo mv override deprecated 2>&1 ; command mv -i'
alias rm='echo rm override obsoleted. Use \"rmi\" ; beep ; sleep 1 ; beep ; false'
alias cp='echo cp override obsoleted. Use \"cpi\" ; beep ; sleep 1 ; beep ; false'
alias mv='echo mv override obsoleted. Use \"mvi\" ; beep ; sleep 1 ; beep ; false'
#    alias rmi='command rm -i'
#    alias cpi='command cp -i'
#    alias mvi='command mv -i'

#some functions that can't be done by aliases

# http://wiki.bash-hackers.org/commands/builtin/caller
function bt() {
    local frame=0
    while caller $frame; do
        ((frame++));
    done
    echo "$*"
    #  exit 1
}

# puppet functionality
function validate_yaml { ruby -ryaml -e "YAML.load_file '$1'" ; }
function validate_erb { erb -P -x -T '-' $1 | ruby -c ; }

function l- () {
    command ls -lA $enablecolor -"$@"
}
function ls- () {
    command ls $enablecolor -"$@"
}

function cd() {
    if [ "$#" = 0 ] ; then
        pushd $HOME
    elif [ "$1" = - ] ; then
        pushd
    else
        pushd "$@"
    fi > /dev/null
}
function fg () {
    if [ x"$*" != x ] ; then
        generatetitle `jobs "$@"`
    else
        generatetitle `jobs | grep '^\[[0-9]\+\]+'`
    fi
    command fg "$@"
}
