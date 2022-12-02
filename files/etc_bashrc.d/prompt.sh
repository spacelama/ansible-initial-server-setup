[ -z "$NONINTERACT" -a -z "$PBS_ENVIRONMENT" ] || return 0

# the disown is so forked processes dont get a sighup -- but if you
# background a program that needs the terminal, it wont be running
# when the promt returns, so such programs will never be disowned -
# only those that are truly running at one point in time when
# backgrounded
PTS=`readlink /dev/fd/0` ; PTS="${PTS#/dev/}"
SAVED_PROMPT_COMMAND="handleprompt"
PROMPT_COMMAND="$SAVED_PROMPT_COMMAND"

#function is_on_git() {
#    timeout 1 git rev-parse 2> /dev/null
#}
#
#function parse_git_dirty() {
#    [[ $(timeout 1 git status 2> /dev/null | tail -n1) != *"working directory clean"* ]] && echo "±"   #FIXME: Don't know whether PS1 will not like the fact that "±" is two bytes, but \[ can only suppress the space of a non-printing character, not 2 characters that take up one character of screen real-estate
#}
#
#function parse_git_branch() {
#    timeout 1 git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/\1$(parse_git_dirty)/"
#}

function handleprompt () {
    retcode=$?

    # At the very first invocation for this shell, lets initialise
    # some shell stuff first
    if [ -z "$BASHRC_LAST_INVOKED" ] ; then
        # echo invoking bashrc_last
        bashrc_last
        BASHRC_LAST_INVOKED=true
    else
        statusbeep $retcode 1$SECONDS 1$CMD_START_SECONDS
    fi

    #        echo "BASH_COMMAND=$BASH_COMMAND" 1>&2
    #        case "${BASH_COMMAND%% *}" in
    #            generatetitlefromhistory|setprompt|handleprompt)
    ##                echo returning: "${BASH_COMMAND}"
    #                return
    #                ;;
    #        esac
    ##        echo not returning: "${BASH_COMMAND}"
    generatetitlefromhistory
    setprompt
    return $retcode
}

if [ "$TERM" = xterm ] ; then
    function winname () {
        if [ -t 2 ] ; then
            echo -ne  '\033]2;'"$1"'\007' 1>&2
        fi
        #            echo "resetting titleprevline"
        titleprevline=
    }
    function iconname  () {
        if [ -t 2 ] ; then
            echo -ne  '\033]1;'"$1"'\007' 1>&2
        fi
    }
else
    function winname () {
        :
    }
    function iconname () {
        :
    }
fi

if [ "$SMALLPROMPT" = yes ] ; then
    function generatetitle () {
        :
    }
    function generatetitlefromhistory () {
        # surpress recursivecalls:
        # http://unix.stackexchange.com/questions/196159/trap-to-debug-signal-was-evoked-twice-before-shell-function-executed-when-funct
        case "${BASH_COMMAND%% *}" in
            generatetitlefromhistory|setprompt|handleprompt)
                #                    echo "2: ${BASH_COMMAND}"
                return
                ;;
        esac
        #            echo "2got here: ${BASH_COMMAND}"
        writetohistory
    }
    function setprompt () {
        stty echo
        if [ -n "$SMALLPROMPTCOLOUR" ] ; then
            BG="${SMALLPROMPTCOLOUR%:*}"
            FG="${SMALLPROMPTCOLOUR#*:}"
        else
            BG=black
            FG=white # ignored for now, so can't be bothered getting it right
        fi
        [ -z "$SSH_CONNECTION" ] && background "$BG"
        disown -r -h  # mark jobs as not affected by SIGHUP of this
                      # shell (do it here so that it doesn't trigger
                      # in DEBUG trap, but does get executed when
                      # every prompt is set, giving us a chance to
                      # affect jobs just backgrounded
        if [ "$PWD" != "$HOME" ] ; then
            PS1=$(
                colorize --PS1prompt bright red -n '$?-'
                colorize --PS1prompt bright green -n '\W'
                colorize --PS1prompt bright red -n ':\t: '
               )
        else
            PS1=$(colorize --PS1prompt bright red -n '$?-\t: ')
        fi
        tmpSMALLPROMPT="$SMALLPROMPT"
        tmpSMALLPROMPTCOLOUR="$SMALLPROMPTCOLOUR"
        unset SMALLPROMPT ; unset SMALLPROMPTCOLOUR ; SMALLPROMPT="$tmpSMALLPROMPT" ; SMALLPROMPTCOLOUR="$tmpSMALLPROMPTCOLOUR"
        # redundant, you say? Hell no - there is no bash unexport
        # function that leaves the variable intact in the current
        # session, but wont export it. So we mimick it here. That way,
        # when we start an xterm as subshell to smallxterm, it
        # inherits a proper PROMPT
    }
else
    function generatetitle () {
        str="$*"
        #            echo Noopt ; sleep 0.3
        local shortpwd="${PWD/$HOME/~}"
        #echo "**** chomp --escapechars --left ... $((COLUMNS-50)) "$*""
        local cmd="`chomp --escapechars --left ... $((COLUMNS-45)) "$str"`"
        #            echo "cmd=$cmd"
        if [ -n "$EXTRA_TITLE" ] ; then
            EXTRA_TITLE="$EXTRA_TITLE -- "  # this is a local  settings - just pretty things up a little
        fi

        local me="${USER}"@"${SHORTHOST}"
        if [ $REALHOST != $LONGHOST ] ; then
            me="${USER}"@"${REALHOST}"
        fi
        winname  "$EXTRA_TITLE`date +%d/%m-%H:%M:%S` -- $PTS -- $cmd -- ${me}: `chomp --escapechars --right ... $((COLUMNS-60)) ${shortpwd}`" ;
        iconname "$EXTRA_TITLE`date +%d/%m-%H:%M:%S` -- $PTS -- $cmd -- ${me}: `chomp --escapechars --right ... 20 ${shortpwd}`"
        titleprevline="$str"
        writetohistory
    }
    function generatetitlefromhistory () {
        case "${BASH_COMMAND%% *}" in
            # there is a bug in bash exit, such that immediately upon
            # exit, it thinks BASH_COMMAND is the *last* command
            # executed, which was setting PS1.  We also try to
            # optimise not running these costly execs when we're not
            # about to exec a command at the users request.  Also
            # don't handle newlines since complicate output greatly -
            # tried to get chomp to convert newlines, but they seem to
            # be getting printed out somewhere else
            generatetitlefromhistory|setprompt|handleprompt)
                #                    echo "1: ${BASH_COMMAND}"
                return
                ;;
        esac
        #            echo "1got here: ${BASH_COMMAND}"
        # does BASH_COMMAND help me at all with setwindowtitle and history etc?   ; eg trap 'echo "ERROR: $0: $LINENO: $BASH_COMMAND"' ERR

        #            titlelastline="$BASH_COMMAND" # doesn't expand out sleep $i etc, so might as well use the full snippet:
        titlelastline=`HISTTIMEFORMAT= history 1`

        titlelastline="${titlelastline#*[0-9]  }"   # remove the number prefix since `history` can't be taught to not print a number
        generatetitle "$titlelastline"
    }
    function setprompt () {
        local shortpwd="${PWD/$HOME/~}"
        local me="${USER}"@"${SHORTHOST}"
        if [ $REALHOST != $LONGHOST ] ; then
            me="${USER}"@"${REALHOST}"
        fi
        local bold="\\[$ESC[7m\\]"
        local norm="\\[$ESC[m\\]"
        local shell=
        [ -z "$SSH_CONNECTION" ] && background black
        #            local git="$(is_on_git && [[ -n $(timeout 1 git branch 2> /dev/null) ]] && echo :)${bold}$(parse_git_branch)${norm}"
        if [ "$subshell" != "" ] ; then
            shell="$subshell"
        else
            shell="bash"
        fi
        #            echo 'setprompt ran?'
        PS1_PREV_EXIT_CODE="\`echo -n '\[$ESC' ; if [ \$retcode = 0 ]; then echo -n '[0;32m'; else echo -n '[0;31m'; fi ; echo -n '\]' ; echo -n \$retcode\`"
        PS1_PRE="#$PS1_PREV_EXIT_CODE\[$ESC[0m\]"'-\j-\t, \d '$(colorizeprompt)":\\w"
        #$git
        PS1_POST=" ($shell"')\n'"${bold}"': \!,\#;'"${norm} "
        GIT_PROMPT_START_USER="$PS1_PRE"
        GIT_PROMPT_START_ROOT="$GIT_PROMPT_START_USER"
        GIT_PROMPT_END_USER="$PS1_POST"
        GIT_PROMPT_END_ROOT="$GIT_PROMPT_END_USER"
        GIT_PROMPT_LAST_COMMAND_STATE=0  # just a dummy we don't care about
        setGitPrompt  # from ~/bash-git-prompt/
        #huh?                NO_PROMPT_VARS=1
        if [ -n "$EXTRA_TITLE" ] ; then
            EXTRA_TITLE="$EXTRA_TITLE -- "  # this is a local  settings - just pretty things up a little
        fi
        winname  "$EXTRA_TITLE`date +%d/%m-%H:%M:%S` -- $PTS -- $retcode -- ${me}: `chomp --escapechars --right ... $((COLUMNS-55)) ${shortpwd}` (bash)" ;
        iconname "$EXTRA_TITLE`date +%d/%m-%H:%M:%S` -- $PTS -- $retcode -- ${me}: `chomp --escapechars --right ... 20 ${shortpwd}` (bash)"
        #            echo have set title
    }

    titleprevline=
    ESC=`echo -ne '\033'`
    . ~/bash-git-prompt/gitprompt.sh
    PROMPT_COMMAND="$SAVED_PROMPT_COMMAND"
fi

