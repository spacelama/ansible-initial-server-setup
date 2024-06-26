# the disown is so forked processes dont get a sighup -- but if you
# background a program that needs the terminal, it wont be running
# when the promt returns, so such programs will never be disowned -
# only those that are truly running at one point in time when
# backgrounded
[ -z "$NONINTERACT" ] && PROMPT_COMMAND=handleprompt # likely to get overriden by git plugins later on

# FIXME: Fixing PROMPT_COMMAND trap that writes command history before
# command is invoked notes:
#
# Another problem with TRAP debug method of writing .bash_fullhistory
# is that if a command doesn't actually run (eg, #blah), then it
# doesn't get echoed yet to the history file.  Probably need to
# combine it with PROMPT_COMMAND, in order to capture any history that
# didn't get caught.  Problem with PROMPT_COMMAND by itself is that it
# doesn't write to the history until the command returns.  OTOH, right
# now, PROMPT_COMMAND *is* set on weinberg, and yet it doesn't seem to
# result in the writing of history..... ; want something like
# bash-preexec: https://github.com/rcaloras/bash-preexec
# https://superuser.com/questions/175799/does-bash-have-a-hook-that-is-run-before-executing-a-command
#
# rewrite writetohistory so it doesn't trigger upon a #!/bin/bash -li
# script (less important now that worked out -i is not needed for most
# environment imports)
#
# sometimes, writetohistory is deleting the last history entry which
# deletes the entire occurences of it when its not duplicated? ;;
# suspect I've seen it when certain conditions are triggered on the
# first command in a shell, possibly a compound command, and when we
# were still using the old DEBUG trap rather than PS0

# https://stackoverflow.com/questions/2575037/how-to-get-the-cursor-position-in-bash
# https://stackoverflow.com/questions/62038398/bash-clearing-the-last-output-correctly/62040654#62040654
# FIXME: oh, turns out zsh does this by default, and you can use that
# to search better for people who dunnit before (and who fixed other
# problems such as clearing in the input buffer):
# https://stackoverflow.com/questions/19943482/configure-shell-to-always-print-prompt-on-new-line-like-zsh
function fetch_cursor_position() {
    local getCpos
    local oldstty
    local CURPOS

    getCpos=$(tput u7)
    oldstty=$(stty -g)
    stty raw -echo min 0
#    set -xv
    echo -en "$getCpos"
    read -sdR CURPOS
    stty $oldstty
    IFS=\; read row column <<<"${CURPOS#$'\e['}"
}

# calling fetch_cursor_position empties the tty buffer, which is
# annoying when you've aleady partially typed out the next command, so
# use this instead
function print_to_eol() {
    local extra_spaces
    printf -v extra_spaces "%$((COLUMNS-1))s"
    colorize --nolf --background white black " "
    echo -n "$extra_spaces"
    echo -ne '\r'
}

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
    generatetitlefromhistory
    setprompt
    return $retcode
}

if [ "$TERM" = xterm ] ; then
    function winname () {
        if [ -t 2 ] ; then
            echo -ne  '\033]2;'"$1"'\007' 1>&2
        fi
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
    function finalise_prompt() {
        # ESC=`echo -ne '\033'`
        # . ~/bash-git-prompt/gitprompt.sh
        [ -z "$NONINTERACT" ] && PROMPT_COMMAND=handleprompt
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

        # move the cursor back to a newline if we're not at the start
        # of a line when we start outputting the prompt
        #fetch_cursor_position
        #if [ $column != 1 ] ; then
        #    colorize --background white black ' ' # %
        #fi

        print_to_eol

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
        [ -z "$IGNORE_GIT_PROMPT" ] && setGitPrompt  # from ~/bash-git-prompt/
        #huh?                NO_PROMPT_VARS=1
        if [ -n "$EXTRA_TITLE" ] ; then
            EXTRA_TITLE="$EXTRA_TITLE -- "  # this is a local  settings - just pretty things up a little
        fi
        winname  "$EXTRA_TITLE`date +%d/%m-%H:%M:%S` -- $PTS -- $retcode -- ${me}: `chomp --escapechars --right ... $((COLUMNS-55)) ${shortpwd}` (bash)" ;
        iconname "$EXTRA_TITLE`date +%d/%m-%H:%M:%S` -- $PTS -- $retcode -- ${me}: `chomp --escapechars --right ... 20 ${shortpwd}` (bash)"
        #            echo have set title
    }

    function finalise_prompt() {
        if [ -z "$GIT_PROMPT_HAS_RUN" ] ; then
            ESC=`echo -ne '\033'`
	    if [ -e /etc/bash-git-prompt/gitprompt.sh ] ; then
		. /etc/bash-git-prompt/gitprompt.sh
	    else
		. ~/bash-git-prompt/gitprompt.sh
	    fi
            GIT_PROMPT_HAS_RUN=true
        fi
        if [ -z "$NONINTERACT" ] ; then
            PROMPT_COMMAND=handleprompt # was likely just overriden
            if [ -x /usr/bin/direnv ] ; then
                eval "$(direnv hook bash)"
            fi
        fi
    }
fi
