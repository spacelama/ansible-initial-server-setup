# -*- Mode: shell-script -*-
# shellcheck shell=bash

# We've long appended our bash history upon a command running, using
# various methods.  `trap '...' DEBUG` was causing us to have to write
# lots of code just to make sure the command being tested was a user
# command and not some poor shell command, and pay careful attention
# to make sure it didn't slow down the shell too much.  Another probel
# with `trap '...' DEBUG` method of writing .bash_fullhistory is that
# if a command doesn't actually run (eg, #blah), then it doesn't get
# echoed yet to the history file.  Problem with PROMPT_COMMAND by
# itself is that it doesn't write to the history until the command
# returns.  Like
# https://superuser.com/questions/175799/does-bash-have-a-hook-that-is-run-before-executing-a-command
# We were using $PS0 for a while, via signal handlers per
# https://github.com/rcaloras/bash-preexec/issues/28 but that causes
# `job1 & job2 & job3 &` having job nunbers %3 %6 %9 etc.
#
# So let's just use bash-preexec, where someone smarter than us has
# already optimised this (and will fix it when bash 5.3 comes out)

function setup_preexec_hook() {
    if [ -z "$NONINTERACT" ] ; then
        local prior_PROMPT_COMMAND="$PROMPT_COMMAND"
        unset PROMPT_COMMAND


        preexec_functions+=(preexec_hook)
        precmd_functions+=("$prior_PROMPT_COMMAND")
        precmd_functions+=(handleprompt)

        __bp_enable_subshells=true # https://github.com/rcaloras/bash-preexec/issues/25
        if [ -e /etc/bash-preexec/bash-preexec.sh ] ; then
            . /etc/bash-preexec/bash-preexec.sh
        else
            # allow use of our PROMPT settings on machines that we don't have root/ansible control over
            . ~/bash-preexec/bash-preexec.sh
        fi

        # FIXME: when set after, these values just completely
        # disappear from scope...  Setting them before we load
        # bash-preexec seems to work though...
        #        preexec_functions+=(preexec_hook)
        #        precmd_functions+=("$prior_PROMPT_COMMAND")
        #        precmd_functions+=(handleprompt)

        # We need to call PROMPT_COMMAND again since the normal
        # bash-preexec assumption is that it sets PROMPT_COMMAND
        # itself to initialise the hooks, but we're only reaching this
        # point by virtue of PROMPT_COMMAND having already triggered.
        # But eval "$PROMPT_COMMAND" here doesn't run the required
        # functions probably because we are deep in a set of functions
        # that still do a bunch of stuff after we return.  We just
        # have to remember to modify PROMPT_COMMAND itself in the call
        # that initialised it.
    fi
}

function preexec_hook() {
    #            generatetitle "$BASH_COMMAND" # doesn't expand out sleep $i etc, so might as well use the full snippet:
    generatetitle "$1"
    writetohistory
    if [ -n "$EXTRA_PROMPT_PRE" ] && type -t $EXTRA_PROMPT_PRE 1>/dev/null ; then
        $EXTRA_PROMPT_PRE
    fi
    CMD_START_SECONDS=$SECONDS
}

function handleprompt () {
    retcode=$?

    if [ -n "$CMD_START_SECONDS" ] ; then
        statusbeep $retcode 1$SECONDS 1$CMD_START_SECONDS
    fi

    # we need to make sure the right history file is loaded, after the
    # previous command, which might have been a `cd` triggering a new
    # history file to be set, before the prompt returns.  Do it now.
    choosehistoryfile

    if [ -n "$EXTRA_PROMPT_SOURCE" ] && [ -e "$EXTRA_PROMPT_SOURCE" ] ; then
        . "$EXTRA_PROMPT_SOURCE"
    fi
    if [ -n "$EXTRA_PROMPT_POST" ] && type -t $EXTRA_PROMPT_POST 1>/dev/null ; then
        $EXTRA_PROMPT_POST
    fi

    setprompt
    return $retcode
}

# https://stackoverflow.com/questions/2575037/how-to-get-the-cursor-position-in-bash
# https://stackoverflow.com/questions/62038398/bash-clearing-the-last-output-correctly/62040654#62040654
# FIXME: oh, turns out zsh does this by default, and you can use that
# to search better for people who dunnit before (and who fixed other
# problems such as clearing in the input buffer):
# https://stackoverflow.com/questions/19943482/configure-shell-to-always-print-prompt-on-new-line-like-zsh
# Finally, there's some near cursor positioning advice here in the usual place (the Arch wiki):
# https://wiki.archlinux.org/title/Bash/Prompt_customization
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
        # the disown is so forked processes dont get a sighup -- but if you
        # background a program that needs the terminal, it wont be running
        # when the promt returns, so such programs will never be disowned -
        # only those that are truly running at one point in time when
        # backgrounded
        disown -r -h  # mark jobs as not affected by SIGHUP of this
                      # specific shell with SMALLPROMPT set (do it
                      # here so that it doesn't trigger in DEBUG trap,
                      # but does get executed when every prompt is
                      # set, giving us a chance to affect jobs just
                      # backgrounded
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
        if [ "$REALHOST" != "$LONGHOST" ] ; then
            me="${USER}"@"${REALHOST}"
        fi
        winname  "$EXTRA_TITLE`date +%d/%m-%H:%M:%S` -- $PTS -- $cmd -- ${me}: `chomp --escapechars --right ... $((COLUMNS-60)) ${shortpwd}`" ;
        iconname "$EXTRA_TITLE`date +%d/%m-%H:%M:%S` -- $PTS -- $cmd -- ${me}: `chomp --escapechars --right ... 20 ${shortpwd}`"
    }
    function setprompt () {
        local shortpwd="${PWD/$HOME/~}"
        local me="${USER}"@"${SHORTHOST}"
        if [ "$REALHOST" != "$LONGHOST" ] ; then
            me="${USER}"@"${REALHOST}"
        fi
        local bold="\\[${ESC}[7m\\]"
        local norm="\\[${ESC}[m\\]"
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
        PS1_PRE="#$PS1_PREV_EXIT_CODE\[${ESC}[0m\]"'-\j-\t, \d '$(colorizeprompt)":\\w"
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
fi
