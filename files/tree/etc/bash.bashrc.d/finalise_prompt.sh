# call bashrc_last as the very last thing after invoking all shell
# login bashrc etc functions, before displaying the prompt, so that we
# can set up history handling.  We ideally only want to call this the
# once, regardless of whether we're on an ansible managed system with
# everything sources out of /etc/bashrc, or whether we're just using
# our own personal config out of CVS/git
function bashrc_last() {
    #trap: need to do this before writetohistory, because it could fail
    trap 'code=$? ; exitcleanly' 0
    trap '          exitcleanly' HUP
    #    trap 'writetohistory --full' USR2

    #    if [ "$versiona" -ge 4 ] ; then  # version 4 since version 3 in rhel5 causes race condition:
    #-bash: child setpgid (20151 to 20139): Operation not permitted
    #-bash: child setpgid (20153 to 20139): Operation not permitted

    #programs that should update the title of the xterm
    #and those whose args should also be displayed
    #        trap 'generatetitle "`eval "echo "$BASH_COMMAND" | sanitise"`"' DEBUG

    setup_environment
    setup_precmd_hook
    writetohistory "@$HOSTNAME OPEN"
    finalise_login
}

function finalise_login() {
    [ ! -e /sys/class/power_supply/BAT0 ] || on_ac_power 2>/dev/null || ( apm_available 2>/dev/null && ( apm ; ibam ) || ( ibam --percentbattery ) 2>/dev/null )
}

if [ "$SMALLPROMPT" = yes ] ; then
    function finalise_prompt() {
        # ESC=`echo -ne '\033'`
        # . ~/bash-git-prompt/gitprompt.sh
        [ -z "$NONINTERACT" ] && PROMPT_COMMAND='calling finalise_prompt_small: PROMPT_COMMAND/handleprompt ; handleprompt ; called finalise_prompt_small: PROMPT_COMMAND/handleprompt'
    }
else
    function finalise_prompt() {
        if [ -z "$GIT_PROMPT_HAS_RUN" ] ; then
            ESC=`echo -ne '\033'`
            if [ -e /etc/bash-git-prompt/gitprompt.sh ] ; then
                . /etc/bash-git-prompt/gitprompt.sh
            else
                # allow use of our PROMPT settings on machines that we don't have root/ansible control over
                . ~/bash-git-prompt/gitprompt.sh
            fi
            GIT_PROMPT_HAS_RUN=true
        fi
        if [ -z "$NONINTERACT" ] ; then
            PROMPT_COMMAND='calling finalise_prompt_normal: PROMPT_COMMAND/handleprompt ; handleprompt ; called finalise_prompt_normal: PROMPT_COMMAND/handleprompt' # was likely just overriden
            if which direnv >& /dev/null ; then
                eval "$(direnv hook bash)"
            fi
        fi
    }
fi
