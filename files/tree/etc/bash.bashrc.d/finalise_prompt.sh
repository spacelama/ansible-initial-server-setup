# unconditionally set PROMPT_COMMAND as an inexpensive way of
# intercepting the first time a prompt is returned once a shell is
# opened.  Sourcing this file more than once won't result in that
# initialisation being repeated.  During the call of that function, we
# initialise our actual prompt handler, which will use bash-preexec
# repo.
#
# Our function that gets called when the PROMPT is first called to be
# drawn, is to initialise things like direnv, gitprompt, our our
# prompt handler and bash-preexec.  However, the bash-preexec
# initialisation already installs its own prompt loader that does
# similar, and so left to its own devices would simply reset
# PROMPT_COMMAND but not actually initialise, so we eval the now
# modified PROMPT_COMMAND again, which actually runs the bash-preexec
# prompt hooks properly this time.
[ -z "$NONINTERACT" ] && PROMPT_COMMAND='handleprompt_initialise ; eval "$PROMPT_COMMAND"'

# handleprompt_initialise is called as the last thing after invoking
# all shell login bashrc etc functions, before displaying the prompt,
# so that we can set up history handling.  We ideally only want to
# call this the once, regardless of whether we're on an ansible
# managed system with everything sources out of /etc/bashrc, or
# whether we're just using our own personal config out of CVS/git.  We
# achieve this through PROMPT_COMMAND above.
function handleprompt_initialise() {
    #trap: need to do this before writetohistory, because it could fail
    trap 'code=$? ; exitcleanly' 0
    trap '          exitcleanly' HUP
    #    trap 'writetohistory --full' USR2

    # unset the mechanism that called us, and we'll then set up
    # bash-preexec to get our actual prompt handler
    unset PROMPT_COMMAND

    setup_environment
    finalise_prompt
    finalise_login
    writetohistory "@$HOSTNAME OPEN"
}

function finalise_login() {
    [ ! -e /sys/class/power_supply/BAT0 ] || on_ac_power 2>/dev/null || ( apm_available 2>/dev/null && ( apm ; ibam ) || ( ibam --percentbattery ) 2>/dev/null )
}

if [ "$SMALLPROMPT" = yes ] ; then
    function finalise_prompt() {
        setup_preexec_hook
    }
else
    function finalise_prompt() {
        if [ -z "$NONINTERACT" ] ; then
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
            unset PROMPT_COMMAND # we don't actually care for gitprompt's own setting own PROMPT_COMMAND
            if which direnv >& /dev/null ; then
                eval "$(direnv hook bash)"
            fi
            setup_preexec_hook
        fi
    }
fi
