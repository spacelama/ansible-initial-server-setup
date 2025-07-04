# -*- Mode: shell-script -*-
# shellcheck shell=bash

# User specific environment and startup programs

#return
#set -vx ;
#echo .bashrc

# echo "$(date): '$DISPLAY' '$SSH_CONNECTION' '$SSH_CLIENT':" "$@" >> /tmp/.bashrc.log

#echo 5 1>&2
if [ -n "$BASHRC_EXECUTED" -o -n "$NOBASHRCEXEC" ] ; then
   return
else
   BASHRC_EXECUTED=yes   # explicitly not exported
fi
#echo 6 1>&2

# BASH_QUICK can be set by outside scripts (in ~/bin) to at least set
# up a minimal environment
if [ -n "$BASH_QUICK" ] ; then
    # similar code as at bottom, but since
    # there is no history in non interactive shells...
    trap 'code=$? ; cd ; trap - 0 HUP ; exit $code' 0
    trap '          cd ; trap - 0 HUP ; kill -HUP $$' HUP

    return
fi
#echo 7 1>&2

#echo PATH=$PATH
if [ -f /etc/bash.bashrc -a -z "`grep -i -e SuSE -e 'Red Hat' -e 'Centos' /etc/issue 2>/dev/null`" ]; then
#echo 13 1>&2
    # for home (but not SuSE, since it insists on trying to be the
    # most fscking difficult possible)
    . /etc/bash.bashrc
fi
if [ -f /etc/bashrc -a -z "`grep -i -e SuSE -e 'Red Hat' -e 'Centos' /etc/issue 2>/dev/null`" ]; then
    # for brecca
    . /etc/bashrc
fi

if [ -e "$HOME/.bashrccommon" ] ; then
    . $HOME/.bashrccommon
fi

if [ -f  ~/.bash_profile -a -z "$BASH_PROFILE_EXECUTED" ] ; then
    # works no matter what state $BASH_PROFILE_EXECUTED is in, as long
    # as it is set
    if [ -z "$BASH_PROFILE_INITIATING" ] ; then
        # don't get outselves in a knot
        BASHRC_INITIATING=1
	. $HOME/.bash_profile  # we need to execute .bash_profile (if
                               # needed), before doing anything else,
                               # because out PATH might not have been
                               # set up yet...
	unset BASHRC_INITIATING
    fi
fi
#echo 8 1>&2

if [ -z "$NONINTERACT" -a -n "$PS1" -a -z "$PBS_ENVIRONMENT" ] ; then
    (
        # only display /etc/motd once per change
        hushlogin=".hushlogin.$SHORTHOST"
        cd $HOME
        [ "$SHORTHOST" != pentane ] &&
            [ $hushlogin -nt /etc/motd ] || (
                    touch $hushlogin &&
                    cat /etc/motd &&
                    echo ------------------------------------------------------------
                )
    )

    function bashversion () {
        versiona=`echo $BASH_VERSION | sed 's/^\([^.]*\)\.\([0-9]*\).*/\1/'`
	versionb=`echo $BASH_VERSION | sed 's/^\([^.]*\)\.\([0-9]*\).*/\2/'`
    }
    bashversion

    # want the following to be after bashversion, because it relies on
    # this to set the completion stuff, and want all these
    # calculations to be done after everything else
    if [ -x $HOME/bin/system ] ; then
        case `$HOME/bin/system` in
            ssi.swin.edu.au)
                extrabashrc=$HOME/.bashrc.SuSE
                ;;
            vpac.org)
                extrabashrc=$HOME/.bashrc.VPAC
                ;;
            home)
                extrabashrc=$HOME/.bashrc.home
                ;;
            apac)
                extrabashrc=$HOME/.bashrc.APAC
                ;;
            aaocbn)
                extrabashrc=$HOME/.bashrc.AAOCBN
                ;;
            suphys)
                extrabashrc=$HOME/.bashrc.suphys
                ;;
            bom)
                extrabashrc=$HOME/.bashrc.BOM
                ;;
        esac
    fi
    if [ -n "$extrabashrc" -a -e "$extrabashrc" ] ; then
        . $extrabashrc
    fi

    if [ "$TERM" != vpnreconnect ] ; then
        programexists fortune && programexists cowsay && programexists lolcat && fortune -s computers | cowsay | lolcat
        #        ( cd $HOME ; wego -f emoji -d 2 -b json | sed '/^$/d' )
        # print the weather forecast
        if [ -e $HOME/.wttr.cache ] ; then
            head -n 24 $HOME/.wttr.cache
        fi
    fi
fi

# sillysu etc are /etc/profile.d/ file dropped into place on
# machines I control, but allow us to do the same thing on other
# machines too
for i in ~/.bashrc.d/*.sh ; do
    if [ -r "$i"  ] ; then
        . "$i"
    fi
    unset i
done

#echo NONINTERACT=$NONINTERACT 1>&2

if [ -n "$NONINTERACT" -o -n "$PBS_ENVIRONMENT" ] ; then
    setup_environment
fi

# finalise_prompt gets called by having set up PROMPT_COMMAND to initialise it



#set +x
