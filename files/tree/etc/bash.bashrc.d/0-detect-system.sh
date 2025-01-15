# -*- Mode: shell-script -*-


#set -x 

if [ "$BASH_PROFILE_INITIATING$BASHRC_INITIATING" != "" ] ; then
    return   #already running -- return outside a function returns to who called "source"
fi

RETURN=120    #the code we return in a subrcfile, in order to get the main file to return
#echo BASH_PROFILE_EXECUTED=$BASH_PROFILE_EXECUTED
##########################

if [ -x $HOME/bin/system ] ; then
    case `$HOME/bin/system` in
        home)
            export system=HOME
            extrabashprofile=$HOME/.bash_profile.home
            ;;
    esac
    export extrabashprofile

    if [ -n "$extrabashprofile" -a -e "$extrabashprofile".pre ] ; then
        . "$extrabashprofile".pre    #needs to be first so incorrect aliases can be unset (before setting them in .bashrc), and we can set $BASH_PROFILE_EXECUTED correctly
    fi
fi
