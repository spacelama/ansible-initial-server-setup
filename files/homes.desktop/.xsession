#!/bin/bash

echo .xinitrc.real

#HOME's XSESSION

unset DISPHOST
unset BASH_PROFILE_EXECUTED


PATH=$HOME/bin:$HOME/`uname -s`:$PATH
export PATH
cd
HOSTNAME=`hostname`
export HOSTNAME

export XSESSIONPID=$PPID
echo "Setting \$XSESSIONPID to $XSESSIONPID:"
ps -p $XSESSIONPID

#echo Before transformation, DISPLAY=$DISPLAY
#if [[ "$DISPLAY" == :? ]] ; then
#    PREV_DISPLAY=$DISPLAY
#    export DISPLAY=$DISPLAY.0
#    echo Resetting DISPLAY=$DISPLAY
#fi

rm -f $HOME/.xsession-errors.$HOSTNAME$DISPLAY.*
exec > $HOME/.xsession-errors.$HOSTNAME$DISPLAY.`date +%Y%m%d`.$$ 2>&1
exec < /dev/null

echo debug: $HOSTNAME $$
echo DISPLAY: "$PREV_DISPLAY->$DISPLAY"
#FIXME: this doesn't work anymore: set -i     #so $NONINTERACT is not set for startup scripts

export TERM=xterm
#NOBASHRCEXEC=yes #no export!
set -vx

#set up the display's properties before sourcing .bash_profile, as
#.bash_profile has to work out how big the screen is, etc
if [ -e "/home/tconnors/bin/initialise_xrandr_$HOSTNAME" ] ; then
    /home/tconnors/bin/initialise_xrandr_$HOSTNAME
fi
# nvidia-settings -a GPUOverclockingState=1 -a GPU2DClockFreqs=<GPU>,<MEM> -a GPU3DClockFreqs=<GPU>,<MEM>

#since powermizer doesn't seem to be enabled yet on this machine
##    nvidia-settings -a GPUOverclockingState=1 -a GPU2DClockFreqs=170,100 -a GPU3DClockFreqs=170,100

#original settings seem to be:
#    nvidia-settings -a GPUOverclockingState=1 -a GPU2DClockFreqs=170,100 -a GPU3DClockFreqs=475,400
#min settings seem to be:
#    nvidia-settings -a GPUOverclockingState=1 -a GPU2DClockFreqs=42,25 -a GPU3DClockFreqs=118,100


if ! ps -u $USER -o pid,args | grep -q /[k]eychain ; then
    rm -f $HOME/.keychain/$HOSTNAME-lockf
fi

sleep 3 # resolve any race conditions from running xrandr and waiting for things to resize
PS1='bogus'
for i in /etc/profile ~/.bash_profile ~/.profile ; do
    if [ -e "$i" ] ; then
        . $i
        break
    fi
done

setup_environment # in leui of .bashrc calling handleprompt() and
                  # bashrc_last() as very last thing, we setup all the
                  # aliases we later depend on

#set +vx

if programexists tsocks ; then
    #    X_PREPEND="$X_PREPEND tsocks"
    ontsocks
fi

#ssh-add < /dev/null
keys=""
for i in $HOME/.ssh2/id_dsa_1024_a $HOME/.ssh/id_dsa $HOME/.ssh/id_rsa ; do #$HOME/.ssh/identity 
    if [ -f $i ] ; then
        keys="$keys $i"
    fi
done
case $SHORTHOST in
    fs|pi|met)
        :
        ;;
    *)
        echo "Running keychain..."
        xterm -geometry 150x50+200+200 -e "echo Running keychain... ; keychain --quiet $keys"
        . $HOME/.keychain/$HOSTNAME-sh
        ;;
esac

export HOSTALIASES=$HOME/.hostaliases
if [ -n "$EXPORT_FVWM" ] ; then
    echo starting fvwm from host $EXPORT_FVWM
    colorize dark red "> ssh $EXPORT_FVWM ./.xinitrc (with HOSTALIASES=$HOSTALIASES)"
    exec ssh $EXPORT_FVWM ./.xinitrc
elif [ -n "$RUNXPROG" ] ; then
    colorize dark red "> $RUNXPROG (with HOSTALIASES=$HOSTALIASES)"
    exec $RUNXPROG
fi

rm -f $HOME/.xsession-errors.$HOSTNAME$DISPLAY-fvwm.* 2>&1
rm -f /var/tmp/FvwmCommand-${SHORTCANONICALDISP}*
#ln -s ~/.fvwm/FvwmCommand-${SHORTCANONICALDISP}{C,M} /var/tmp

export WINDOWMANAGER="$HOME/bin/keepfvwmalive"

echo Will start startmaindisplay.sh `date`
bash -x startmaindisplay.sh
if [ "$?" = 10 ] ; then
    echo "told to quit by startmaindisplay.sh"
    exit
fi
echo Finished startmaindisplay.sh `date`

# mpd ~/.mpd/config 

echo XSESSIONPID=$XSESSIONPID BASH_SESSION_PID=$BASH_SESSION_PID

echo exec $X_PREPEND $WINDOWMANAGER
exec $X_PREPEND $WINDOWMANAGER
