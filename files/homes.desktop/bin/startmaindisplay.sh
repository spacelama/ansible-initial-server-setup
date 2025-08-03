#!/bin/bash

# spawned by ~/.xsession which is responsible for setting up our
# display and our window manager.  We're responsible for starting up
# programs we want starting on boot

echo `date`: Starting startmaindisplay.sh on $DISPLAY
#sometimes fvwm will die, when this happens, hopefully it will be
#respawned. But we don't want to do all the initialisation again, so
#kill it

#if [ "$RESTARTINGFVWM" ] ; then
#	exit
#fi

PATH=$PATH:$HOME/bin:/usr/local/bin:/usr/games
export PATH
#echo \$PATH=$PATH
set

trap 'date' DEBUG

# create new blank XAUTHORITY file for every login:

echo -n "Overriding XAUTHORITY=$XAUTHORITY with: "
XAUTHORITY=/tmp/.$USER.$DISPLAY ; export XAUTHORITY
echo XAUTHORITY=$XAUTHORITY
xauth generate $DISPLAY . trusted

#if [ -z "$VERT2SIZE" -o -z "$HOR2SIZE" ] ; then
#  sleep 3
#  . ~/.bash_profile # try calculate it again - might have a race condition on some startups
#fi

unthrottle_browser &
restart-compositor
[ $SHORTHOST = fermi ] && daemon-work-external-display

xrandr --current
for i in HORSIZE VERTSIZE HOR2SIZE VERT2SIZE ; do
    echo $i=`eval echo '$'$i`
done

function SetupTimer () {
    #must be forked
    echo "SetupTimer(): $( date )"
    sleep 60
    findprocessbypid $$ && ( echo signalling main timer fork $$ ; kill -SIGALRM $$ ; echo this timer stopped `date` ) || echo "Timer stopped successfully `date`"
    echo "SetupTimer return: $( date )"
}

function TimeOut () {
    #running as part of the main thread, this is called from the SIGALRM
    echo "TimeOut(): $( date )"
    pid=$$
    xterm -geometry 180x40+10+256 -fn 'fixed' -xrm 'XTerm*renderFont:false' -e "pstree -a -l -p -H $pid | less" &  # -G
    pid=$!
    sleep 0.1
    xmessage -geometry +280+250 'Seems something got stuck in startmaindisplay' -buttons "Sleep more,Abort"
    status=$?
    findprocessbypid $pid && kill -TERM $pid
    if [ "$status" = 102 ] ; then
        exit 10   #tell the caller to quit
    elif [ "$status" = 101 ] ; then
        SetupTimer &
    else
        echo "Unexpected failure code from xmessage"
    fi
    echo "TimeOut return: $( date )"
}

trap 'TimeOut' ALRM
SetupTimer &

function waitforcommandpipe() {
    #    set -vx
    echo "Waitforcommandpipe(): $( date )"
    i=0
    while [ $i -lt 100 -a "$(ls -L /var/tmp/FvwmCommand-${SHORTCANONICALDISP}*{C,M})" = "" ] ; do
	sleep 1
        i=$(($i+1))
    done
    echo "Waitforcommandpipe return: $( date )"
}

function setupdesks() {
    echo "setupdesks(): $( date )"
    waitforcommandpipe
    for i in `pidof FvwmCommandS` ; do
        ls -lA --color=yes /proc/$i/fd
    done
    ls -lA --color=yes /var/tmp
    sleep 1
    FvwmCommand 'GotoDesk 0 1'

    ( sleep 5 ; FvwmCommand 'GotoDesk 0 0' ) &
    echo "setupdesks return: $( date )"
}

function waitfordesk1() {
    echo "waitfordesk1(): $( date )"
    waitforcommandpipe
    sleep 10
    echo "waitfordesk1 return: $( date )"
}

function waitfordesk2() {
    echo "waitfordesk2(): $( date )"
    waitforcommandpipe
    sleep 2
    echo "waitfordesk2 return: $( date )"
}

(setupdesks)&

#SET XSESSIONPID to something that will at least not exist anymore when X quits


###########################################You can add things below here...####

#killdxconsole

#gnome-settings-daemon &   # /desktop/gnome/background/draw_background must be off, and probably want to start it manually for a first time and make sure Xmodmap.* aren't loaded.   Nope, disabled again 2013/07/10 because debian upgrades have bought in dconf, and I can't convince it not to fuck around with the screensaver
sleep 3 # give it time to screw up the settings which we then go and fix soon...

if [[ $DISPLAY == :* ]] ; then
    setupkeymap # xmodmap etc
fi

if [ -x /usr/bin/synclient ] ; then
    synclient -l > $HOME/.touchpadrc-orig
    setup-touchpad
fi

repeat=0  # FIXME: turns out you've been able to address individual
          # monitors all along.  xterm -geometry 80x24+100+100@1
          # but nevertheless, that syntax changes in fvwm3 to:
          # xterm -geometry 80x24+100+100@DisplayPort-1
          # https://github.com/fvwmorg/fvwm3/discussions/878

#xhost +localhost
#xhost -

xrdb-reinitialise

if [ $HORSIZE -ge 1024 ] ; then
    xterm -ls -geometry +0+0   &
    #    xterm -ls -geometry +500+0 &
    if [ $OS = SunOS ] ; then
        (waitfordesk2 ; exec xterm -geometry $((87*$HORSIZE/1280))x$((62*$VERTSIZE/1024))+0+$(($VERTSIZE*2)) -fn 'fixed' -xrm 'XTerm*renderFont:false' -T "top cpu@$SHORTHOST" -e bash -l -c top ) &
        (waitfordesk2 ; exec xterm -geometry $((87*$HORSIZE/1280))x$((62*$VERTSIZE/1024))+$(($HORSIZE/2))+$(($VERTSIZE*2)) -fn 'fixed' -xrm 'XTerm*renderFont:false' -T "top memory@$SHORTHOST" -e bash -l -c top ) &
    else
        (waitfordesk2 ; exec xterm -geometry $((101*$HORSIZE/1280))x$((67*$VERTSIZE/1024))+0+$(($VERT2SIZE*2)) -fn 'fixed' -xrm 'XTerm*renderFont:false' -T "top (cpu)" -e top ) &
        (waitfordesk2 ; exec xterm -geometry $((101*$HORSIZE/1280))x$((67*$VERTSIZE/1024))+$(($HORSIZE/2))+$(($VERT2SIZE*2)) -fn 'fixed' -xrm 'XTerm*renderFont:false' -T "top (memory)" -e top ) &
    fi
fi

bash -xv newxclock
bash -xv restartx11vnc

# superceded by /etc/rc.local: xterm -iconic -e 'start-tunnel.sh' &
xterm -iconic -e 'inputplug -d -c bin/on-new-kbd' &

# REMEMBER: the xterms that run something don't have -ls, but those that don't do!

cut-paste-mgmt
case $LONGHOST-$SHORTDISPHOST in
    met.*-*)
        pidisplay &
#        setupx2x --usebarrier --server dirac --left &
        ;;
    bohr*-galileo)
        procmeter3 -geometry +$(($HORSIZE-108))+$(($VERTSIZE-364)) -xrm procmeter3*pane*backgroundPixmap: --resources.background=grey15 &
        ;;
    scuzzie*-*|dirac*-*|gamow*-*|maxwell*-*|fermi*-*|bohr*-*|hubble*-*|penrose*-*|curie*-*|fs*-*)
        #multihead displays with custom setup
        if [ $SHORTHOST = scuzzie -o $SHORTHOST = dirac -o $SHORTHOST = gamow -o $SHORTHOST = maxwell -o $SHORTHOST = fs -o $SHORTHOST = fermi ] ; then
            if [ $SHORTHOST = scuzzie ] ; then
                killall i8kbuttons
                i8kbuttons -u "mastvolup" -d "mastvoldown" -m "togglevolmute" -r 100 -v &
                if [ $HORSIZE -gt 1024 ] ; then
                    procmeter3 -geometry +$(($HORSIZE-104))+$(($VERTSIZE-465)) &
                else
                    procmeter3 -geometry +$(($HORSIZE-104))+$(($VERTSIZE-529)) &
                fi
            elif [ $SHORTHOST = dirac ] ; then
                repeat="0 $HOR2SIZE"

                for i in $repeat ; do
                    procmeter3 -geometry +$(($HORSIZE-104+$i))+$(($VERTSIZE-650)) &
                done
                #                hotkeys -Z
                #                ( osdsh ; sleep 5 ; osdctl -S ~/.osdsh ) &
                #                nvidia-settings --load-config-only
                #                ( sleep 30 ; skype ) &
                #                xrandr --fb 3360x1050 --output LVDS1 --scale 1x1 --output VGA1 --pos 0x0 --panning 3360x1050+0+0/3360x1050+0+0/0/0/0/0 # a cute little hack that after xrandr is initially set up previously to have 2 side by side monitors, and fvwm has initialised itself, we then set up a panning window for the right (primary) screen, and the left screen is just a static version of the left screen
                #                setupx2x --usebarrier --right &

                #                barrier & # 20230627 - find proper method to start using tray and default config 20230801 disabled since no longer have work laptop
                # following commandline taken from an invocation from within barrier
                /usr/bin/barriers -f --debug INFO --name dirac -c ~/.local/share/barrier/.barrier.conf --address :24800 &
                # Barrier will be replaced by InputLeap when it hits
                # 3.0.  In the meantime, keep using 2.4.0:
                # https://github.com/input-leap/input-leap/issues/1414
                # https://github.com/input-leap/input-leap
                # https://github.com/debauchee/barrier/issues/1989

                monitorCamera &

                sudo env bash -l -i -c addkeychain   # needed for get_conf_info daily

            elif [ $SHORTHOST = gamow ] ; then
                #                repeat="0 1920"
                repeat="0 $HORSIZE"

                for i in $repeat ; do
                    procmeter3 -geometry +$(($HORSIZE-104+$i))+$(($VERTSIZE-650)) &
                done
            fi
            syndaemon -t -i 1.0 -k -d -p /tmp/syndaemon.$DISPLAY.pid
        else
            procmeter3 -geometry +$(($HORSIZE-108))+$(($VERTSIZE-303)) &
        fi
        sleep-browser.sh &
	if ls -lA /dev/dvb/adapter{0,1} ; then
            #	if ls -lA /dev/dvb/adapter0 ; then
            ( sleep 90 ; kaffeine ) &
	fi

        if [[ "$SYSTEM" == SunOS* ]] ; then
            xset r 103  #pgup/down repeat
            xset r 130
        fi
        #        setup-cloud-tunnel.sh >& /var/log/setup-cloud-tunnel.log &
        ;;
    *.bom.gov.au-*)
        repeat=0
        case $SHORTHOST in
            #            ant)
            #                repeat="0 1280"
            #            ;;
            weinberg)
                repeat="0 $HORSIZE $((2*$HORSIZE))"
            ;;
        esac
        pidgin & # jabber
        for i in $repeat ; do
            procmeter3 -geometry +$(($HORSIZE-104+$i))+$(($VERTSIZE-325)) &
        done

        xinput list-props 15
        xinput set-float-prop 15 'Device Accel Adaptive Deceleration' 2
        xinput set-float-prop 15 'Device Accel Constant Deceleration' .2
        xinput set-int-prop 15 'Evdev Scrolling Distance' 32 -1 1 1
        ;;
    *.swin.edu.au-*)
        procmeter3 -geometry +$(($HORSIZE-108))+$(($VERTSIZE-324)) &
        ;;
    *)
        procmeter3 -geometry +$(($HORSIZE-108))+$(($VERTSIZE-294)) &
        ;;
esac
reset_mouse_keyboard_speed

#penguineyes -d &
for i in $repeat ; do
    tuxeyes --puppy -geometry 70x80+$((1024+$i))-1 &
    weather -bg gray44 -geometry 80x80 &
done

#wmcliphist &
(
    if false && type -p oneko > /dev/null ; then
        oneko -name leadcat -tofocus -time 65000 &
        sleep 5
        oneko -toname leadcat -position +20+35 -bg orange -fg brown -tora &
    fi
) &

#if [[ "$DISPLAY" == :0* ]] ; then
#    icu &
#fi

xsetroot -solid grey50

#causes gtk to flicker: http://www.mail-archive.com/fvwm-workers@lists.math.uh.edu/msg14140.html
killall unclutter  # X starts up up, pain in the arse
if [ `hostname` != weinberg ] ; then # horrible X cursor bug on skylake
    unclutter -noevents &
fi



####################           PI               #########
case `hostname` in
    pi|met)
        exit 0
        ;;
esac
####################           PI               #########

if [ "`xdpyinfo |sed -n 's/.*depth of root window: *\([^ ]*\) .*/\1/p'`" -ge 16 -a `sed -n 's/^MemTotal:  *\([^ ]*\)  *kB/\1/p' /proc/meminfo` -gt 128000 ] ; then
    #if we have enough colours, display a perty background.
    #dont want this running on the 8 bit terminal, because it be stealin' yer colours
    ( sleep 120 ; nice updatebackground.sh ) &
fi

(
#    [ -z "$HOR2SIZE" ] && HOR2SIZE=0
    trayer --monitor 0 --widthtype request --align right --expand true --alpha 50 --transparent true --distancefrom right --distance 0 &
    pasystray &
    sleep 1
    pnmixer &
    blueman-applet &
    solaar --window=hide &
    # stalonetray
) &

mountremote &

case $system in
    SWIN)    #all .swin.edu.au - unfortunately, some are named inconsistenty - some hostname give .ssi.swin, others don't
        #        xset dpms 3600 0 0
        xset b 100 600 50
        #        esd -nobeeps -tcp -public &
        #        esd -nobeeps &
        #	xemacsserver.sh &
        #	xset +fp $HOME/.xemacs/packages/etc/x-symbol/pcf/
        ;;
esac

#first, because it is so large, and would cover other useful things:
#SMALLPROMPT=yes xterm -name smallxterm -ls -geometry 128x3+450-0 -bg darkblue -fg coral &

#then later...
#xkbvleds -geometry 64x20+902-1 &
#if [ "$system" = SWIN ] ; then
#D=`echo "$DISPLAY" | sed 's/\.0$//'`
#weather &
#wmcliphist -geometry +635+691 &
#wmcliphist &
#wmmoonclock -lat -37.72 -lon -144.965 &   #from http://www.calle.com/info.cgi?lat=-37.72&long=144.965&name=Hawthorn&cty=Australia&alt=380
#vumeter -v -x $(($HORSIZE - 50)) -y $(($VERTSIZE - 400)) &

#xterm -geometry 200x200+0+0

echo `date`: Restarting any auto-spawning processes that rely on a started Xsession on $DISPLAY
killall gbuffy runxscreensaver runxscreensaver-monitor  # volume-detect doesn't need the local display - it just uses notify-send

echo `date`: Finishing startmaindisplay.sh on $DISPLAY

exit 0
