#!/bin/bash

# ASSUMES A DUAL OR TRIPLE DISPLAY

# swap_window.sh
# Moves the active window to the other screen of a dual-screen Xinerama setup.
#
# Requires: wmctrl, xprop, xwininfo
#
# Author: Raphael Wimmer
# raphman@gmx.de

source bash_error_handler.sh

# if acting on the same window as last time, and rapidly, then go in a
# cyclic fashion.  If slowly, then alternate back and forth with the
# last location.  Else, go to the most common monitor given the
# current position
function desired_mode() {
    read lastTime lastWinId lastPos < /tmp/fvwm.switch_time
    if [ $numMonitors -le 2 ] || [ "$lastWinId" != "$1" ] ; then
        echo default
    elif (( $(date +%s) <= $lastTime + 5 )) ; then
        echo cycle
    else
        echo "$lastPos"
    fi
}

#set_before=$( set )

# get monitor dimensions
#monitorLine=$(xwininfo -root | grep "^ *Width")
# xrandr --current stops xrandr from blocking the X server for a fraction of a second
monitors=$( xrandr --current | grep ' connected' | sed 's/ primary//' | awk '{print $3}' | sed 's/[x+]/ /g' | sort -g -k 3 )
numMonitors=$( echo "$monitors" | wc -l )
monitorStarts=()
monitorEnds=()
monitorWidths=()
monitorHeights=()
while read w h x y ; do
    monitorStarts+=("$x")
    monitorEnds+=("$((x+w))")
    monitorWidths+=("$w")
    monitorHeights+=("$h")
done <<< "$monitors"

# get active window id
activeWinLine=$(xprop -root | grep "^_NET_ACTIVE_WINDOW(WINDOW)")
activeWinId="${activeWinLine:40}"

date=$(date +%s)
desired_mode=$( desired_mode "$activeWinId" )

# get window position
xPosLine=$(xwininfo -id $activeWinId | grep "Absolute upper-left X")
xPos=${xPosLine:25} 
yPosLine=$(xwininfo -id $activeWinId | grep "Absolute upper-left Y")
yPos=${yPosLine:25} 

# get window width
xWidthLine=$(xwininfo -id $activeWinId | grep "^ *Width")
xWidth=${xWidthLine:8}
xHeightLine=$(xwininfo -id $activeWinId | grep "^ *Height")
xHeight=${xHeightLine:9}

# get window decor
xWinDecorLine=$(xprop -id $activeWinId | grep "_KDE_NET_WM_FRAME_STRUT(CARDINAL)")
xWinDecor=${xWinDecorLine:35 :2}
yWinDecor=${xWinDecorLine:42 :2}

windowCentre=$((${xPos} + ${xWidth}/2))

#set_after=$( set )
#colordiff -ub <( echo "$set_before" ) <( echo "$set_after" ) || true

# calculate new window position
if [ $numMonitors -ge 3 ] && (( $windowCentre > ${monitorStarts[2]} )) ; then
    lastPos=right
    case "$desired_mode" in
        default|middle)
            #right hand screen, move to middle
            xPos=$(( ${xPos} - ${monitorWidths[1]} - ${xWinDecor} ))
            if (( $yPos + $xHeight > ${monitorHeights[1]} )) ; then
                yPos=$(( ${yPos} - ${monitorHeights[2]} + ${monitorHeights[1]} ))
            fi
            ;;
        *) # cycle|left)
            #right hand screen, move to left
            xPos=$(( ${xPos} - ${monitorWidths[1]} - ${monitorWidths[0]} - ${xWinDecor} ))
            if (( $yPos + $xHeight > ${monitorHeights[0]} )) ; then
                yPos=$(( ${yPos} - ${monitorHeights[2]} + ${monitorHeights[0]} ))
            fi
            ;;
    esac
elif (( $windowCentre > ${monitorStarts[1]} )) ; then
    lastPos=middle
    case "$desired_mode" in
        default|left)
            #middle screen, move to left
            xPos=$(( ${xPos} - ${monitorWidths[0]} - ${xWinDecor} ))
            if (( $yPos + $xHeight > ${monitorHeights[0]} )) ; then
                yPos=$(( ${yPos} - ${monitorHeights[1]} + ${monitorHeights[0]} ))
            fi
            ;;
        *) # cycle|right)
            #middle screen, move to right
            xPos=$(( ${xPos} + ${monitorWidths[1]} - ${xWinDecor} ))
            if (( $yPos + $xHeight > ${monitorHeights[2]} )) ; then
                yPos=$(( ${yPos} - ${monitorHeights[1]} + ${monitorHeights[2]} ))
            fi
            ;;
    esac
else
    lastPos=left
    case "$desired_mode" in
        default|cycle|middle)
            #left screen, move to middle
            xPos=$(( ${xPos} + ${monitorWidths[0]} - ${xWinDecor} ))
            if (( $yPos + $xHeight > ${monitorHeights[1]} )) ; then
                yPos=$(( ${yPos} - ${monitorHeights[0]} + ${monitorHeights[1]} ))
            fi
            ;;
        *) # right)
            #left screen, move to right
            xPos=$(( ${xPos} + ${monitorWidths[0]} + ${monitorWidths[1]} - ${xWinDecor} ))
            if (( $yPos + $xHeight > ${monitorHeights[2]} )) ; then
                yPos=$(( ${yPos} - ${monitorHeights[0]} + ${monitorHeights[2]} ))
            fi
            ;;
    esac
fi

yPos=$((${yPos}-${yWinDecor}))


# make sure window stays on screen completely
(( ${xPos} < 0 )) && xPos=0
[ $numMonitors -ge 3 ] && (( ${xPos} + ${xWidth} > ${monitorEnds[2]} )) && xPos=$((${monitorEnds[2]} - ${xWidth}))
[ $numMonitors = 2 ]   && (( ${xPos} + ${xWidth} > ${monitorEnds[1]} )) && xPos=$((${monitorEnds[1]} - ${xWidth}))
#(( ${yPos} + FIXME ${yWidth} > ${monitorEnds[2]} )) && xPos=$((${monitorEnds[2]} - ${xWidth}))

echo Raise
echo Move ${xPos}p ${yPos}p # warp

echo "$date $activeWinId $lastPos" > /tmp/fvwm.switch_time


# and bye
exit 0
