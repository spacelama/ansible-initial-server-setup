#!/bin/bash

# ASSUMES A DUAL DISPLAY
# copied from fvwm_swap_windows.sh

# get monitorWidth
monitorLine=$(xwininfo -root | grep "Width")
monitorWidth=$((${monitorLine:8}))
if [ $monitorWidth -gt 1920 ] ; then
    monitorWidth=$(($monitorWidth/2))
fi
#echo $monitorWidth
monitorLine=$(xwininfo -root | grep "Height")
monitorHeight=${monitorLine:9}
#echo $monitorHeight

# get active window id
activeWinLine=$(xprop -root | grep "^_NET_ACTIVE_WINDOW(WINDOW)")
activeWinId="${activeWinLine:40}"

# get window position
xPosLine=$(xwininfo -id $activeWinId | grep "Absolute upper-left X")
xPos=${xPosLine:25} 
#yPosLine=$(xwininfo -id $activeWinId | grep "Absolute upper-left Y")
#yPos=${yPosLine:25} 

# get window width
xWidthLine=$(xwininfo -id $activeWinId | grep "Width")
xWidth=${xWidthLine:8}
#xHeightLine=$(xwininfo -id $activeWinId | grep "Height")
#xHeight=${xHeightLine:9}

# get window decor
xWinDecorLine=$(xprop -id $activeWinId | grep "_KDE_NET_WM_FRAME_STRUT(CARDINAL)")
xWinDecor=${xWinDecorLine:35 :2}
yWinDecorLine=$(xprop -id $activeWinId | grep "_KDE_NET_WM_FRAME_STRUT(CARDINAL)")
yWinDecor=${yWinDecorLine:38 :2}

# calculate new window position
if (( ${xPos} + ${xWidth}/2  > ${monitorWidth} )) ; then
    #right hand screen
    xPos=${monitorWidth}
else
    #left hand screen
    xPos=0
fi
yPos=0
xWidth=$(( $monitorWidth - 2 * ${xWinDecor} ))
yHeight=$(( $monitorHeight - 2 * ${yWinDecor} - 125 ))

#echo Raise
echo ResizeMove ${xWidth}p ${yHeight}p ${xPos}p ${yPos}p


# and bye
exit 0
