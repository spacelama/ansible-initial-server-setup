#!/bin/bash
# -*- Mode: shell-script -*-
#template by ~tconnors/bin/newshscript
#Wed May 14 23:36:36 EST 2003

# USENET posts often have URLs split over many lines. Lets concat them
# together into one line.  furthermore, to save on mouse and
# keystrokes, and since one pastes all of the URL in one go, timeout
# after a second or two

norun=

if [ "$1" = --norun ] ; then
    shift
    norun=true
fi


#if [ $# -ne 0 ] ; then
if [ "$1" = --help ] ; then
   echo "Usage: $0 [--norun]" 1>&2
   exit 1
fi

cmdsupplied=
line1="$*"   #capture if the user has accidentally supplied part of the line on the command line
if [ -n "$line1" ] ; then
    cmdsupplied=1
    echo -n "$line1"
fi

IFS=''
read -r lines    # first, read a line - no timeout - that way we can run, and take as long as we want, before pasting URL
line1=$(echo "$line1" | sed 's/^ *//' | sed s'/ *$//' )
lines=$(echo "$lines" | sed 's/^ *//' | sed s'/ *$//' )
total="$line1$lines"
#echo -n "$total"
while read -r -t 0.01 line ; do
#    echo "found line $line" 1>&2
    line=$(echo "$line" | sed 's/^ *//' | sed 's/ *$//' )

    #now keep on reading. Perhaps the last line didnt have a newline,
    #and it timeouted, in which case $line is now empty, and there are
    #still keystrokes in the buffer.

    total="$total$line"
#    echo -n "$total"
done
first=true
middlespaces=
while read -r -t 1 -n 1 char ; do #I cant think of any way to get these keys without reading one char at a time.
#    echo "found extra char $char" 1>&2
    if [ $first = true -a "$char" = " " ] ; then #first chars of line is a space?
        echo
        echo -n "$total"
    elif [ "$char" = "" ] ; then #newline
        first=true
        echo -n "$total"
    elif [ "$char" = " " ] ; then #a space elsewhere in the line
        middlespaces="$middlespaces$char"
    else
        total="$total$middlespaces$char"
        middlespaces=
        first=false               #chop spaces from start of each line but not in middle
    fi
done

#echo "$total"
echo
total=`echo "$total" | sed 's/^<// ; s/>$// ; s/^URL://'`
if [ -z "$norun" ] ; then
    browser-hist "$total"
fi

if [ -n "$cmdsupplied" ] ; then
    echo "WARNING: Cmdline was supplied. If there were any funky characters in it, the URL is likely screwed up"
    exit 1
fi
