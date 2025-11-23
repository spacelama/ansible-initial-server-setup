#!/usr/bin/env -S echo "This should not be run directly - try using the following command, perhaps configuring \$ERROR_CONTINUE, \$ERROR_PRINT_UNSAFE: \nsource"
# -*- Mode: shell-script -*-
# shellcheck shell=bash

# source this, and you get a nicer 'set -xv' output
# https://news.ycombinator.com/item?id=44666984

      export __VERBOSE=1 ;
      #export PS4='+${LINENO}: ' ;
      #export PS4='+ #${BASH_SOURCE}:${LINENO}:${FUNCNAME[0]:+${FUNCNAME[0]}()}:$(date +%T)\n+ ' ;
      #export PS4='+ ${LINENO} ${FUNCNAME[0]:+${FUNCNAME[0]}()}: ' ;
      #export PS4='+ $(printf "%-4s" ${LINENO}) | '
      export PS4='+ $(printf "%-4s %-24s " ${LINENO} ${FUNCNAME[0]:+${FUNCNAME[0]}} )| '
      #export PS4='+ $(printf "%-4s %-${SHLVL}s %-24s" ${LINENO} "     " ${FUNCNAME[0]:+${FUNCNAME[0]}} )| '

      export __VERBOSE=1 ;
      # red=31
      export ANSI_FG_YELLOW='\e[1;33m'
      #export MID_GRAY_256='\e[38;5;244m'    # Example: a medium gray
      export _CRESET='\e[0m'
      export _COLOR="${ANSI_FG_YELLOW}"
      printf "${_COLOR}DEBUG: --debug-color: This text is ANSI gray${_CRESET}\n" >&2
      export PS4='+ $(printf "${_COLOR}%-4s %-24s%s |${_CRESET} " ${BASH_SOURCE}:${LINENO}"${FUNCNAME[0]:+(${FUNCNAME[0]})}" )'

set -xv

ho=HO

asd() {
    echo hi $ho
    false
    echo there
}

echo 1
asd
echo 2

PS4='\[\e[32m\]\u@\h:\w ${LINENO} {$BASH_COMMAND} \$ \[\e[0m\]' # Customize the prompt for more context

for i in `seq 1 4 ` ; do
    echo i:$i
done

echo "This is a test"
echo "$MY_VAR" "$ANOTHER_VAR"
v=/nonexistent
ls $v
