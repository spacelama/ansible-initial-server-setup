# -*- Mode: shell-script -*-
# shellcheck shell=bash

function ontsocks() {
  #FIXME: if 64 bit apps start being needed, may eventually compile for 64 bit, and then move it into ~/.../sparcv9/ (or /64/), and then remove this hack.  Maybe just need to have both versions, and set $LD_PRELOAD, and it will pick it the correct version in the correct path
  #FIXME: in fact, the whole hack can probably be removed, since solaris can probably deal with the different 32/64 bit paths in $LD_LIBRARY_PATH implicitly too, and just setting LD_PRELOAD to libtsocks without an explicit path
    location=`LD_PRELOAD=libtsocks.so ldd /bin/sleep | grep libtsocks | awk '{print $3}'`
    offtsocks
    if [ -n "$location" ] ; then
        export LD_PRELOAD$LD_OWN_ARCH="$location `eval echo \\\$LD_PRELOAD$LD_OWN_ARCH`"
    fi
}

function offtsocks() {
    if [[ `eval echo \\\$LD_PRELOAD$LD_OWN_ARCH` == *libtsocks.so* ]] ; then

        export LD_PRELOAD$LD_OWN_ARCH=$(echo `eval echo \\\$LD_PRELOAD$LD_OWN_ARCH` | sed 's![^:]*libtsocks.so!!')
        if [ -z `eval echo \\\$LD_PRELOAD$LD_OWN_ARCH` ] ; then
            unset LD_PRELOAD$LD_OWN_ARCH
        fi
    fi
}

#ontsocks   #done in the X startup files.  Don't necessarily want this when we log in remotely

export TSOCKS_CONF_FILE=$HOME/.tsocks.conf
