#!/bin/bash

function watchdog() {
    echo Setting watchdog to: $( date )
    last_ping=$( date +%s )
}


function poll() {
    while : ; do
        gateway=$( ip route get 8.8.8.8 | head -n 1 | awk '{print $3}' | grep . ) && fping -q -c 1 -t 1000 $gateway && watchdog
        sleep 20
        if [ $( date +%s ) -gt $(($last_ping+60)) ] ; then
            # haven't had network for 2 minutes
            echo "Restarting NetworkManager at $( date )"
            nmcli device disconnect wlan0
#            timeout -k 10 120 systemctl restart NetworkManager
#            rfkill block wlan
            sleep 5
#            rfkill unblock wlan
#            nmcli device reapply wlan0 # or maybe connect wlan0
            nmcli device connect wlan0
            watchdog # ensure no immediate retrigger
        fi
    done
}


case `hostname` in
    dirac-laptop|fermi)
        watchdog
        poll
        ;;
esac
