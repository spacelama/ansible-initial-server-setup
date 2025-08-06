#!/bin/bash

# So that wireless connected devices have no pretence that they're
# able to talk to the internet through this device, force them to
# disconnect from us and allow themselves to failover to another
# working AP (if there are any) mobile data if they have a SIM.
#
# Monitors for upstream internet connection (not just that we have a
# valid connection to our gateway), so that if no internet, we take
# down the main SSIDs and implement an emergency SSID that I can still
# use to get in during an emergency to manage this device (useful if
# we accidentally locked out all valid VLANs from all ethernet ports).
#
# This does mean in an internet outage we do lose contact to all of
# our wifi devices and can't manage them independent of the cloud.
# For any IOT devices that our system can still orchestrate
# independent of the cloud, we want them to remain connected to our
# system (also, they probably don't have SIMs, so they have no
# mechanism to fallback to mobile data).  Can work around this by
# having the IOT devices talk to an SSID that is still in our
# wireless.emergency config.  This has a counter-mitigation - if it's
# just this device that has lost connection to the network, then
# devices connected to us will still be isolated from the rest of the
# network and our system can't orchestrate them and they won't
# failover to one of the working APs. *That* has a mitigation in that
# at least our Tasmota devices are configured for two APs - if they
# get a valid SSID but can't talk to the actual network and get a DHCP
# address (served out only via our gateway), then they'll failover to
# the other AP, which hopefully we've configured to be the non-optimal
# AP and thus not us, and thus might be able to talk to the gateway.
#
# A better mitigation against system vs device failure is to implement
# two levels of wireless.emergency - one for loss of internet but
# retention of gateway (disable mode), in which case we keep serving
# out Asio2.4 as well as our emergency SSIDs, and loss of device to
# gateway (and thus also internet), in which case we only serve out
# the emergency SSIDs (isolate mode).
#
# Setup: We monitor whether we have internet by if we can ping
# $upstream1 or $upstream2 (google 8.8.8.8 or cloudflare 1.1.1.1).  If
# both are down, then the internet must be down.  Our default AP setup
# is via /etc/config/wireless, which you've configured through LUCI or
# this ansible playbook etc.  Fallback AP config is through
# /etc/config/wireless.emergency, which we're assuming you've done
# manually.  In my case, I've dropped all main VLANs and SSIDs except
# for Asio2.4 which all our non-cloud dependent IOT devices talk to,
# and added "$device-24-emergency" and "$device-5-emergency" SSIDs.

upstream1=8.8.8.8
upstream2=1.1.1.1

gw=$( route -n | grep ^0.0.0.0 | awk '{print $2}' )

logfile=/tmp/disable-AP-when-no-internet.log
pidfile=/tmp/disable-AP-when-no-internet.pid

log() {
    if [ $# != 0 ] ; then
        echo "$@" |
            sed 's/^/disable-AP-when-no-internet.sh: /'
    else
        sed 's/^/disable-AP-when-no-internet.sh: /'
    fi | tee >( logger) | while read -r log ; do
        sed "s/^/$( date ): /"
    done
}

ping() {
    fping -q -r 3 -c 3 -p 150 -t 1000 "$@" 2>&1
}


enable_ap() {
    if [ -L /etc/config/wireless ] ; then
        # we had previously disabled the main wireless and implemented our emergency SSID
        date

        log "Enabling normal AP mode"

        rm /etc/config/wireless
        mv /etc/config/wireless.real /etc/config/wireless
        wifi down
        wifi up
    fi
}

disable_ap() {
    if ! [ -L /etc/config/wireless ] ; then
        # we have been running in main wireless mode, so wish to enable our emergency-disable SSID now
        date

        log "Disabling normal AP mode, going into emergency-disable mode"

        mv /etc/config/wireless /etc/config/wireless.real
    elif readlink /etc/config/wireless | grep -q wireless.emergency.isolate ; then
        # we have been running in emergency-isolate wireless mode, but should go into emergency-disable wireless mode
        date

        log "Disabling emergency-isolate AP mode, going into emergency-disable mode"

        rm /etc/config/wireless
    else
        # no change
        return
    fi

    ln -s /etc/config/wireless.emergency.disable /etc/config/wireless
    wifi down
    wifi up
}

isolate_ap() {
    if ! [ -L /etc/config/wireless ] ; then
        # we have been running in main wireless mode, so wish to enable our emergency-isolate SSID now
        date

        log "Disabling normal AP mode, going into emergency-isolate mode"

        mv /etc/config/wireless /etc/config/wireless.real
    elif readlink /etc/config/wireless | grep -q wireless.emergency.disable ; then
        # we have been running in emergency-disable wireless mode, but should go into emergency-isolate wireless mode
        date

        log "Disabling emergency-disable AP mode, going into emergency-isolate mode"

        rm /etc/config/wireless
    else
        # no change
        return
    fi

    ln -s /etc/config/wireless.emergency.isolate /etc/config/wireless
    wifi down
    wifi up
}

rotate_logfile() {
    if [ -e "$logfile" ] ; then
        if [ $( stat -c %s "$logfile" ) -gt $((1024*1024)) ] ; then
            log "Rotating logfile: $logfile -> $logfile.0"
            mv "$logfile" "$logfile.0"
        else
            echo
        fi
    fi
    exec >> $logfile 2>&1
}

if [ -e $pidfile ] ; then
    pid=$( cat $pidfile )
    log "Restarting disable-AP-when-no-internet: $pid"
    ps fup $pid | log
    kill $pid
fi

(
    echo $BASHPID > $pidfile
    log "Started disable-AP-when-no-internet: $BASHPID"
    ps fup $BASHPID | log

    while : ; do
        rotate_logfile
        date
        if ping=$( ping $upstream1 ) || { sleep 1 ; log "first failover ping attempt, $upstream2: $ping" ; ping=$( ping $upstream2 ) ; } ||
                { sleep 1  ; log "first failover pair ping attempt, $upstream1: $ping" ;  ping=$( ping $upstream1 ) ; } || { sleep 1 ;  log "first failover pair ping attempt, $upstream2: $ping" ;          ping=$( ping $upstream2 ) ; } ||
                { sleep 10 ; log "second failover pair ping attempt, $upstream1: $ping" ; ping=$( ping $upstream1 ) ; } || { sleep 10 ; log "second (final) failover pair ping attempt, $upstream2: $ping" ; ping=$( ping $upstream2 ) ; }
           # can afford ~30 seconds outage before triggering
           # failover procedure
        then
            log "succeeded to talking to upstream: $ping"
            enable_ap
        elif ping=$( ping $gw ) || { sleep 1 ; log "failover attempt to gateway before final isolation, $gw: $ping" ; ping=$( ping $gw ) ; } ; then
            log "failed to talk to upstream, but succeeded talking to gateway: $ping"
            disable_ap
        else
            log "failed to talk to gateway: $ping"
            isolate_ap
        fi
        sleep 60
    done
) &
echo -n backgrounding...
sleep 1
echo
