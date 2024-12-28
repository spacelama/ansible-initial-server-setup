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

logfile=/tmp/disable-AP-when-no-internet.log
pidfile=/tmp/disable-AP-when-no-internet.pid

enable_ap() {
    if [ -L /etc/config/wireless ] ; then
        # we had previously disabled the main wireless and implemented our emergency SSID
        date

        echo "Enabling normal AP mode"

        rm /etc/config/wireless
        mv /etc/config/wireless.real /etc/config/wireless
        wifi down
        wifi up
    fi
}

disable_ap() {
    if ! [ -L /etc/config/wireless ] ; then
        # we have been running in main wireless mode, so wish to enable our emergency SSID now
        date

        echo "Disabling normal AP mode, going into emergency mode"

        mv /etc/config/wireless /etc/config/wireless.real
        ln -s /etc/config/wireless.emergency /etc/config/wireless
        wifi down
        wifi up
    fi
}

rotate_logfile() {
    if [ -e "$logfile" ] ; then
        if [ $( stat -c %s "$logfile" ) -gt 10240 ] ; then
            echo "Rotating logfile: $logfile -> $logfile.0"
            mv "$logfile" "$logfile.0"
        else
            echo
        fi
    fi
    exec >> $logfile 2>&1
}

if [ -e $pidfile ] ; then
    pid=$( cat $pidfile )
    echo "Restarting disable-AP-when-no-internet: $pid"
    kill $pid
fi
echo $$ > $pidfile

while : ; do
    rotate_logfile
    date
    if ping -c 1 $upstream1 || ping -c 1 $upstream2 ; then
        enable_ap
    else
        disable_ap
    fi
    sleep 60
done
