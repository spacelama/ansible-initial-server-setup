#!/bin/sh
 
# /etc/show_wifi_clients.sh
# Shows MAC, IP address and any hostname info for all connected wifi devices
# written for openwrt 12.09 Attitude Adjustment

# https://openwrt.org/faq/how_to_get_a_list_of_connected_clients
 
echo    "# All connected wifi devices, with IP address,"
echo    "# hostname (if available), and MAC address."
echo -e "# IP address\tname\tMAC address"
# list all wireless network interfaces 
# (for MAC80211 driver; see wiki article for alternative commands)
for interface in `iw dev | grep Interface | cut -f 2 -s -d" "`
do
  # for each interface, get mac addresses of connected stations/clients
  maclist=`iw dev $interface station dump | grep Station | cut -f 2 -s -d" "`
  # for each mac address in that list...
  for mac in $maclist
  do
    # If a DHCP lease has been given out by dnsmasq,
    # save it.
    ip=`cat /proc/net/arp | awk '{print $1, $4}' | grep "$mac$" | awk '{print $1}'`
    host=`nslookup "$ip" | sed -n 's/.*name = //p'`
    # ... show the mac address:
    echo -e "$ip\t$host\t$mac"
  done
done
