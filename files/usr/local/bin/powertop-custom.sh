#!/bin/bash

# Try to turn on as much powersaving as practical on the dell optiplex
# 9020sff (23W -> 18W @ idle) by getting powertop to turn off and
# power-manage all that is feasible
# https://askubuntu.com/questions/678779/powertop-auto-tune-without-messing-with-usb-and-touchpad
# but in my case want to supress the logs every 5 minutes of: Sep 19
# 18:08:30 pve3 kernel: [59453.494335] pcieport 0000:00:1c.4: Enabling
# MPC IRBNCE Sep 19 18:08:30 pve3 kernel: [59453.495795] pcieport
# 0000:00:1c.4: Intel PCH root port ACS workaround enabled


powertop --auto-tune
HIDDEVICES=$(ls /sys/bus/usb/drivers/usbhid | grep -oE '^[0-9]+-[0-9\.]+' | sort -u)
for i in $HIDDEVICES; do
  echo -n "Enabling " | cat - /sys/bus/usb/devices/$i/product
  echo 'on' > /sys/bus/usb/devices/$i/power/control
done

echo 'on' > '/sys/bus/pci/devices/0000:00:1c.4/power/control'
