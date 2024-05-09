# Ansible Playbook for setting up a proxmox cluster with some debian and container VMs, some tasmota IOT devices, and some Openwrt Access Points and Routers with VLAN separation of the radios

A couple of [Ansible](http://docs.ansible.com/) playbooks which runs a series of configuration steps to set up an SOE based on Debian, in order to provide a solid foundation for subsequent actions.

It borrows heavily from the work of: [Bryan Kennedy](https://plusbryan.com/my-first-5-minutes-on-a-server-or-essential-security-for-linux-servers), [Ryan Eschinger](http://ryaneschinger.com/blog/securing-a-server-with-ansible/),  [Ashley Rich](https://github.com/A5hleyRich/wordpress-ansible), and [Digital Ocean](https://www.digitalocean.com/community/tutorials/initial-server-setup-with-ubuntu-14-04)

It will perform the following:
* Create a new ansible "super user" with with root privileges and public key authentication on your proxmox and debian machines as well as the containers
* Implement several SSH hardening techniques
* Configure the timezone and enable time synchronization
* Modify the hostname and hosts file
* Install the admin's account with some nice .bashrc settings
* Install a package baseline appropriate for laptops, desktops, virtualisation hosts, containers, as appropriate
* Override some of debian's more annoying defaults
* Manage tasmota settings on your IOT devices
* Manage openwrt settings on your openwrt devices, including installing VLANs 10,30,40,70 and configuring radios on them

Note that it won't set up your OpenWRT APs and tasmota configurations
from scratch - but it does make it easier to wholesale change all of
your SSIDs for example.

## Requirements

* [Ansible](http://docs.ansible.com/ansible/intro_installation.html) installed locally on your machine

Ideally, you'd create a gpg encrypted file in misc/vault-password.gpg,
and verify it can be read with:
misc/get-vault-pass.sh

## Configuration

Clone the repo

```
$ git clone --recurse-submodules https://github.com/spacelama/ansible-initial-server-setup.git
```

# Debian Linux LXC and VMs, desktops, servers, Proxmox etc

Modify the variables in **_vars/main.yml_** according to your needs:

**user:** the username for your new "super user"

**password:** a [hashed](http://docs.ansible.com/ansible/faq.html#how-do-i-generate-crypted-passwords-for-the-user-module) sudo password

**my_public_key:** the local path to your public SSH key that will be authorized on all remote hosts

**domain:** your chosen domain

**hostname:** your chosen hostname

**timezone:** the most appropriate [timezone](https://en.wikipedia.org/wiki/List_of_tz_database_time_zones) for your server

**ssh_port:** your chosen SSH port

Modify **_hosts.yml_** with your various host settings

I put a bunch of vault encoded per-host secrets in
`host_vars/<hostname>.yml` too, encrypted via `ansible-vault
encrypt_string`, eg settings such as `ssh_host_rsa_key`,
`ssh_host_ed25519_key`, `ssh_host_ecdsa_key`, `ssh_local_port`,
`switch_pass`, `root_id_rsa` - `host_vars/` being in .gitignore to
further protect their contents (likewise for `files/main.*.password`
and `files/ap.*` which should have been vault entries in the first
place).  Host settings I want to track in git are in `hosts.yml`.

## Bootstrapping

Install the ansible_adm account and the sudo permissions for this account to escalate to root with:

`$ ansible-playbook bootstrap.yml -u root -k --extra-vars "target=dirac-new" --ask-vault-pass # always run with --check when first starting out!`

Just to renew ssh hostkeys etc, without having to first turn on ssh PermitRootLogin:

`$ ansible-playbook bootstrap.yml -u ansible --extra-vars "target=dirac-new" --ask-vault-pass --become # always run with --check when first starting out!`

Fix up an old installation:

`$ ansible-playbook bootstrap.yml -u tconnors -k --extra-vars "target=maxwell" --ask-vault-pass --become --become-method=su -K # always run with --check when first starting out!`

Fix up an lxc container:

`$ ansible-playbook bootstrap.yml -u root --diff --extra-vars "target=zm" # always run with --check when first starting out!`

## Testing

It's not foolproof, but try `--check` prior to each real ansible.

`--diff` is extremely handy, but not foolproof when also running --check.

I frequently `--limit` to hosts or away from hosts.

`$ ansible-playbook --ask-vault-pass initial_server_setup.yml --diff --check --limit='!dirac-new,!fs-new,!hass-debian,!mail'`

--limit also useful when you get a new openwrt AP or tasmota device:

`$ ansible-playbook -v openwrt_maintenance.yml --diff --check`

`$ ansible-playbook tasmota_maintenance.yml --diff --check --limit patiofluro-power,loungefrontlight-power  --extra-vars "setpsk=true" --extra-vars "setsyslog=true"`

## Production

Then run the playbooks:

`$ ansible-playbook --ask-vault-pass initial_server_setup.yml --diff --limit='!dirac-new,!fs-new,!hass-debian,!mail'`

Likewise for tasmota and openwrt:

`$ ansible-playbook -v openwrt_maintenance.yml --diff`

`$ ansible-playbook tasmota_maintenance.yml --diff --limit patiofluro-power,loungefrontlight-power  --extra-vars "setpsk=true" --extra-vars "setsyslog=true"`

## Using tags to limit the scope of changes

initial_server_setup.yml uses tags on each role.

Limit your changes to only apply webserver and smtp roles with eg:

`$ ansible-playbook --diff initial_server_setup.yml --limit met,webserver,iot --tags webserver,smtp --check`

or to stop the webserver role from running while still running everything else:

`$ ansible-playbook --diff initial_server_setup.yml --limit met,webserver,iot --skip-tags webserver --check`

# Openwrt routers, wireless APs, VLANs

`vars/openwrt.yml` contains some settings for all your openwrt devices
(routers, APs etc), and sets up a bunch of VLANs for your IOT devices,
windows devices etc, assigned per MAC address (VLAN decided by which
SSID your device joins - my IOT devices from China only know about my
IOT SSID, and some of them get a firewall entry that stops them even
talking to the internet, let alone amongst themselves; sorry, firewall
was done through point-and-click, not yet encoded here).  DHCP
reservations set in `roles/openwrt/templates/dhcp.*` and static
hostnames for serving static RR A records.

You'll need to set up `files/ap.mobility_domain
files/ap.wpa2.{default_radio0.psk,default_radio1.psk,wifinet{4,5,6,7,10,11}.{psk,ssid}}`
to contain values for your PSK etc.  hosts.yml knows about some of the
network ssids, so `ap.wpa2.wifinet10.ssid` and
`ap.wpa2.wifinet11.ssid` aren't needed or consulted.

My router required a bunch of manual config (upstream VLANs,
firewalls, banip etc), but I've been using this to configure fresh APs
from scratch.  Have a good backup of your APs before you run this for
the first time though if you've already set them up in any way.  The
radio stuff is expected to be quite fragile, and has only received
most testing on current openwrt 22.03.

Run the playbook to configure all openwrt devices configured in
hosts.yml:

`$ ansible-playbook openwrt_maintenance.yml --diff # --check to verify changes first`

# Tasmota esp8266/esp32 devices

NTP, latitude, longitude, syslog, timezones, SSIDs are encoded in
vars/tasmota.yml.  You'll need to tweak these.

This will set both SSID1 and the fallback SSID2 - here, we set the
first one to be your primary SSID that you mesh or roam between
throughout your host, and SSID2 might be the *second* closest AP to
where your device normally sits.  That way, tasmota will lock onto
(and roam via 80211.r) your closest AP on the primary SSID by default,
but if that AP continues to serve valid wifi connections but loses
connectivity to the network itself, tasmota's watchdog will notice
this loss of packet connectivity, and will failover to the second
closest AP that is hopefully still on a working network.

It will configure all mqtt paths to be a single standard (I don't know
much about mqtt, but my network and home assistant seem happy with my
current settings).

I set `disable_default_reset_on_power_reset7=1` on devices that
frequently lose their power, so they don't accidentally get firmware
reset.

The ansible tasmota provider thinks PSK and syslog change every time
you try to adjust them, even if actually unchanged, so by default, I
don't set them.  They only get attempted to be set when you supply
setpsk=true and setsyslog=true.  When making a mass change after
testing something well, I'll leave off --limit, but when configuring a
new device, I'll use this:

`ansible-playbook tasmota_maintenance.yml --diff --extra-vars "setpsk=true" --extra-vars "setsyslog=true" --limit airmon1`
