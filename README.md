# Ansible Playbook for Proxmox, Debian (LXC) Containers, VMs, Physical nodes, Tasmota IOT, OpenWRT Routers and Access Points

## Introduction

### What are we?

We're a couple of [Ansible](http://docs.ansible.com/) playbooks which
runs a series of configuration steps to set up a SOE based on Debian,
in order to provide a solid foundation for subsequent actions outside
the control of these playbooks.  We can provide optional ***(partial)
configuration of a Proxmox cluster, manage Debian (LXC) containers,
VMs, and physical hardware (including some hardware management),
configuration of Tasmota IOT devices, and some OpenWRT Access Points
and Routers configurable with VLAN separation of the SSIDs on each of
the radios***.

It borrows heavily from the work of: [Luke Harvey](https://github.com/lukeharvey), [Bryan Kennedy](https://plusbryan.com/my-first-5-minutes-on-a-server-or-essential-security-for-linux-servers), [Ryan Eschinger](http://ryaneschinger.com/blog/securing-a-server-with-ansible/),  [Ashley Rich](https://github.com/A5hleyRich/wordpress-ansible), and [Digital Ocean](https://www.digitalocean.com/community/tutorials/initial-server-setup-with-ubuntu-14-04)

It will perform the following:
* Setup (not initial deploy)/manage Debian 10 (Buster), 11 (Bullseye), 12 (Bookworm) (and Proxmox 6,7,8) physical machines, LXC and VM (Proxmox) instances
* Create a new Ansible "super user" with with root privileges and public key authentication on your Proxmox and Debian machines as well as the containers
* Set root user credentials, and a primary user credentials, ssh keys, sudoers to root
* Implement several SSH hardening techniques
* Configure the timezone and enable time synchronization
* Modify the hostname and hosts file
* Install the admin's account with some nice .bashrc settings
* Install a package baseline appropriate for laptops, desktops, virtualisation hosts, containers, as appropriate
* Override some of Debian's more annoying defaults per my opinionated opinion
* Manage Tasmota settings on your IOT devices
* Manage OpenWRT settings on your OpenWRT devices, including installing VLANs 10,30,40,70 and configuring radios on them

## What aren't we?

This playbook does not deploy Proxmox from scratch.  It also doesn't
require Proxmox - but I find it very helpful.  It can convert an
existing Proxmox cluster into being managed like any of our other
Debian SOE's (as far as our SOE interaction is concerned - user
accounts, keys, SOE configuration, backups, parts of networking;
however our playbooks don't do Proxmox management on the Proxmox
cluster).  Likewise, we don't rely on any aspect of that (I do it,
because I want my helpful tools to also be used on my Proxmox
cluster).  It doesn't deploy new VMs/CTs via Proxmox yet - you still
need to deploy them some other means, or manually.  We also don't
preclude you doing management outside these playbooks.  And of course,
not practicing what I preach, I don't have a properly thought out dev
environment nor have any automated testing infrastructure, because I
ain't doing anything important here.  It doesn't manage every aspect
of machines - plenty of my machines can be redeployed from scratch and
will come up identically because their OS settings are managed via
these plays, and I have otherwise bind mounted their data into their
LXC containers, but someone had to populate that data and application
configuration in the first place.  And the Proxmox cluster, Ceph
installation and configuration get managed the Proxmox way.  But Ceph
secrets distribution and mounting on clients?  We can manage that here.

While it can manage OpenWRT installations, it similarly isn't going to
deploy OpenWRT from scratch (although we can now apply a sysconfig
update and keep your firmware versions encoded into our config!), and
you'll want to set up initial networking, switching, wifi radio, etc
at initial installation time, but we come along and install our
packages and configs then tweak those UCI settings per our config.

Similarly story for Tasmota - very few of my devices have had their
GPIOs programmed through these playbooks, for example.  But every time
I shift an AP around, I update the relevant primary/failover SSID of
all my Tasmota devices.  It wouldn't be much work to arrange firmware
to be patched like in the OpenWRT playbook either (makes me more
nervous, but is it any harder to open up a bricked 240v smart plug
with triangle security screws vs a bricked TP-Link Archer D7 v1 held
together with fragile plastic clips?).

We are not DRY - many configs have to be specified all over the place.
Sorry.

## What can we automate, what point-and-drool do we still need to do?

For physical boxes, VMs and LXC containers (both provided through
Proxmox in my case), I rely on the machine being deployed the
traditional manual way (haven't yet automated talking to the Proxmox
API, even though the Proxmox boxes themselves are managed in this
playbook like any other Debian box).  We take this vanilla Debian
install where I've just clicked through the install ISO steps in a
fairly braindead fashion (I haven't setup Debian fully-automated, nor
preseed or anything like that; our playbooks goes and then fixes a
bunch of things asked in install anyway), apply bootstrap.yml first,
then initial_server_config.yml to bring the configuration up to my SOE
and upon any further update to my config or after the machine is
patched.  I don't automate the entire playbook being regularly played
or setup automated drift-detection, because I still have far too many
pets and manual changes that need checking, and the playbook is quite
slow to run all the way through, so I'm frequently using `--diff`,
`--check` and `--tags` to limit the scope of changes to what I'm
currently concerned with.  It would be easy to email myself an output
of `--check --diff initial_server_config.yml` every night, but I get
enough email as-is.

For OpenWRT APs and Tasmota configurations however, we still rely on
some manual configuration for a new device from scratch before
applying these playbooks - but it does make it easier to wholesale
change all of your SSIDs for example.  For upgrades however, we can
automate all of the tasks needed to bring your device back into
compliance with your config without any manual configuration.  I apply
it routinely after any minor upgrade of the OpenWRT device, perhaps
checking with `--diff` `--check` first if I'm a little nervous.  I do
test on my VMs first (sometimes I'm even sensible enough to test on
one of my virtualised APs before I test it on my internet gateway),
and take a snapshot manually beforehand.  For major upgrades, it
hasn't broken majorly for me yet, but my planning tends to be a little
more careful around these events.  For setting up a new OpenWRT AP, my
manual configuration tends to be limited to setting switch vlan
tagging information in /etc/config/network, and assigning radio0 and
radio1 consistently with my other devices, before letting
uci_config.yml loose on VLAN and network definitions and what SSIDs
we've assigned to which radios.  This sometimes involves swapping
radio0 and radio1 pci/hardware devices in /etc/config/wireless.

# Requirements

* [Ansible](http://docs.ansible.com/ansible/intro_installation.html) installed locally on your machine to run the playbooks.  Tested on Debian's Ansible from Debian 10 (Buster) to Debian 12 (Bookworm)
* You probably want to install `ansible-mitogen` (and `python3-mitogen`) on your Ansible server too, for my ansible.cfg sets `strategy = mitogen_linear` to greatly accelerate the playbook (it works with that setting disabled if you can't install migoten, but mitogen has never created any detectable problems for me).  I have only tested this from a Debian machine (Debian 11,12).
* Ideally, you'd create a gpg encrypted file in misc/vault-password.gpg, and verify it can be read with: misc/get-vault-pass.sh
* OpenWRT plays rely on [ansible-openwrt](https://github.com/gekmihesg/ansible-openwrt), which is published as a [galaxy collection](https://galaxy.ansible.com/ui/repo/published/nn708/openwrt/).
* Tasmota plays rely on [ansible-tasmota](https://github.com/tobias-richter/ansible-tasmota), which is available through [ansible galaxy](https://galaxy.ansible.com/ui/standalone/roles/tobias_richter/tasmota/), but which [I had modified](https://github.com/spacelama/ansible-tasmota) (but which got merged into upstream) to allow for and transparently recovers from the Tasmota device spontaneously rebooting after certain configurations are applied.
* The SMTP plays setup DKIM signatures via [ansible-dkim](https://github.com/FoxyRoles/ansible-dkim), which is published through [ansible galaxy](https://galaxy.ansible.com/ui/standalone/roles/sunfoxcz/dkim/), but which [I've modified](https://github.com/spacelama/ansible-dkim) and included in this repo to allow for other opendkim parameters I need.

# Initial playbook setup and Configuration

Clone the repo

```
$ git clone --recurse-submodules https://github.com/spacelama/ansible-initial-server-setup.git
```

## Configure Debian Linux LXC and VMs, desktops, servers, Proxmox etc

Look at all the .gitignore files scattered around for an indication as
to what secrets you may need.  Populate those files with secrets
properly encoded with the vault.

Modify the variables in **_vars/main.yml_** according to your needs:

**user:** the username for your new "super user"

**password:** a [hashed](http://docs.ansible.com/ansible/faq.html#how-do-i-generate-crypted-passwords-for-the-user-module) sudo password

**my_public_key:** the local path to your public SSH key that will be authorized on all remote hosts

**domain:** your chosen domain

**hostname:** your chosen hostname

**timezone:** the most appropriate [timezone](https://en.wikipedia.org/wiki/List_of_tz_database_time_zones) for your server

**ssh_port:** your chosen SSH port

Modify **_hosts.yml_** with your various host settings (and public keys etc).

I put a bunch of vault encoded per-host secrets in
`host_vars/<hostname>.yml` too, encrypted via `ansible-vault
encrypt_string`, eg settings such as `ssh_host_rsa_key`,
`ssh_host_ed25519_key`, `ssh_host_ecdsa_key`, `ssh_local_port`,
`switch_pass`, `root_id_rsa` - `host_vars/` being in .gitignore to
further protect their contents (likewise for `files/main.*.password`
and `files/ap.*` which should have been vault entries in the first
place).  Host settings I want to track in git are in `hosts.yml`.

## Bootstrapping your new LXC/VM/Physical node to be Ansible managed

Install the Ansible_adm account and the sudo permissions for this account to escalate to root with:

`$ ansible-playbook bootstrap.yml -u root -k --extra-vars "target=dirac-new" --ask-vault-pass # always run with --check when first starting out!`

Just to renew ssh hostkeys etc, without having to first turn on ssh PermitRootLogin:

`$ ansible-playbook bootstrap.yml -u ansible --extra-vars "target=dirac-new" --ask-vault-pass --become # always run with --check when first starting out!`

Fix up an old installation:

`$ ansible-playbook bootstrap.yml -u tconnors -k --extra-vars "target=maxwell" --ask-vault-pass --become --become-method=su -K # always run with --check when first starting out!`

Fix up an lxc container:

`$ ansible-playbook bootstrap.yml -u root --diff --extra-vars "target=zm" # always run with --check when first starting out!`

## Testing

It's not foolproof, but try `--check` prior to each real ansible invocation.

`--diff` is extremely handy, but not foolproof when also running --check.

I frequently `--limit` to hosts or away from hosts.

`$ ansible-playbook --ask-vault-pass initial_server_setup.yml --diff --check --limit='!dirac-new,!fs-new,!hass-debian,!mail'`

--limit also useful when you get a new OpenWRT AP or Tasmota device:

`$ ansible-playbook -v openwrt_maintenance.yml --diff --check`

`$ ansible-playbook tasmota_maintenance.yml --diff --check --limit patiofluro-power,loungefrontlight-power --extra-vars "setpsk=true" --extra-vars "setsyslog=true"`

# Production

Then run the playbooks:

`$ ansible-playbook --ask-vault-pass initial_server_setup.yml --diff --limit='!dirac-new,!fs-new,!hass-debian,!mail'`

Likewise for Tasmota and OpenWRT:

`$ ansible-playbook -v openwrt_maintenance.yml --diff`

`$ ansible-playbook tasmota_maintenance.yml --diff --limit patiofluro-power,loungefrontlight-power --extra-vars "setpsk=true" --extra-vars "setsyslog=true"`

# Using tags to limit the scope of changes

initial_server_setup.yml uses tags on each role.

Limit your changes to only apply webserver and smtp roles with eg:

`$ ansible-playbook --diff initial_server_setup.yml --limit met,webserver,iot --tags webserver,smtp --check`

or to stop the webserver role from running while still running everything else:

`$ ansible-playbook --diff initial_server_setup.yml --limit met,webserver,iot --skip-tags webserver --check`

# Ad hoc commands:

Using [ansbak](https://github.com/spacelama/ansbak), you can get an
analogous result to using `pdsh ... | dshbak -c`, using your same
Ansible inventory as usual (such that this has become my main way of
interacting with my fleet instead of pdsh or cssh):

`$ ANSIBLE_FORCE_COLOR=true ansible laptops,desktops,servers,containers -i ~/Ansible/hosts.yml -m shell -a "sudo env bash -c 'dpkg --get-selections | grep munin'" | ansbak.py`


# OpenWRT routers, wireless APs, VLANs

`vars/openwrt.yml` contains some settings for all your OpenWRT devices
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
most testing on current OpenWRT 22.03.*, 23.05.*, 24.10.*.

My inventory is in hosts.yml, and tells us whether the OpenWRT device
uses DSA switch config or the old definition, via
`openwrt_dsa_switch_config`.  IP address are decided by
`inet_addr_suffix` in your inventory to assign
`192.168.{{interface}}.{{inet_addr_suffix}}` (where `interface` is
decided by uci_config.yml per VLAN).  We might set
`openwrt_heavy_installation: false` for devices with particularly
small flash (but I was able to NFS mount a fileserver to manually
offload the biggest of non-essential packages from even my smallest
wavlink wn575a3 with 8MB of flash).  `type` is 'ap' or 'router' and
decided which packages to install and how to set up DHCP.

Run the playbook to configure all OpenWRT devices configured in
hosts.yml:

`$ ansible-playbook openwrt_maintenance.yml --diff # --check to verify changes first`

By default, since they're expensive to run (and when experimenting,
can be more fragile) we leave some UCI settings alone (so we can just
quickly install packages, update files, etc) unless told otherwise:

`$ ansible-playbook openwrt_maintenance.yml --diff --extra-vars "run_uci_config=yes"`

Conversely, if you're just changing UCI config params, then shortcut
the package installation and file sync with:

`$ ansible-playbook openwrt_maintenance.yml --diff --extra-vars "run_uci_config=yes" --extra-vars "run_packages_config=no" --tags openwrt_config`

We can also issue firmware patches, and then run the configuration
update step again (which assumes run_uci_config=yes, which of course
is more fragile over major version updates):

`$ ansible-playbook openwrt_upgrade.yml --diff`

Firmware files are assumed to be kept under
roles/openwrt/files/firmware/, which in my case contains symlinks back
to where I keep the firmware images.  Update `firmware_image` in
hosts.yml and run this playbook whenever you want to issue an upgrade.
It will run across all devices in your inventory unless you --limit
them, so long as they have firmware_image set.  If unset for a
particular device, that device never gets patched by this playbook.

Keep in mind this playbook is much more likely to fail than the
OpenWRT_maintenance one, because who knows what assumptions we make or
your configuration makes that will break in each update (be careful of
migrations to DSA switches, as always), so keep firm backups (or be
prepared to run the playbook multiple times until you get it right).
But in this play, devices are serialised so you never have more than
one device out at a time, and failure halts the play so shouldn't
extend beyond more than one device if not --limited.

There's also a playbook just to run single shell commands from the commandline:

`$ ansible-playbook openwrt_shell.yml -e "cmd='id'"`

Because it has to set up the gekmihesg.openwrt dependent stuff and
isn't an ad-hoc command that can be parsed by ansbak, it's not as
useful as the ansbak example outlined above, so I usually still resort
to dshbak:

`$ pdsh -w $(nodeattr -c openwrt ) 'colordiff -ub /etc/config/wireless*' | dshbak -c`

which uses a different inventory (but one which I still maintain
through this playbook).

# Tasmota esp8266/esp32 devices

NTP, latitude, longitude, syslog, timezones, SSIDs are encoded in
vars/tasmota.yml.  You'll need to tweak these.

This will set SSID1 and you might choose fallback SSID2 per host in
your hosts.yml inventory - here, we set the first one to be your
primary SSID that you mesh or roam between throughout your host, and
SSID2 might be the *second* closest AP to where your device normally
sits.  That way, Tasmota will lock onto (and roam via 80211.r) your
closest AP on the primary SSID by default, but if that AP continues to
serve valid wifi connections but loses connectivity to the network
itself, Tasmota's watchdog will notice this loss of packet
connectivity, and will failover to the second closest AP that is
hopefully still on a working network.

It will configure all mqtt paths to be a single standard (I don't know
much about mqtt, but my network and home assistant seem happy with my
current settings).

I set `disable_default_reset_on_power_reset7=1` on devices that
frequently lose their power, so they don't accidentally get firmware
reset.

The Ansible Tasmota provider thinks PSK and syslog change every time
you try to adjust them, even if actually unchanged, so by default, I
don't set them.  They only get attempted to be set when you supply
setpsk=true and setsyslog=true.  But since writing that, I've set a
lot more parameters, some of which unconditionally overwrite the
setting even when unchanged, and some of these result in the Tasmota
device rebooting every time.  I've had to put a [workaround in
ansible-tasmota](https://github.com/spacelama/ansible-tasmota) to
allow the device to recover and continue setting subsequent
parameters.

When making a mass change after testing something well, I'll leave off
--limit, but when configuring a new device, I'll use this:

`ansible-playbook tasmota_maintenance.yml --diff --extra-vars "setpsk=true" --extra-vars "setsyslog=true" --limit airmon1`
