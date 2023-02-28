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

## Configuration

Clone the repo

```
$ git clone https://github.com/spacelama/ansible-initial-server-setup.git
```

Modify the variables in **_vars/main.yml_** according to your needs:

**user:** the username for your new "super user"

**password:** a [hashed](http://docs.ansible.com/ansible/faq.html#how-do-i-generate-crypted-passwords-for-the-user-module) sudo password

**my_public_key:** the local path to your public SSH key that will be authorized on all remote hosts

**default_domain:** your chosen domain

**hostname:** your chosen hostname

**timezone:** the most appropriate [timezone](https://en.wikipedia.org/wiki/List_of_tz_database_time_zones) for your server

**ssh_port:** your chosen SSH port

Modify **_hosts.yml_** with your various host settings

## Testing

It's not foolproof, but try `--check` prior to each real ansible run

`$ ansible-playbook --ask-vault-pass initial_server_setup.yml --diff --check --limit='!dirac-new,!fs-new,!hass-debian,!mail'`
`$ ansible-playbook -v openwrt_maintenance.yml --diff --check`
`$ ansible-playbook tasmota_maintenance.yml --diff --check --limit patiofluro-power,loungefrontlight-power  --extra-vars "setpsk=true" --extra-vars "setsyslog=true"`

## Production

Then run the playbooks:

`$ ansible-playbook --ask-vault-pass initial_server_setup.yml --diff --limit='!dirac-new,!fs-new,!hass-debian,!mail'`
`$ ansible-playbook -v openwrt_maintenance.yml --diff`
`$ ansible-playbook tasmota_maintenance.yml --diff --limit patiofluro-power,loungefrontlight-power  --extra-vars "setpsk=true" --extra-vars "setsyslog=true"`

