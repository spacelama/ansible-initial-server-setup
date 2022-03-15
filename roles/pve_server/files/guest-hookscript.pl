#!/usr/bin/perl

# Exmple hook script for PVE guests (hookscript config option)
# You can set this via pct/qm with
# pct set <vmid> -hookscript <volume-id>
# qm set <vmid> -hookscript <volume-id>
# where <volume-id> has to be an executable file in the snippets folder
# of any storage with directories e.g.:
# qm set 100 -hookscript local:snippets/hookscript.pl

use strict;
use warnings;

print "GUEST HOOK: " . join(' ', @ARGV). "\n";

# First argument is the vmid

my $vmid = shift;

# Second argument is the phase

my $phase = shift;
my $date=`date` ; chomp $date;

#open(FHO, ">> /tmp/hook.log") or die "can't open /tmp/hook.log";
#print FHO "$date: $vmid $phase\n";
#close FHO;

sub guest_set_time {
    open(AGENT, "| socat unix-connect:/var/run/qemu-server/$vmid.qga stdin") or die "can't open agent to $vmid";
    print AGENT '{"execute":"guest-set-time"}';
    close AGENT or die "couldn't close agent to $vmid";
}

if ($phase eq 'pre-start') {

    # First phase 'pre-start' will be executed before the guest
    # ist started. Exiting with a code != 0 will abort the start

    print "$date: $vmid is starting, doing preparations.\n";

    # print "preparations failed, aborting."
    # exit(1);

} elsif ($phase eq 'post-start') {

    # Second phase 'post-start' will be executed after the guest
    # successfully started.

    print "$date: $vmid started successfully.  Will run guest-set-time in case host is resuming from suspend.\n";
    guest_set_time;

} elsif ($phase eq 'pre-stop') {

    # Third phase 'pre-stop' will be executed before stopping the guest
    # via the API. Will not be executed if the guest is stopped from
    # within e.g., with a 'poweroff'

    print "$date: $vmid will be stopped.\n";

} elsif ($phase eq 'post-stop') {

    # Last phase 'post-stop' will be executed after the guest stopped.
    # This should even be executed in case the guest crashes or stopped
    # unexpectedly.

    print "$date: $vmid stopped. Doing cleanup.\n";

} elsif ($phase eq 'pre-suspend') {

    # as a VM is being suspended, we run pre-suspend hook

    print "$date: $vmid suspending.\n";
} elsif ($phase eq 'post-resume') {

    # after a suspended VM is resumed, we run post-resume hook.  Good
    # idea to reset the time if guest tools are installed

    print "$date: $vmid resumed - resetting time.\n";
    guest_set_time;

} else {
    die "$date: got unknown phase '$phase'\n";
}

exit(0);
