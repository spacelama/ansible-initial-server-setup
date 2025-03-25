We authorize the mount, and then copy the keyring files to files/etc/ceph, and also populate keyring files in mount_ceph_clients in vars/main.yaml

ssh $node "sudo env ceph fs authorize '$mount' client.$user $remotepathauth $rwro" > ~/Ansible/files/etc/ceph/ceph.client.$user.keyring
eg:
ssh pve1 "sudo env ceph fs authorize cephfs-hdd client.cephfs-hdd.template /template rw" > ~/Ansible/files/etc/ceph/ceph.client.cephfs-hdd.template.keyring
ssh pve1 "sudo env ceph fs authorize cephfs-hdd client.cephfs-hdd.media /mounts/media rw" > ~/Ansible/files/etc/ceph/ceph.client.cephfs-hdd.media.keyring

Original creation was done with `add-ceph-mount` before we ansibilised this

We could probably further ansiblise this - the resultant authorisation is set by the files in pve:/etc/pve/priv/ceph/*.secret, *.keyring
