#!/bin/bash

# invoked by rsyslogd.conf.d/syslog-collector.conf to parse out the IP
# address of the syslog sender and maps it to a short-hostname (not
# taking what the sender itself is claiming to be the hostname)

# Declare reverse DNS cache
declare -A cache

dest_dir=/var/log/remote/
mkdir -p "$dest_dir"
chmod 755 "$dest_dir"

while IFS='|' read -r ip msg; do
    # Sanity check
    [[ -z "$ip" || -z "$msg" ]] && continue

    # If we haven't cached the hostname
    if [[ -z "${cache[$ip]}" ]]; then
        # Try reverse DNS
        host=$(dig +short -x "$ip" | sed 's/\.$//' | cut -d. -f1)
        # Fallback to IP if rDNS fails (but don't cache this negative
        # lookup)
        if [[ -z "$host" ]] ; then
            host="$ip"
        else
            cache["$ip"]="$host"
        fi
    else
        host="${cache[$ip]}"
    fi

    short="$host"
    logfile="${dest_dir}/${short}.log"

    # Append message to appropriate host log
    echo "$msg" >> "$logfile"
done
