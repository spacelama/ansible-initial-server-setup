#!/bin/bash

# source: http://stackoverflow.com/questions/17398636/how-can-i-send-a-mail-from-unix-mutt-client-with-both-html-and-body-in-html https://raw.githubusercontent.com/kaihendry/sg-hackandtell/master/list/maillist

if [ $# != 3 ] ; then
    echo "Usage: $0 <subject> <textfile> <htmlfile>" 1>&2
    exit 1
fi

subject="$1"
textfile="$2"
htmlfile="$3"

PATH=$PATH:/opt/bom/bin:sbin:/usr/sbin ; export PATH


boundary=$(RANDFILE=/dev/null openssl rand -base64 24) # User might not have a writable $HOME, and don't need cryptographic security

(
    cat << END
From: $USER
To: $USER
Subject: $subject
MIME-Version: 1.0
Content-Disposition: inline
Content-Type: multipart/alternative; boundary=$boundary

--$boundary
Content-Type: text/plain; charset=UTF-8

END

    cat "$textfile"

    cat << END

--$boundary
Content-Type: text/html; charset=UTF-8

END

    cat "$htmlfile"

    cat << END
--$boundary--
END
) | sendmail -t

