#!/bin/sh

LESS= ; export LESS

if [ "$1" = --keepproxy ] ; then
    shift
    reload=
else
    unset http_proxy ftp_proxy WWW_http_GATEWAY
    reload=-reload
fi

lynx $reload -mime_header "$@" 2>&1 | less
