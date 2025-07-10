# -*- Mode: shell-script -*-
# shellcheck shell=bash

PATH=$PATH:/root/bin
export PATH

LESS="-i -M -R -P%t?f%f:stdin .?pb%pb\%:?lbLine %lb:?bbByte %bb:-..."
export LESS
