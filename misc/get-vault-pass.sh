#!/bin/sh
bash P | gpg --passphrase-fd 0 --batch --pinentry-mode loopback --decrypt --quiet "$(dirname $0)/vault-password.gpg"
