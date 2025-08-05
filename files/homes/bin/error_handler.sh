#!/usr/bin/env bash

# source this from your script that wants backtraces from your error
# handler:

# https://stackoverflow.com/questions/68768860/how-to-print-all-function-calling-orders-in-a-bash-script

shopt -so errexit
shopt -so errtrace
shopt -so noglob
shopt -so nounset
shopt -so pipefail
shopt -s extglob

bold() {
  tput bold; echo -n "$@"; tput sgr0
}

trap ERR ERR; ERR() {
  local code=$?
  local cmnd=$BASH_COMMAND
  local func file line level

  echo "Exit status $(bold "$code") for command: $(bold "$cmnd")"

  for i in "${!FUNCNAME[@]}"; do
    if [[ $i == 0 ]]; then continue; fi
    func=${FUNCNAME[$i]}
    file=${BASH_SOURCE[$i]}
    line=${BASH_LINENO[$i - 1]}

    if (( i == ${#FUNCNAME[@]} - 1 )); then
      level='└─'
      func="<$func>"
    else
      level='├─'
    fi

    echo " $level $(bold "$func") ($file:$line)"
  done

  exit 1
} >&2
