#!/usr/bin/env -S echo "This should not be run directly - try using the following command, perhaps configuring \$ERROR_CONTINUE and \$ERROR_PREPEND: \nsource"

# source this from your script that wants backtraces from your error
# handler:

# https://stackoverflow.com/questions/68768860/how-to-print-all-function-calling-orders-in-a-bash-script

#don't know why we'd specify this per the original source: shopt -so noglob
#don't know why we'd specify this per the original source: shopt -s extglob
#shopt -so errexit
shopt -so errtrace
shopt -so nounset
shopt -so pipefail

bold() {
  tput bold; echo -n "$@"; tput sgr0
}

print_context() {
    # https://news.ycombinator.com/item?id=44666984
    awk 'NR>L-4 && NR<L+4 { printf "%-5d%3s%s\n",NR,(NR==L?">>>":""),$0 }' L="$2" "$1"
}

ERR() {
  local code=$?
  local cmnd=$BASH_COMMAND
  local func file line level

  echo "ERROR: $hostname:$0"

  echo "Exit status $(bold "$code") for command: $(bold "$cmnd")"

  echo "${BASH_SOURCE[1]}:${BASH_LINENO[0]}:"
  print_context "${BASH_SOURCE[1]}" "${BASH_LINENO[0]}"

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

  if [ -n "${ERROR_CONTINUE:-}" ] ; then
      echo Continuuing...
      exit=$code # but save the exit code into a variable the caller can use
  else
      exit $code
  fi
} >&2
trap ERR ERR
