#!/usr/bin/env -S echo "This should not be run directly - try using the following command, perhaps configuring \$ERROR_CONTINUE, \$ERROR_PRINT_UNSAFE: \nsource"

# source this from your script that wants backtraces from your error
# handler:

# https://stackoverflow.com/questions/68768860/how-to-print-all-function-calling-orders-in-a-bash-script

#don't know why we'd specify this per the original source: shopt -so noglob
#don't know why we'd specify this per the original source: shopt -s extglob
#shopt -so errexit
shopt -so errtrace
shopt -so nounset
shopt -so pipefail
if ! [ -t 2 ] ; then
    # disable our STDERR colouring
    function tput() {
        :
    }
fi

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

    local nl=$'\n'
    local FAILED_COMMAND=
    if [ -n "${ERROR_PRINT_UNSAFE:-}" ] ; then
        # Imagine you've failed on this line:
        # openbracket='$(' ; closebracket=')' ; echo baaz & echo foo ; echo bar ; ls "$a $b" "|" "$c" `echo eee` $(echo fff) $( echo $a $b ; echo $b & echo $a ) $openbracket eval echo c d $closebracket `echo "$b $a"` > /tmp/bbbb.log

        # We want to eval it to expand all the variables which can
        # appear in a number of different formats, but otherwise
        # render all the shell special characters safe.  This is
        # merely best efforts - don't use it on code you have no
        # control over, takes user input, etc.  Merely a debugging aid
        # to your own scripts.

        local SLIGHTLY_SAFENED_FAILED_COMMAND=$( echo "$cmnd" | sed 's/\([`()&;<>]\)/\\\1/g' )
        FAILED_COMMAND="$nl>$(eval echo "$SLIGHTLY_SAFENED_FAILED_COMMAND")"
    fi
    echo "Exit status $(bold "$code") for command:$nl $(bold "$cmnd$FAILED_COMMAND")"

    echo "${BASH_SOURCE[1]}:${BASH_LINENO[0]}:"
    print_context "${BASH_SOURCE[1]}" "${BASH_LINENO[0]}"

    for i in "${!FUNCNAME[@]}"; do
        if [[ $i == 0 ]]; then continue; fi
        local func=${FUNCNAME[$i]}
        local file=${BASH_SOURCE[$i]}
        local line=${BASH_LINENO[$i - 1]}

        if (( i == ${#FUNCNAME[@]} - 1 )); then
            local level='└─'
            func="<$func>"
        else
            local level='├─'
        fi

        echo " $level $(bold "$func") ($file:$line)"
    done

    if [ -n "${ERROR_CONTINUE:-}" ] ; then
        echo Continuing...

        exit=$code # but save the exit code into a variable the caller can use
    else
        exit $code
    fi
} >&2
trap ERR ERR
