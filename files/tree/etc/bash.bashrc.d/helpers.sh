# https://stackoverflow.com/questions/68768860/how-to-print-all-function-calling-orders-in-a-bash-script

# http://wiki.bash-hackers.org/commands/builtin/caller
bt() {
    local frame=0
    while caller $frame; do
        ((frame++));
    done | sed 's/^/line:/'
    echo "$*"
    #  exit 1
}

print_callers() {
  local func file line

  echo 'CALLERS:'

  for i in "${!FUNCNAME[@]}"; do
      # Uncomment next line to skip the current function, i.e., `print_callers`.
      if [[ $i == 0 ]]; then continue; fi
      func=${FUNCNAME[$i]}
      file=${BASH_SOURCE[$i]}
      line=${BASH_LINENO[$i - 1]}
      echo " - $func ($file:$line)"
  done

  echo
} >&2

#creates or appends to a file, either the args supplied or stdin
#otherwise, creating it with permissions supplied (but not changing
#the permissions if the file is merely being appended to and already
#existed)
log_to_file() {
    local behaviour="$1" ; shift
    local umask="$1" ; shift
    local file="$1" ; shift
    # messages = everything else, otherwise stdin

    (
        case $behaviour in
            create)
                command rm -f "$file"
                ;;
            append)
                :
                ;;
            *)
                echo "log_to_file: create|append umask file [<messages>]"
                exit 1
                ;;
        esac
        umask "$umask"

        if [ "$#" = 0 ] ; then
            cat
        else
            echo "$@"
        fi >> "$file"
    )
}



# debugging trace functions
calling() {
    if [ -n "$BASH_DEBUG" ] ; then
        echo "Calling: $@"
#        bt
        print_callers
    fi 1>&2
}

called() {
    if [ -n "$BASH_DEBUG" ] ; then
        echo "Returning from: $@"
#        bt
        print_callers
    fi 1>&2
}
# when finished debugging, can speed up the shell a little by instead setting:
# alias calling=:
# alias called=:
