# https://stackoverflow.com/questions/68768860/how-to-print-all-function-calling-orders-in-a-bash-script

print_callers() {
  local func file line

  echo 'CALLERS:'

  for i in "${!FUNCNAME[@]}"; do
    # Uncomment next line to skip the current function, i.e., `print_callers`.
    # if [[ $i == 0 ]]; then continue; fi
    func=${FUNCNAME[$i]}
    file=${BASH_SOURCE[$i]}
    line=${BASH_LINENO[$i - 1]}
    echo " - $func ($file:$line)"
  done

  echo
} >&2
