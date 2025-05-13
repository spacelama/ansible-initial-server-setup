setup_bash_settings() {
#    echo setup_bash_settings 1>&2
    setup_bash_history_settings

    ulimit -c unlimited 2>/dev/null
    ulimit -S -c 0   #soft core limit. Can be increased by user

    #    echo I hate `date +"%A"`s.
    #    GLOBIGNORE='.*'   # man bash shows that . and .. are ignored with this set, but dotfiles have to be explicitly included since dotglob is then turned off (FIXME: but then .* can't be globbed explicitly - want something that .* excudes . and .., but "*" doesn't include .*
    set -o noclobber
    set -b                 #job control: notify as soon as a program gets a signal
    shopt -s checkwinsize  #when a program exits, and control is returned to the shell, we want it to check the window size again
    shopt -s checkhash     #checks hash for existance so that still works if files moved

    shopt -s no_empty_cmd_completion #good for pasting in code from scripts?
}
