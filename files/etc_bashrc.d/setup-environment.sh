function setup_environment() {
    setup_bash_settings
    setup_aliases
    setup_remote_aliases

    # FIXME: add all the other bash settings we care about, paths, etc

    #  cd `pwd`    #when /home/tconnors points to /home/tconnors-scuzzie, then we want to cd to set the path
    RPWD=$(realpath "$PWD")
    if [ "$RPWD" = $(realpath $HOME) ] ; then
        cd
    fi
}
