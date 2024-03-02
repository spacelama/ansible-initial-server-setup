# constructs PATH paying attention to not add anything that is not
# currently quickly accesible over NFS for example

# usage: setpath <PATH|LD_LIBRARY_PATH|...> p1:p2:PATH:p3:... p4 p5 ...
function setpath() {
    env_name="$1" ; shift

    orig_env=$( eval echo \$"$env_name" )
#    echo "setpath(): 1: $env_name = $orig_env" 1>&2
#    echo "setpath(): 2: intending to set: $env_name = $@" 1>&2
    new_env=
#    PATH="/bin:/usr/bin"  # just to have basic commands work during
                          # this function call while we are partially
                          # setting up $PATH
    oldIFS="$IFS"
    newline="
"
    PATHs="${@//:/$newline}"
    for p in $PATHs ; do
#        echo testing "p=$p" 1>&2
        case "$p" in
            $env_name)
                # substitute the original PATH
#                echo testing "found p=PATH" 1>&2
                new_env="$new_env:$orig_env"
                ;;
            *)
            # now see that we can reach the tested path quickly and
#                that we don't have a hanging NFS mount for example
#                echo testing "testing timeout on p=$p" 1>&2
#                if PATH="$p":$PATH timeout -k 1 1 /bin/true ; then
#                    new_env="$new_env:$p"
#                fi
# alternatively, test that the provided path is both a directory and subject to timeout
                case "$HOSTNAME" in
                    pi|DBJXCT3) # very slow machines
                        pathtimeout=2
                        ;;
                    *)
                        pathtimeout=1 # 0.4 - reduce temporarily
                                      # perhaps when we have a known
                                      # fault, but otherwise the
                                      # false-positives are disruptive
                        ;;
                esac
                if timeout -k 1 $pathtimeout [ -d "$p" ] ; then
#                    echo Found good p="$p" 1>&2
                    new_env="$new_env:$p"
                fi
                ;;
        esac
    done
    new_env="${new_env#:}" # strip the leading ":"
#    echo "setting $env_name=\"$new_env\"" 1>&2
    eval $env_name="$new_env"
#    echo "PATH: $PATH" 1>&2
}
