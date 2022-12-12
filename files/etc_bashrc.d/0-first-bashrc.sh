function sh_interactive () {
    case "$-" in
       *i*)
          [ dumb != "$TERM" ] && [ -t 0 ]
          return
       ;;
       *)
          return 1
       ;;
    esac
}

#echo 1 1>&2
if ! sh_interactive ; then
#echo 2 1>&2
   PS1='$'
   NONINTERACT=1
elif [ -z "$PBS_ENVIRONMENT" ] ; then
    if [ $TERM = xterm ] ; then
        function winname () { 
            if [ -t 2 ] ; then
                echo -ne  '\033]2;'"$1"'\007' 1>&2
            fi
        }
        function iconname  () { 
            if [ -t 2 ] ; then
                echo -ne  '\033]1;'"$1"'\007' 1>&2
            fi
        }
    else
        function winname () {
            :
        }
        function iconname () {
            :
        }
    fi
    ( winname "Wait... logged into $HOSTNAME and sourcing startup" & )
fi


