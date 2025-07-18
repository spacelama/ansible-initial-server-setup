# -*- Mode: shell-script -*-
# shellcheck shell=bash

function warn_profile_path_not_allowlisted() {
    echo "Caution: '$*' hasn't been allowlisted or blocked from setup_profile_files()" 1>&2
}
function blocking_of_sourcing() {
    echo "WARNING: '$*' has been blocked from setup_profile_files()" 1>&2
}

function setup_profile_files() {
    declare -A etc_profile_allowlist=(
        [apps-bin-path]=true
        [bash_completion]=true
        [flatpak]=true
        [gawk]=true
        #[gnome-session_gnomerc]=blocking_of_sourcing
        #[gnome-session_gnomerc]=false
        [gnome-session_gnomerc]=true
        [vte-2.91]=true
    )

    local i=
    for i in /etc/profile.d/*.sh ; do
        if [ -e "$i" ] ; then
            local file_base="${i#/etc/profile.d/}"
            file_base="${file_base%.sh}"

            allowlisted="${etc_profile_allowlist[$file_base]:-warn_profile_path_not_allowlisted}"

            if $allowlisted "$i" ; then
                # echo sourcing "$i" 1>&2
                #local e_b=$( env )
                . "$i"
                #local e_a=$( env )
                #colordiff -ub <( echo "$e_b" ) <( echo "$e_a") || echo "env: modified by $i"
#            else
#                echo blocking of sourcing of "$i" 1>&2
            fi
        fi
    done
}

# add all the other bash settings we care about, paths, etc, that
# would normally have gone in .bash_profile
function setup_environment() {
#    print_callers

    #UID=`uid` ; export UID   #already set by bash readonly
    USER=`id_username` ; export USER

    PTS=`readlink /dev/fd/0` ; PTS="${PTS#/dev/}"

    # No matter what shell we've been given, make sure we override it
    # to bash if available.  Can't simply rely on "bash" and path
    # expansion, because ~/.ssh/config exec flags needs to execute it
    # with an explicit pathname
    [ -x /bin/bash ] && export SHELL=/bin/bash

    #need to do this before quiting, because the original bash session might not have had a DISPLAY
    if [ -n "$PBS_ENVIRONMENT" ] ; then
        unset DISPLAY
    fi

    if [ -z "$BASH_SESSION_PID" ] ; then
        export BASH_SESSION_PID=$$
    fi

    export GOOGLE=google.com.au   #can be overridden by .pre files if eg google have blocked access to some institution
    export GOOGLEPRE=search

    SYSTEM=`uname -s`-`uname -m | sed 's/ /_/g'` ; export SYSTEM
    KERNEL=`uname -r | sed 's/-.*//'` ; export KERNEL
    OS=`uname -s` ; export OS

#    echo OS=$OS 1>&2

#    if [ -z "$NONINTERACT" -a -f /etc/bash_completion -a -z "$BASH_COMPLETION" ]; then
#        [ -e /etc/profile.d/bash_completion.sh ] && . /etc/profile.d/bash_completion.sh
#    fi

#    case "$SYSTEM" in
#        Darwin*)
#            stty erase ^?
#        ;;
#        Linux*)
#            stty erase ^?      #????!  Never needed this before AATPC2, and this has broken things in the past, but it seems that even logging into scuzzie from aatpc2 sets this
#        ;;
#        SunOS*)
# #           stty istrip
#            stty erase ^?
#            alias ps='/usr/ucb/ps'
#        ;;
#        *)
#            :
#        ;;
#     esac

    setpath PATH "/usr/lib/ccache:/usr/lib/colorgcc:/usr/local/bin:/usr/bin:/bin:/usr/games"
    setpath PATH $HOME/bin:$HOME/bin/$SYSTEM-$KERNEL:$HOME/bin/$SYSTEM:$HOME/software/$SYSTEM/bin:PATH:$HOME/.local/bin  #just set up an initial path with the essential private directories, to bootstrap ourselves -- the real paths are set in the .bash_profile.* files
    export PATH

    #LD_LIBRARY_PATH is always removed by xterm, because xterm is suid. So set it to a saved version
    if [ -n "$LD_LIBRARY_PATH_SAVE" ] ; then
        export LD_LIBRARY_PATH=$LD_LIBRARY_PATH_SAVE
    fi
    if [ -n "$LD_PRELOAD_SAVE" ] ; then
        export LD_PRELOAD=$LD_PRELOAD_SAVE
    fi

    if [ -z "$NONINTERACT" ] && [ -n "$PS1" ] ; then
        # set -xv
        setup_bash_settings
        setup_aliases
        setup_remote_aliases
        setup_profile_files
        # set +xv
        #        setup_cmd
    fi

    if [ "$OS" != SunOS  ] ; then
        #  if [ -e /etc/redhat-release ] ; then  #I wonder if rucking fedhat will ever fix their buggy shite?
        #      #utf=.UTF-8
        #      :
        #  else
        #utf=.utf8
        utf=.UTF-8
        #  fi
    else
        utf=
    fi

    export utf

    unset LC_COLLATE LC_MESSAGES LC_TIME LANG LANGUAGE
    if LANG=en_AU$utf perl -e 1 2>&1 | grep warning >/dev/null ; then
        export LC_COLLATE=POSIX
        export LC_TIME=POSIX
        #    echo bland
    else
        #LANG=en_AU.ISO8859-1 ; export LANG
        #LANG=en_AU ; export LANG  #still bad ls -lA listings

        export LANG=en_AU$utf
        export LC_COLLATE=POSIX
        export LC_MESSAGES=en_AU$utf
        export LC_TIME=POSIX
        #    export LC_TIME=en_AU$utf
        #    export LC_TIME=en_DK$utf
        #export LANG=en_AU LC_ALL=en_AU
        #    echo not bland
    fi

    export LONGHOST=$(
        set -o pipefail
        ( programexists byname && timeout 0.4 byname `hostname` 2>/dev/null | sed -n 's/canonical name: //p' | grep . ) ||
            hostname -f 2>/dev/null || hostname   #might fail if hostname is really stuffed, timeout, and we havent yet compiled byname...
           )
    export REALHOST=$(
        set -o pipefail
        ( programexists byname && timeout 0.4 byname `timeout 0.4 ipaddr` 2>/dev/null | sed -n 's/canonical name: //p' | grep . ) ||
            hostname -f 2>/dev/null || hostname   #might fail if hostname is really stuffed, timeout, and we havent yet compiled byname...
           )
    export SHORTHOST=$(echo $LONGHOST | sed 's/\..*//')
    export HOSTNAME=$SHORTHOST

    if [ $SHORTHOST = ds4 ] ; then
        umask 077 # too sensitive to have otherwise
    else
        umask 022
    fi

    #    set -xv
    setpath PATH PATH:/usr/local/mozilla/:\
            /usr/libexec/xscreensaver:/usr/lpp/dx/bin:/usr/sbin:/sbin

    setpath LD_LIBRARY_PATH LD_LIBRARY_PATH:$HOME/software/$SYSTEM/lib
    #    set +xv

    case "$SYSTEM" in
        Linux-sparc|Linux-armv6l)
            #FIXME:  Hopefully this is only sparc32
            export SLOWMACHINE=true
            ;;
        SunOS*)
            OLDPATH=$PATH
            setpath PATH $HOME/bin:$HOME/bin/$SYSTEM:$HOME/software/$SYSTEM/bin
            setpath PATH PATH:/opt/csw/bin:/opt/sfw/bin
            setpath PATH PATH:/opt/csw/xemacs/bin:/opt/csw/gcc4/bin:/opt/csw/gcc3/bin
            setpath PATH PATH:/opt/SUNWspci/bin:/opt/SUNWspro/bin
            setpath PATH PATH:/usr/sfw/bin:/usr/local/bin
            setpath PATH PATH:/usr/openwin/bin:/usr/dt/bin
            setpath PATH PATH:/opt/sfw/gcc-3/bin:/usr/ccs/bin
            setpath PATH PATH:/usr/xpg4/bin:/usr/bin:/usr/sbin:/usr/ucb
            setpath PATH PATH:/usr/dt/appconfig/sdtvolctl
            setpath PATH PATH:/opt/Adobe/Acrobat7.0/Reader/sparcsolaris/bin
            setpath PATH PATH:$OLDPATH

            OLDMANPATH=$MANPATH
            setpath MANPATH $HOME/software/$SYSTEM/share/man
            setpath MANPATH MANPATH:/opt/csw/share/man:/opt/sfw/man
            setpath MANPATH MANPATH:/opt/SUNWspci/man:/opt/SUNWspro/man
            setpath MANPATH MANPATH:/usr/sfw/share/man:/usr/local/share/man
            setpath MANPATH MANPATH:/usr/openwin/share/man:/usr/dt/share/man
            setpath MANPATH MANPATH:/opt/sfw/gcc-3/man:/usr/share/man
            setpath MANPATH MANPATH:$OLDMANPATH

            OLDLD_LIBRARY_PATH=$LD_LIBRARY_PATH
            setpath LD_LIBRARY_PATH LD_LIBRARY_PATH:/opt/sfw/lib
            setpath LD_LIBRARY_PATH LD_LIBRARY_PATH:/opt/SUNWspci/lib:/opt/SUNWspro/lib
            setpath LD_LIBRARY_PATH LD_LIBRARY_PATH:/usr/sfw/lib:/usr/local/lib
            setpath LD_LIBRARY_PATH LD_LIBRARY_PATH:/usr/openwin/lib:/usr/dt/lib
            setpath LD_LIBRARY_PATH LD_LIBRARY_PATH:/opt/sfw/gcc-3/lib:/usr/ccs/lib
            setpath LD_LIBRARY_PATH LD_LIBRARY_PATH:/usr/lib
            setpath LD_LIBRARY_PATH LD_LIBRARY_PATH:/usr/xpg4/lib
            setpath LD_LIBRARY_PATH LD_LIBRARY_PATH:$OLDLD_LIBRARY_PATH

            TOP="-S 100" ; export TOP
            LD_OWN_ARCH=_32 ; export LD_OWN_ARCH
            ;;
    esac
    export PATH
    export LD_LIBRARY_PATH

    setup_display

    MANPATH=$HOME/lib/usr/man:$HOME/lib/usr/share/man:$HOME/software/$SYSTEM/man:$HOME/software/$SYSTEM/share/man:$HOME/perllib/share/man:$MANPATH:
    #       export MANPATH=$(manpath -q):$HOME/lib/usr/man:$HOME/lib/usr/share/man
    export MANPATH

    #man dpkg-buildpackage
    #DEB_BUILD_OPTIONS='parallel=3'
    #export DEB_BUILD_OPTIONS parallel=3
    export CCACHE_COMPRESS=yes

    eval "`dircolors 2>/dev/null`"

    export LS_COLORS="$LS_COLORS:no=00:fi=00:di=01;34:ln=01;36:pi=40;33:so=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.bz2=01;31"
    export LS_COLORS="$LS_COLORS:*.gz=01;31:*.lzma=01;31:*.xz=01;31:*.deb=01;31:*.jpg=01;35:*.png=01;35:*.gif=01;35:*.bmp=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.mpg=01;37:*.mp3=01;37:*.avi=01;37:*.gl=01;37:*.dl=01;37"
    export LS_COLORS="$LS_COLORS:*.c=01;36:*.f=01;36:*.f90=01;36:*.o=00;32:*.html=00;35:*.cgi=00;35:*.txt=00;36:*.tex=00;34:*.dvi=00;35:*.ps=00;35:*.eps=00;35:*.doc=00;35"
    export CLICOLOR=1
    #export LSCOLORS="3x5x2x3x1x464301060203" # export LSCOLORS="dxfxBxcxbxegedabagacad"
    export LSCOLORS="dxfxBxcxbxegedabagacad"
    # #877582 -  https://unix.stackexchange.com/questions/258679/why-is-ls-suddenly-wrapping-items-with-spaces-in-single-quotes
    export QUOTING_STYLE=literal

    export PROMPT_USER_ANSI=$'\[\e[1;36m\]'
    export ANSI_RESET=$'\e[0m'

    #export GREP_OPTIONS='--color=auto'
    #export LS_OPTIONS #no such thing, cant remove the aliases

    # ZFS_COLOR doesn't have an =auto or =yes option, but fortunately =1 seems to imply auto
    export ZFS_COLOR=1 # although `zpool iostat -v 1 | zpool-iostat--color` is nicer than `ZFS_COLOR=1 zpool iostat -v 1`

    # is the world composed entirely of idiots? https://ubuntuhandbook.org/index.php/2019/09/make-scrollbar-always-visible-ubuntu-18-04/
    # gtk4 will further fuck it up: https://wiki.archlinux.org/index.php/GTK%2B#Disable_overlay_scrollbars
    export GTK_OVERLAY_SCROLLING=0
    # this doesn't appear to yet work on debian stable 10.9, 2021/04/21,
    # where a fresh gedit installation still has no window decorations.
    # http://www.webupd8.org/2014/08/how-to-disable-gtk3-client-side.html
    # https://askubuntu.com/questions/961161/how-do-i-disable-client-side-decoration-globally-in-gnome
    # instead, run gedit under gtk3-nocsd
    # export GTK_CSD=0

    # shitty file dialogs in eg firefox
    # https://www.reddit.com/r/linux/comments/54ocjq/why_cant_applications_call_on_the_desktop_ui_to/d83tsax/
    # https://wiki.archlinux.org/title/Environment_variables#Examples
    # XDG_SESSION_DESKTOP=twm (maybe fvwm?) will also help menus
    # disappearing when you mouse-off them in focus-follows-mouse WMs:
    # https://forums.freebsd.org/threads/firefox-strange-behaviour-after-upgrade.88730
    # export XDG_CURRENT_DESKTOP=XFCE:LXDE:LXQt:MATE:KDE:GNOME
    unset XDG_CURRENT_DESKTOP # seems any setting at all breaks some drop-down menus in firefox, such as LuCI in OpenWRT
    export XDG_SESSION_DESKTOP=fvwm # XFCE, twm (fvwm might stop bookmarks submenu's invoking: https://forums.freebsd.org/threads/firefox-strange-behaviour-after-upgrade.88730/ but on the other hand, we've been using it for a few weeks already via direct setting in ~/bin/firefox ; consider also widget.gtk.grab-pointer = 1 (default 2))
    export DE=generic
    export DESKTOP_SESSION=fvwm
    export WINDOW_MANAGER=fvwm
    export QT_QPA_PLATFORMTHEME=kde/gtk3

    export PAGER=less

    BROWSER=choosebrowser

    #export LC_MESSAGES=en_AU
    #export EMACSPACKAGEPATH=$HOME/.xemacs/packages
    if [ -z "$EMAIL" ] ; then
        if [[ $LONGHOST == *ltu.edu.au ]] ||
           [[ $LONGHOST == *latrobe.edu.au ]] ; then
            export EMAIL=t.connors@latrobe.edu.au
            setpath PATH PATH:$HOME/bin/ltu
        else
            export EMAIL=tconnors@rather.puzzling.org
        fi
    fi
    # for reportbug so we can keep an otherwise vanilla file
    export REPORTBUGEMAIL=reportbug@rather.puzzling.org
    export DEBFULLNAME="Tim Connors"
    #export WEBPAGE=http://www.astronomy.swin.edu.au/staff/tconnors
    #export WEBPAGE=http://tau-iota-mu-c.livejournal.com/

    #export MAILCHECK=no
    unset MAILCHECK
    #unset MAIL

    #only start the server if on a *local* display.
    #only use graphics also if local - don't want to start xemacs over the modem!
    EDITORS="emacs xemacs21 nano pico vi"
    for EDITOR in $EDITORS ; do
        if programexists $EDITOR ; then
            # for root, limit emacs to normal emacs only, not xemacs
            if [ "$UID" = 0 ] ; then
                if [[ $EDITOR == xemacs21 ]]; then
                    EDITOR=emacs
                fi
            fi
            # when not on local display, use emacs-term in preference to vanilla emacs
            if [[ "$DISPLAY" != :* ]] ; then
                if [[ $EDITOR == *emacs* ]]; then
		    if which emacs-term >& /dev/null ; then
			EDITOR=emacs-term
		    fi
                fi
            fi
            # use emacsclienserver in preference to vanilla emacs
            if [ "$EDITOR" = emacs -a "$UID" != 0 ] ; then
		if which emacsclientserver >& /dev/null ; then
                    EDITOR=emacsclientserver
		fi
            fi
            # for root, limit emacs to terminal only, not X11
            if [ "$UID" = 0 ] && [[ "$EDITOR" ==  *emacs* ]] && ! [ $EDITOR = emacs-term ] ; then
                EDITOR="$EDITOR -nw"
            fi
            break
        fi
    done
    export EDITOR
    export CVSEDITOR=cvse

    CVSROOT=tconnors@dirac:/home/tconnors/cvsroot
    export CVSROOT

    #CVSROOT=$HOME/cvs
    #CVSROOT=/var/cvs
    #CVSROOT=":ext:developer@cvs_server_box.domain.com:/home/cvsroot"
    if programexists fsh ; then
        export CVS_RSH=fsh
    else
        export CVS_RSH=ssh
    fi

    LESSHISTSIZE=20480
    export LESSHISTSIZE
    LESS="-i -M -R -P%t?f%f:stdin .?pb%pb\%:?lbLine %lb:?bbByte %bb:-..."  #no -X - because this disables cursor movement (when and where was this? Works on debian unstable 20070130, mind you, it seems to be reasonably annoying when viewing multiple files in querybts and the previous file isnt cleared off the screen until you scroll, so it becomes hard to tell where the old file ends and the new file starts)
    #FIXME: -g has been disabled temporary because of bug #459335, 460171
    case $OS in
        Darwin)
            LESS="-X $LESS"
            ;;
        SunOS)
            #         LESS="-g -i -P%t?f%f:stdin .?pb%pb\%:?lbLine %lb:?bbByte %bb:-..."
            export XFILESEARCHPATH=/opt/csw/lib/X11/%T/%N%C:/usr/openwin/lib/X11/%T/%N%C
            ;;
    esac
    export LESS
    if [ -n "$WSL_DISTRO_NAME" ] ; then
        setpath PATH PATH:/mnt/c/WINDOWS:/mnt/c/WINDOWS/System32:/mnt/c/WINDOWS/System32/WindowsPowerShell/v1.0/
    fi

    #debian bug 505963 (may really be associated with UTF8 problem that show_types highlights:
    #export LIBXCB_ALLOW_SLOPPY_LOCK=yes

    TEXEDIT="$EDITOR +%d %s"
    #put a spare : somewhere - this is the default path
    BSTINPUTS=.:$HOME/latex/bstinputs/:
    BIBINPUTS=.:$HOME/latex/bib/:
    TEXPSHEADERS=.:$HOME/latex/texinputs:
    setpath TEXINPUTS .:$HOME/latex/texinputs:$HOME/latex/texmf/tex/latex/preview:$HOME/.TeX:/usr/share/doc/.TeX
    setpath TEXINPUTS /usr/doc/.TeX:/usr/share/texmf/tex/latex/:TEXINPUTS:
    export TEXINPUTS BIBINPUTS BSTINPUTS TEXPSHEADERS TEXEDIT

    #export PERLLIB=$HOME/perllib:$PERLLIB
    #export UNITSFILE=$HOME/.units

    #eval `echo -n perl ; perl -V:version`  #gives $perlversion
    #perlmajorversion=`echo "$perlversion" | sed 's/\([^.]\)*\.\([^.]\).*/\1.\2/'`
    #perlmajormajorversion=`echo "$perlmajorversion" | sed 's/\([^.]\)*\.\([^.]\).*/\1/'`

    #export PERLLIB=$HOME/perllib/$perlversion:$HOME/perllib/$perlmajorversion:$HOME/perllib/lib/perl$perlmajormajorversion/site_perl/$perlversion:$HOME/perllib/lib/perl$perlmajormajorversion/site_perl/$perlversion/i386-linux-thread-multi:$PERLLIB
    export PERL5LIB=$HOME/perllib:$HOME/perllib/lib
    export PYTHONPATH=$HOME/lib/python
    export GOPATH=$HOME/lib/go

    setpath PATH PATH:$GOPATH/bin

    setpath PATH $HOME/bin:$HOME/bin/$SYSTEM:$HOME/software/$SYSTEM/bin:$HOME/perllib/bin:PATH
    export PATH

    if programexists ssh-askpass && [[ "$DISPLAY" == :* ]] ; then
        export SSH_ASKPASS=ssh-askpass
    else
        unset SSH_ASKPASS
    fi

    #from: cpan> o conf init
    PATH="$HOME/perl5/bin${PATH:+:${PATH}}"; export PATH;
    PERL5LIB="$HOME/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
    PERL_LOCAL_LIB_ROOT="$HOME/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
    PERL_MB_OPT="--install_base \"$HOME/perl5\""; export PERL_MB_OPT;
    PERL_MM_OPT="INSTALL_BASE=$HOME/perl5"; export PERL_MM_OPT;

    for i in /usr/share/X11/rgb.txt ; do
        if [ -e $i ] ; then
            PGPLOT_RGB=$i
            export PGPLOT_RGB
            break
        fi
    done

    export PDSH_RCMD_TYPE=ssh
    export RSHCOMMAND=ssh

    case "$TERM-$SHORTHOST" in
        emacs-*)
            export EMACS=running
            export PAGER=more
            #       export TERM=dumb
            ;;
        #    xterm-indium*)
        #       export TERM=xterm-color
        #    ;;
    esac

    #force core dump for severe errors: fortran:
    # export decfort_dump_flag=y
    #export CFLAGS="-O3 -g -tpp7 -axW"
    #export CXXFLAGS="$CFLAGS"
    #export CC=icc
    #export CXX="$CC"

    export VERSION_CONTROL=numbered

    export INFO_PRINT_COMMAND=a2ps

    #export TZ=Australia/Melbourne   # rely on /etc/timezone, since perl Date::Manip can't handle arbitrary TZ

    # We do want to addkeychain on dirac, not on any ssh sessions from
    # dirac, on ltu, and not on any ssh sessions from ltu (ie,
    # coyote8), except maybe dirac if it's the first time logging into
    # dirac.  So yes everywhere except ssh session, except when agent
    # is running but unpopulated.  Previously solved this temporarily
    # in EDC just by checking `ssh-add -l`'s exit code for $? = 2

    # Maybe it suffices just to run it when the system we're logging
    # onto actually has keys of its own?
    if [ -n "$( find ~/.ssh/ -maxdepth 1 -name 'id*pub' -quit )" ] ; then
        # quickest check for a wildcard is with `find ... -quit`: https://stackoverflow.com/questions/6363441/check-if-a-file-exists-with-a-wildcard-in-a-shell-script
        if [ -e $HOME/.keychain/$HOSTNAME-sh ] ; then
            . $HOME/.keychain/$HOSTNAME-sh
        fi
        if programexists keychain ; then
            if ! [ -n "$SSH_AUTH_SOCK" ] ; then
                if [ -z "$NONINTERACT" -a -z "$PBS_ENVIRONMENT" -a -n "$DISPLAY" -a -t 0 ] ; then # not some sort of automated process
                    if ! programexists nodeattr ; then  # a node we don't control ourselves
                        addkeychain
                    elif nodeattr -l $SHORTHOST 2>/dev/null | grep -q desktop ; then   # not a remote machine where we don't have keys installed
                        addkeychain
                    fi
                fi
            else
                addkeychain
            fi
        fi
    fi

    #export HOSTALIASES=$HOME/.scuzzieaddress
    #export HOSTALIASES=$HOME/.hostaliases

    if programexists pathstat ; then
        for i in LD_LIBRARY_PATH PATH PERL5LIB ; do # PERLLIB
            if [ -n "`eval echo '$'$i`" ] ; then
	        p=`pathstat "$i"`
	        if [ -n "$p" ] ; then
                    export $p
                else
                    unset $i
	        fi
            fi
        done
        if [[ "$SYSTEM" == *SunOS* ]] ; then
            estpath LD_LIBRARY_PATH /opt/csw/lib/\$ISALIST:$HOME/software/$SYSTEM/lib/\$ISALIST:LD_LIBRARY_PATH
        fi
        for i in MANPATH TEXINPUTS BIBINPUTS BSTINPUTS ; do
            if [ -n "`eval echo '$'$i`" ] ; then
	        p=`pathstat "$i"`
	        if [ -n "$p" ] ; then
                    export $p:
                else
                    unset $i
	        fi
            fi
        done
    fi

    if [ -n "$LD_LIBRARY_PATH" ] ; then
        export LD_LIBRARY_PATH_SAVE=$LD_LIBRARY_PATH   #save for future, because xterm always removes LD_LIBRARY_PATH
    fi
    if [ -n "$LD_PRELOAD" ] ; then
        export LD_PRELOAD_SAVE=$LD_PRELOAD   #save for future, because xterm always removes LD_LIBRARY_PATH
    fi

    #    echo 3 1>&2

    #  cd `pwd`    #when /home/tconnors points to /home/tconnors-scuzzie, then we want to cd to set the path
    RPWD=$(realpath "$PWD")
    if [ "$RPWD" = $(realpath $HOME) ] ; then
        cd
    fi
}
