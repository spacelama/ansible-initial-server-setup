The aim here is to be able to install this directory into
/etc/bash.bashrc.d/ for any machine we control, and have it set up the
shell the way we want it as efficiently as possible (given opening an
xterm results in everything being set up from scratch every time).
Our /etc/bash.bashrc sources every *.sh in /etc/bash.bashrc.d/*.sh
followed by ~/.bashrc.d/*.sh

But on machines we don't control, we want to be able to also copy this
directory into ~/.bashrc.d/ and have the shell behave as close as we
can possibly do so without touching anything in /etc, even on stupid
systems like SuSE that unconditionally set a bunch of insane and
dangerous stuff in /etc/profile.d/* etc, while trying its hardest to
make sure it is the last thing run to it tries to clobber all your
settings.

We achieve this mostly through setting up helper functions that
re-initialise our environment only when the prompt is about to be
displayed via PROMPT_COMMAND.


The things we do have to touch in /etc (bash.bashrc optimisations to
stop everything being run twice, etc), we try to do in as minimal way
as possible to minimise diffs at installation/onboarding and every
`apt dist-upgrade` time (hence why we have our own ~/.bashrc.d/.

# NOPE: only *.sh is copied into place by ansible: You can put helper
# script snippets in this directory too, and so long as the filenames
# don't end in '.sh', they won't be sourced automatically, so you can
# put more expensive calls in there such as bash_completion setup, and
# control when they're called, yourself.
