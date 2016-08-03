#!/bin/bash
#
command_exists () {
    type "$1" &> /dev/null ;
}

if command_exists xterm; then
  failsafe="xterm -ls -T Failsafe -geometry 80x24-0-0"
  trap "exec $failsafe" EXIT SIGHUP SIGINT SIGPIPE SIGTERM SIGIO
fi

#
# Some bash (1 and 2) settings to avoid trouble on a
# failed program call.
#
set +e > /dev/null 2>&1
set +u > /dev/null 2>&1
set +o posix  > /dev/null 2>&1
if type shopt > /dev/null 2>&1 ; then
    shopt -s execfail
else
    no_exit_on_failed_exec=1
fi

#
# Source common code shared between the
# X session and X init scripts
#
XINITRC_COMMON="/etc/X11/xinit/xinitrc.common"
if [[ -r "${XINITRC_COMMON}" ]]; then
  source "${XINITRC_COMMON}"
fi

#
# Uncomment next lines to activate asking for ssh passphrase
#
# if test -S "$SSH_AUTH_SOCK" -a -x "$SSH_ASKPASS"; then
#       ssh-add < /dev/null
# fi



if command_exists /usr/bin/xset ; then
  # disable the annoying bell/beep
  /usr/bin/xset b off
fi

if command_exists vmware-user-autostart-wrapper ; then
  set +e
  vmware-user-autostart-wrapper &
  set -e
fi

if [[ -r $HOME/.Xresources ]]; then
  xrdb -merge $HOME/.Xresources
fi

if command_exists setxkbmap; then
  setxkbmap dk
fi

if command_exists ssh-agent ; then
  eval $(ssh-agent)
fi

# 
# finally start the window manager
#
unset WINDOW_MANAGER STARTUP
exec $WINDOWMANAGER ${1+"$@"}

# call failsafe
exit 0
