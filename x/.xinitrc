#!/bin/sh

#######################################################################
#              _    _                         _       _ _             #
#  _ __   ___ | | _| |_ __ _            __  _(_)_ __ (_) |_ _ __ ___  #
# | '_ \ / _ \| |/ / __/ _` |  _____    \ \/ / | '_ \| | __| '__/ __| #
# | | | | (_) |   <| || (_| | |_____|  _ >  <| | | | | | |_| | | (__  #
# |_| |_|\___/|_|\_\\__\__,_|         (_)_/\_\_|_| |_|_|\__|_|  \___| #
#                                                                     #
#######################################################################

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

DEFAULT_SESSION=gnome-session

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi



# exec $(get_session)
xinput --set-prop DLL07BE:01\ 06CB:7A13\ Touchpad "libinput Tapping Enabled" 1 &
xbindkeys & 
feh --bg-scale ~/.bg.png &
nm-applet & 
picom --config $HOME/.config/picom/picom.conf &
dunst &
xbacklight -set 40 &
amixer set Master 0% &
amixer set Master mute &

exec i3
