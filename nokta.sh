#!/bin/bash

###########################################
#              _    _              _      #
#  _ __   ___ | | _| |_ __ _   ___| |__   #
# | '_ \ / _ \| |/ / __/ _` | / __| '_ \  #
# | | | | (_) |   <| || (_| |_\__ \ | | | #
# |_| |_|\___/|_|\_\\__\__,_(_)___/_| |_| #
#                                         #
###########################################

stow -t ~ \
     bin \
     compton \
     dunst \
     emacs \
     i3 \
     fish \
     kitty \
     polybar \
     rofi \
     vim \
     x

# Export oomox configuration as gtk theme
oomox-cli $HOME/.config/oomox/colors/Custom/gruvbox --output oomox-gruvbox
oomoxify-cli -s /opt/spotify/Apps/ $HOME/.config/oomox/colors/Custom/gruvbox
