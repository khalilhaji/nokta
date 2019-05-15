#!/bin/bash

###########################################
#              _    _              _      #
#  _ __   ___ | | _| |_ __ _   ___| |__   #
# | '_ \ / _ \| |/ / __/ _` | / __| '_ \  #
# | | | | (_) |   <| || (_| |_\__ \ | | | #
# |_| |_|\___/|_|\_\\__\__,_(_)___/_| |_| #
#                                         #
###########################################

mkdir -pv "$HOME/Downloads"
mkdir -pv "$HOME/Documents"
mkdir -pv "$HOME/.config"

sudo pacman -S --needed --noconfirm reflector

sudo reflector --latest 15 --protocol http --protocol https --sort rate --save /etc/pacman.d/mirrorlist

sudo pacman -Syyu --needed --noconfirm base base-devel git stow


function install {
    PACKAGES="$PACKAGES ${@:2}"
}


install system os-prober
install shell kitty fish bash-completion neofetch htop
install networking networkmanager network-manager-applet curl wget
install vpn nordvpn-bin
install gui xorg xbindkeys xorg-xinit xdotool xdg-utils
install wm i3-gaps polybar dunst compton betterlockscreen rofi feh maim
install rust rustup
# Necessary for oomox
rustup install stable
rustup default stable


install java openjdk8-doc openjdk8-src jdk8-openjdk
install node npm
install racket racket
install python python python2 python-virtualenv python-pip python2-pip
install themes oomox-git
install fonts ttf-fira-code ttf-fira-mono ttf-fira-sans ttf-font-awesome ttf-dejavu noto-fonts-emoji powerline-fonts-git
install drivers xf86-video-intel bumblebee
install audio alsa-utils pulseaudio pavucontrol
install zip zip unzip
install music spotify playerctl cava
install video mpv youtube-dl
install torrent transmission-cli transmission-gtk
install web google-chrome firefox
install files thunar
install communication slack-desktop discord riot-desktop
install editor emacs vim

git clone https://aur.archlinux.org/yay.git
cd yay
makepkg -si --noconfirm
cd ..
rm -rf yay

yay -Sq --needed --noconfirm $PACKAGES

echo 'if [-z \"$BASH_EXECUTION_STRING\" ]; then exec fish; fi' > ~/.bashrc

stow -t ~ \
     bin \
     compton \
     dunst \
     emacs \
     fish \
     i3 \
     kitty \
     oomox \
     polybar \
     rofi \
     vim \
     x


# Export oomox configuration as gtk theme
oomox-cli $HOME/.config/oomox/colors/Custom/gruvbox --output oomox-gruvbox
oomoxify-cli -s /opt/spotify/Apps/ $HOME/.config/oomox/colors/Custom/gruvbox
