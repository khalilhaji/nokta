#!/bin/bash

###########################################
#              _    _              _      #
#  _ __   ___ | | _| |_ __ _   ___| |__   #
# | '_ \ / _ \| |/ / __/ _` | / __| '_ \  #
# | | | | (_) |   <| || (_| |_\__ \ | | | #
# |_| |_|\___/|_|\_\\__\__,_(_)___/_| |_| #
#                                         #
###########################################


echo "foweijf"


mkdir -pv "$HOME/.config"

sudo pacman -S --needed --noconfirm reflector

sudo reflector --latest 15 --protocol http --protocol https --sort rate --save /etc/pacman.d/mirrorlist

sudo pacman -Syyu --needed --noconfirm base base-devel git stow openssh


function install {
    PACKAGES="$PACKAGES ${@:2}"
}


install system os-prober ntp
install shell kitty zsh bash-completion neofetch htop figlet
install networking networkmanager network-manager-applet curl wget
install vpn nordvpn-bin
install gui xorg xbindkeys xorg-xinit xdotool xdg-utils
install wm i3-gaps polybar dunst picom betterlockscreen rofi feh maim
install screen xrandr arandr autorandr

curl https://sh.rustup.rs -sSf | sh -s -- -y
install java openjdk8-doc openjdk8-src jdk8-openjdk
install node npm
install python python python2 python-virtualenv python-pip python2-pip

install themes arc-gtk-theme arc-icon-theme-git
install fonts ttf-fira-code ttf-fira-mono ttf-fira-sans ttf-font-awesome-4 ttf-dejavu noto-fonts-emoji powerline-fonts-git
install drivers xf86-video-intel bumblebee bbswitch
install audio alsa-utils pulseaudio pavucontrol pulseaudio-alsa
install zip zip unzip
install music spotify playerctl cava
install video mpv youtube-dl
install torrent transmission-cli transmission-gtk
install web google-chrome firefox
install files thunar gvfs gvfs-afc gvfs-goa gvfs-google gvfs-mtp gvfs-smb ntfs-3g thunar-volman xdg-user-dirs
install communication slack-desktop discord 
install editor emacs vim 
install power tlp powertop
install security libu2f-host ufw


if [ ! -f /usr/bin/yay ]; then
  mkdir -pv "$HOME/tmp"
  pushd "$HOME/tmp"

  git clone https://aur.archlinux.org/yay.git
  pushd yay
  yes | makepkg -sri
  popd

  popd
  rm -vrf "$HOME/tmp"
fi

gpg --recv-keys A87FF9DF48BF1C90
gpg --recv-keys 4773BD5E130D1D45

echo $PACKAGES
yay -Sq --needed --noconfirm $PACKAGES

sudo chsh /usr/bin/zsh

git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim


stow -t ~ \
     bin \
     picom \
     dunst \
     emacs \
     zsh \
     gtk \
     i3 \
     kitty \
     polybar \
     rofi \
     vim \
     x


sudo systemctl enable --now NetworkManager
sudo systemctl enable --now bumblebeed
sudo systemctl enable --now ntpd
sudo systemctl enable --now tlp
sudo systemctl enable --now tlp-sleep
sudo systemctl enable --now nordvpnsd.service
systemctl --user enable --now nordvpnud
xdg-user-dirs-update

betterlockscreen -u $HOME/.bg.png
