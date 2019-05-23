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

sudo pacman -Syyu --needed --noconfirm base base-devel git stow openssh


function install {
    PACKAGES="$PACKAGES ${@:2}"
}


install system os-prober ntp
install shell kitty fish bash-completion neofetch htop
install networking networkmanager network-manager-applet curl wget
install vpn nordvpn-bin
install gui xorg xbindkeys xorg-xinit xdotool xdg-utils
install wm i3-gaps polybar dunst compton betterlockscreen rofi feh maim

curl https://sh.rustup.rs -sSf | sh -s -- -y
install java openjdk8-doc openjdk8-src jdk8-openjdk
install node npm
install racket racket
install python python python2 python-virtualenv python-pip python2-pip

install themes oomox-git
install fonts ttf-fira-code ttf-fira-mono ttf-fira-sans ttf-font-awesome ttf-dejavu noto-fonts-emoji powerline-fonts-git
install drivers xf86-video-intel bumblebee
install audio alsa-utils pulseaudio pavucontrol pulseaudio-alsa
install zip zip unzip
install music spotify playerctl cava
install video mpv youtube-dl
install torrent transmission-cli transmission-gtk
install web google-chrome firefox
install files thunar gvfs gvfs-afc gvfs-goa gvfs-google gvfs-mtp gvfs-smb ntfs-3g thunar-volman
install communication slack-desktop discord riot-desktop
install editor emacs vim
install power tlp powertop
install security libu2f-host ufw


if [ ! -f /usr/bin/trizen ]; then
  mkdir -pv "$HOME/tmp"
  pushd "$HOME/tmp"

  git clone https://aur.archlinux.org/trizen.git
  pushd trizen
  yes | makepkg -sri
  popd

  popd
  rm -vrf "$HOME/tmp"
fi

gpg --recv-keys A87FF9DF48BF1C90

trizen -Sq --needed --noconfirm $PACKAGES

curl -L https://get.oh-my.fish > "$HOME/install"
chmod +x "$HOME/install"
fish -c "eval $HOME/install --path=$HOME/.local/share/omf --config=$HOME/.config/omf --noninteractive --yes"
rm -vf "$HOME/install"
fish -c "omf install bobthefish"
fish -c "omf theme bobthefish"

git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim

echo 'if [ -z "$BASH_EXECUTION_STRING" ]; then exec fish; fi' > ~/.bashrc

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


sudo systemctl enable --now NetworkManager
sudo systemctl enable --now ntpd
sudo systemctl enable --now tlp
sudo systemctl enable --now tlp-sleep
sudo systemctl enable --now nordvpnsd.service
systemctl --user enable --now nordvpnud

# Export oomox configuration as gtk theme
oomox-cli $HOME/.config/oomox/colors/Custom/gruvbox --output oomox-gruvbox
oomoxify-cli -s /opt/spotify/Apps/ $HOME/.config/oomox/colors/Custom/gruvbox

betterlockscreen -u .bg.png
