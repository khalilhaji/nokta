######################################################
#              _    _                                #
#  _ __   ___ | | _| |_ __ _                         #
# | '_ \ / _ \| |/ / __/ _` |  _____                 #
# | | | | (_) |   <| || (_| | |_____|                #
# |_| |_|\___/|_|\_\\__\__,_|                        #
#                                                    #
#                    __ _          ____ _     _      #
#    ___ ___  _ __  / _(_) __ _   / / _(_)___| |__   #
#   / __/ _ \| '_ \| |_| |/ _` | / / |_| / __| '_ \  #
#  | (_| (_) | | | |  _| | (_| |/ /|  _| \__ \ | | | #
# (_)___\___/|_| |_|_| |_|\__, /_/ |_| |_|___/_| |_| #
#                         |___/                      #
######################################################

set -gx PATH $HOME/.bin $HOME/.local/bin $HOME/.gem/ruby/2.6.0/bin $PATH
set -gx EDITOR vim
set -gx WEB_BROWSER google-chrome-stable
set -gx GTK_THEME oomox-gruvbox
set -gx GTK2_RC_FILES $HOME/.themes/oomox-gruvbox/gtk-2.0/gtkrc
set -gx ESHELL /usr/bin/fish
set -gx SHELL /usr/bin/fish


omf theme bobthefish

alias sl ls
alias emacs "emacs -nw"
