[[ -f /etc/zsh/zshrc ]] && source /etc/zsh/zshrc
export PATH=$HOME/bin:$HOME/.local/bin:$HOME/go/bin:$PATH
export WEB_BROWSER=firefox
export GOPATH=~/go
ANTIGEN=$HOME/.antigen.zsh
test -e $ANTIGEN || (echo "Installing Antigen..."; curl -L git.io/antigen > $ANTIGEN 2>/dev/null)
source $ANTIGEN
antigen use oh-my-zsh
antigen theme robbyrussell
antigen bundle git
antigen bundle sudo
antigen apply
alias v=vim
alias sl=ls
## Emacs in term
alias e='emacsclient -nw'
alias play='cd $HOME/playground'
