[[ -f /etc/zsh/zshrc ]] && source /etc/zsh/zshrc
export PATH=$HOME/bin:$HOME/.local/bin:$HOME/go/bin:$PATH
export WEB_BROWSER=firefox
ANTIGEN=$HOME/.antigen.zsh
test -e $ANTIGEN || (echo "Installing Antigen..."; curl -L git.io/antigen > $ANTIGEN 2>/dev/null)
source $ANTIGEN
antigen use oh-my-zsh
antigen theme robbyrussell
antigen bundle git
antigen bundle sudo
antigen apply
alias sl=ls
eval "$(pyenv init -)"
