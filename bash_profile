export EDITOR="vim"

export NOTES_DIR="$HOME/notes"
SUBLIME_HOME="/Applications/Sublime\ Text.app"
TOOLS_HOME="$HOME/developer/tools"
GCLOUD_HOME="/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk"
IMAGEMAGICK_HOME="/opt/ImageMagick-7.0.5"
export PATH="$IMAGEMAGICK_HOME/bin:$PATH"
export PATH="$HOME/developer/notes-cli/bin:$PATH"
export SUMO_HOME="$HOME/developer/sumo/sumo/sumo"
export MATSIM_BINARY_PATH="$HOME/developer/matsim-0.8.1/matsim-0.8.1.jar"
export PATH="$NOTES_HOME/bin:$PATH"
export PATH="$SUBLIME_HOME/Contents/SharedSupport/bin:$PATH"
export PATH="$TOOLS_HOME:$PATH"
export PATH="$GCLOUD_HOME/bin:$PATH"
# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
eval "$(pyenv init -)"

## git branch and bash prompt
parse_git_branch() {
	 git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}
export PS1="\u@ \[\033[32m\]\w\[\033[33m\]\$(parse_git_branch)\[\033[00m\] "

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion


# MacPorts Installer addition on 2017-08-15_at_13:22:19: adding an appropriate PATH variable for use with MacPorts.
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
# Finished adapting your PATH environment variable for use with MacPorts.


[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

source "$HOME/.bash_aliases"


# http://www.unixwiz.net/techtips/ssh-agent-forwarding.html
# http://www.funtoo.org/OpenSSH_Key_Management,_Part_2
#
# setup ssh-agent: http://mah.everybody.org/docs/ssh#run-ssh-agent
#

# set environment variables if user's agent already exists
[ -z "$SSH_AUTH_SOCK" ] && SSH_AUTH_SOCK=$(ls -l /tmp/ssh-*/agent.* 2> /dev/null | grep $(whoami) | awk '{print $9}')
[ -z "$SSH_AGENT_PID" -a -z `echo $SSH_AUTH_SOCK | cut -d. -f2` ] && SSH_AGENT_PID=$((`echo $SSH_AUTH_SOCK | cut -d. -f2` + 1))
[ -n "$SSH_AUTH_SOCK" ] && export SSH_AUTH_SOCK
[ -n "$SSH_AGENT_PID" ] && export SSH_AGENT_PID

# start agent if necessary
if [ -z $SSH_AGENT_PID ] && [ -z $SSH_TTY ]; then  # if no agent & not in ssh
  eval `ssh-agent -s` > /dev/null
fi

# setup addition of keys when needed
if [ -z "$SSH_TTY" ] ; then                     # if not using ssh
  ssh-add -l > /dev/null                        # check for keys
  if [ $? -ne 0 ] ; then
    alias ssh='ssh-add -l > /dev/null || ssh-add && unalias ssh ; ssh'
    if [ -f "/usr/lib/ssh/x11-ssh-askpass" ] ; then
      SSH_ASKPASS="/usr/lib/ssh/x11-ssh-askpass" ; export SSH_ASKPASS
    fi
  fi
fi

