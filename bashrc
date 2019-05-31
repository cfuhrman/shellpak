#!/bin/bash
# ====================================================================
#
# bashrc
#
# Copyright (c) 2000 Christopher M. Fuhrman
# All rights reserved.
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the Simplified BSD License (also
# known as the "2-Clause License" or "FreeBSD License".)
#
# ====================================================================

#
# This file gets read in when this is an interactive (non-login) shell,
# such as xterm windows and such
#

# We need to set SHELLDIR up here lest breakage occur
SHELLDIR=$HOME/SHELL
export SHELLDIR

# Includes
source $SHELLDIR/functions
source $SHELLDIR/prompts
source $SHELLDIR/hostaliases
source $SHELLDIR/thirdparty/git-prompt.sh

# Set SHELLPAK_VERSION
if [ -f ${SHELLDIR}/VERSION ]; then
        SHELLPAK_VERSION=$( cat ${SHELLDIR}/VERSION )
        export SHELLPAK_VERSION
fi

# Set some variables useful in determining our environment
export HOSTNAME=$(hostname)
export OSTYPE=$(uname)
export OSVERSION=$(uname -r)

# I live on the American West Coast
export TZ='America/Los_Angeles'

# Make sure gpg-agent(1) knows what tty it's on
export GPG_TTY=$(tty)

# Setting for Pine/Alpine remote config
PINE_REMOTE_CONFIG="{mail.example.com/ssl/novalidate-cert/user=cfuhrman@example.com}INBOX.in-my-alpine-config"

# ----------------------------------------------------------------------

# Function: __sp_bashrc_set_browser
#
# Sets BROWSER environment variable
#
# Order of preference:
#
#  * firefox
#  * galeon
#  * konqueror
#  * mozilla
#  * w3m
#  * elinks
#  * lynx

__sp_bashrc_set_browser ()
{

        # Set preferred browsers in order of evaluation
        browsers=('firefox'                     \
                  'galeon'                      \
                  'epiphany'                    \
                  'konqueror'                   \
                  'mozilla'                     \
                  'w3m'                         \
                  'elinks'                      \
                  'lynx'
                 )

        for browser in ${browsers[@]}; do

                if type -p ${browser} >/dev/null; then
                        BROWSER=$browser
                        break;
                fi

        done

        export BROWSER

} # __sp_bashrc_set_browser()

# Function: __sp_bashrc_set_editor
#
# Sets EDITOR environment variable
#
# Order of preference:
#
#  * emacs
#  * mg
#  * nano
#  * vim
#  * vi (if all else fails)

__sp_bashrc_set_editor ()
{

        # Set preferred editors in order of evaluation
        editors=('emacs'                        \
                 'mg'                           \
                 'nano'                         \
                 'vim'                          \
                 'vi'
                )

        for edit in ${editors[@]}; do

                if type -p ${edit} >/dev/null; then
                        EDITOR=$edit
                        export EDITOR;

                        break;
                fi

        done

        # Should the editor be emacs(1), make sure we use console
        # mode, but only if this isn't SunOS (which has issues with
        # the '-nw' flag)
        if [[ ${EDITOR} == 'emacs' && ${OSTYPE} != 'SunOS' ]]; then
                EDITOR='emacs -nw'
        fi

} # __sp_bashrc_set_editor()

# Function: __sp_bashrc_set_pager
#
# Sets PAGER environment variable
#
# Order of preference:
#
#  * less
#  * view
#  * more

__sp_bashrc_set_pager ()
{

        # Should this be an emacs terminal, then set $PAGER to 'cat' since
        # emacs will do our paging for us
        if [ $TERM == 'dumb' ] || [ $TERM == 'emacs' ]; then
                PAGER='cat'
        else

                # Set preferred pagers in order of evaluation
                pagers=('less'                  \
                        'more'                  \
                        'view'
                       )

                for pagr in ${pagers[@]}; do

                        if type -p ${pagr} >/dev/null; then
                                PAGER=$pagr
                                break;
                        fi

                done

       fi

        export PAGER

} # __sp_bashrc_set_pager()

# ----------------------------------------------------------------------

# Source global definitions file
if [ -f /etc/bashrc ]; then
        . /etc/bashrc
fi

# First check and see if we're an interactive shell
if [ "$PS1" ]; then

        if [ "x`tput kbs`" != 'x' ]; then
                stty erase `tput kbs`
        fi

        # Set prompt to preference depending if we're a dumb terminal
        # or not
        if [ $TERM == 'dumb' ] || [ $TERM == 'emacs' ]; then
                promptDumb
        else
                promptCMF
        fi

fi

# Determine PATH
paths=('/usr/games'				\
       '/usr/X11R6/bin'				\
       '/usr/X11R7/bin'				\
       '/usr/sbin'				\
       '/opt/bin'				\
       '/opt/sbin'				\
       '/sbin'					\
       '/usr/local/sbin'			\
       "$HOME/.composer/vendor/bin"		\
       "$HOME/perl5/bin"			\
       "$HOME/bin"
      )

# Pre-pend additional directories if required
for path in ${paths[@]}; do

        # Determine if this directory is already in $PATH
        if [ -d $path ]; then
                echo $PATH | egrep '(^|\:)'${path}'(\:|$)' >/dev/null 2>&1 || PATH=${path}:$PATH
        fi

done

# Get rid of pre-pended ':' and double colons
PATH=${PATH#:}
PATH=${PATH//::/:}

# Set default for YASTISBROKEN
YASTISBROKEN=0

# Used for specifying a 'verbose' flag for file operation commands
# (e.g., mv, cp) for operating systems that support it
FILE_OPS_FLAGS=""
FILE_OPS_RM_FLAGS=${FILE_OPTS_RM_FLAGS}

# Operating system PKILL command
PKILL='pkill'

# Set default for awk
AWK=awk

# Make grep(1) output pretty on systems that support color
export CMF_GREP_OPTIONS='--color=auto'

# Set up GOPATH
if type -p go >/dev/null; then
        export GOPATH=${HOME}/go

        if [ -d ${GOPATH}/bin ]; then
                echo $PATH | egrep '(^|\:)'${GOPATH}'/bin(\:|$)' >/dev/null 2>&1 || PATH=$PATH:${GOPATH}/bin
        fi

fi

export PATH

# Set up perl environment
PERL5LIB="${HOME}/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="${HOME}/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"${HOME}/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=${HOME}/perl5"; export PERL_MM_OPT;


# OS-Specific variable(s)
# --------------------------------------------------------------------
case $OSTYPE in

Darwin)
        source $SHELLDIR/bash_darwin
        ;;

*BSD )
        source $SHELLDIR/bash_bsd
        ;;

Linux )
        source $SHELLDIR/bash_linux
        ;;

[uU][nN][iI][xX]* )
        source $SHELLDIR/bash_unix_sv
        ;;

# Note: if we cannot determine the host OS, use SYSV
* )
        source $SHELLDIR/bash_unix_sv
        ;;

esac

# Aliases
# --------------------------------------------------------------------

alias cp='cp -${FILE_OPS_FLAGS}i'
alias rm='rm -${FILE_OPS_FM_FLAGS}i'
alias mv='mv -${FILE_OPS_FLAGS}i'
alias ll='ls -alh'

# Make sure we use the rm binary
if [ -e /bin/rm ]; then
        alias rmrmrm='/bin/rm -${FILE_OPS_RM_FLAGS}f'
else
        alias rmrmrm='rm -${FILE_OPS_RM_FLAGS}f'
fi

# Set up pine/alpine aliases
if type -p alpine >/dev/null; then
        alias alpine="alpine -p ${PINE_REMOTE_CONFIG}"
        alias pine='echo "Mailer:NOTE:Alpine replaces pine\!"'
else
        if type -p pine >/dev/null; then
                alias pine="pine -p ${PINE_REMOTE_CONFIG}"
        fi
fi

# Bloated unstable web browsers.  Coming to a computer near you!
alias kn='${PKILL} -9 netscape-communicator'
alias km='${PKILL} -9 mozilla'
alias kf='${PKILL} -9 firefox'

# An alias to strip out current directory from PATH
alias stripcwd='export PATH=${PATH/:./}'

# Add alias for switching to 256 colors should the need arise
if [[ $TERM =~ ^screen ]]; then
        alias go256='export TERM=screen-256color'
        alias no256='export TERM=screen'
else
        alias go256='export TERM=xterm-256color'
        alias no256='export TERM=xterm'
fi

# Alias for fossil
if type -p fossil >/dev/null; then
        alias fl="fossil"
fi

# Aliases for PHP CodeSniffer
if type -p phpcs >/dev/null; then
       alias phpcs='phpcs --standard=PSR2'
fi

if type -p phpcbf >/dev/null; then
       alias phpcbf='phpcbf --standard=PSR2'
fi

# GREP_OPTIONS is deprecated, so here is a work-around
alias grep="grep ${CMF_GREP_OPTIONS}"

# Program defaults
# --------------------------------------------------------------------

# Determine browser
__sp_bashrc_set_browser

# Determine editor
__sp_bashrc_set_editor

# Determine pager
__sp_bashrc_set_pager

# Bash Command History
# --------------------------------------------------------------------

# Do *not* append the following to our history: consecutive duplicate
# commands, ls, bg and fg, and exit
HISTIGNORE='\&:fg:bg:ls:pwd:cd ..:cd ~-:cd -:cd:jobs:set -x:ls -l:ls -l'

# Don't keep useless history commands.  Note the last pattern is to not
# keep dangerous commands in the history file.  Who really needs to
# repeat the shutdown(8) command accidentally from your command
# history?
HISTIGNORE=${HISTIGNORE}':%1:%2:popd:top:alpine:mutt:clear:shutdown*'
export HISTIGNORE

# Save multi-line commands in history as single line
shopt -s cmdhist

# Disk is cheap.  Memory is cheap.  My memory isn't!  Keep a lot of
# history by default.  10K lines seems to go back about 6 months, and
# captures all of the wacky one-off shell scripts that I might want
# again later.
export HISTSIZE=10000
export HISTFILESIZE=${HISTSIZE}

#
# Bash behavior settings
#

# Correct minor spelling errors in cd commands
shopt -s cdspell

# This option mostly keeps you from needing to run "hash -r" when you
# modify directories in your path
shopt -s checkhash

# Do not delete your precious history file before writing the new one
shopt -s histappend

# This is useful for embedded newlines in commands and quoted arguments
shopt -s lithist

# Enable egrep-style pattern matching
shopt -s extglob

#
# Additional Logic
#

# Set up some default settings
umask 022

# There's a SuSE bug whereby if interactive POSIX mode is enabled,
# then /etc/bash_completion.d/yast2-completion.sh will throw a syntax
# error (see https://bugzilla.novell.com/show_bug.cgi?id=504844)
if [ ! ${YASTISBROKEN} ]; then
        set -o posix
fi

# Host-specific processing.  We only do this if the host-specific file
# exists
HOSTBITS=(${HOSTNAME//./ })

# Extract domain name based on length of HOSTBITS
if [ ${#HOSTBITS[@]} -gt 1 ]; then
        DOMAIN=${HOSTBITS[${#HOSTBITS[@]} - 2]}
else
        DOMAIN=${HOSTNAME}
fi

# Finally, source the appropriate file if it exists and is a regular
# file
if [ $DOMAIN ] && [ -f $SHELLDIR/hosts/$DOMAIN ]; then
        source $SHELLDIR/hosts/$DOMAIN
fi

# Ende
