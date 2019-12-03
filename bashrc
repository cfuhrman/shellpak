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
source $SHELLDIR/aliases.commands
source $SHELLDIR/aliases.hosts
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
        browsers=('google-chrome'		\
                  'chromium'			\
                  'firefox'			\
                  'epiphany'			\
                  'konqueror'			\
                  'mozilla'			\
                  'w3m'				\
                  'elinks'			\
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
       "$HOME/vendor/bin"			\
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

# Set default for awk
AWK=awk

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
export HISTTIMEFORMAT="[%F %T]: "

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

# bashrc ends here
