#!/bin/bash
# ====================================================================
#
# bash_profile
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
# Executed when this is a login shell
#

# Turn off TTY 'start' and 'stop' commands (they default to C-q and C-s,
# respectively, but Bash uses C-s to do a forward history search)
stty start ''
stty stop  ''

OSTYPE=$(uname)
MAIL=/var/mail/$LOGNAME

# System Specific Stuff
case $OSTYPE in

Linux )
        echo 'Linux Detected'
        MAIL=/var/spool/mail/$LOGNAME
        ;;

*BSD )
        echo '*BSD Detected'
        ;;

Darwin )
        echo 'Mac OS X (Darwin) Detected'
        ;;

# By default, assume SysV
* )
        echo "UNIX System 5 (${OSTYPE}) Detected"
        ;;

esac

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
        . ~/.bashrc
fi

export MAIL
uname -a

# Ende
