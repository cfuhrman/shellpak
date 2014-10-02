#!/usr/bin/env bash
# ====================================================================
#
# setup.sh
# 
# Copyright (c) 2003 Christopher M. Fuhrman
# All rights reserved.
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the Simplified BSD License (also
# known as the "2-Clause License" or "FreeBSD License".) 
#
# --------------------------------------------------------------------
#
# DESCRIPTION:
#
#   Set up my personal shell environment to taste, backing up old
#   files and linking appropriate dot files
#
# USAGE:
#
#   bash setup.sh [-hnpru] [-d <directory>] [-b <directory>]
#
# REQUIREMENTS:
#
#   - rsync(1)
#   - bash(1) v3.0 or greater
#   - GNU make compatible make(1)
#
# ====================================================================

# Variables
SHELLDIR=${HOME}/SHELL
BACKUPDIR=${HOME}/Backup/shell
HOMETMPDIR=${HOME}/tmp
RSYNC=rsync
RSYNC_EXCLUDE=global-excludes
RSYNC_OPTS="-Ccav --delete --exclude-from=${RSYNC_EXCLUDE}"
MAKE=make
COPYRIGHT='Copyright (c) 2000-2014 Christopher M. Fuhrman'
OUTPUTSPACING=55
DRYRUN=""
UNINSTALL=0
BUILD_EMACS_PACKAGES=0
EMACS_OUTPUT_LOG=${HOME}/tmp/emacs-pkg-install-$$-`date +%y%m%d`.log
NOLINK=0            # If set to 1 (true), then the following apply:
                    #   - Linked files will not be created
                    #   - ~/tmp directory will not be created
                    #   - Existing files in $HOME will not be backed up

# Read only constants
readonly L1=1
readonly L2=2
readonly TRUE=1
readonly FALSE=0

# Define colors, but only if this is *not* a dumb terminal
if [[ ${TERM} == 'dumb' ]]; then
        RED=
        GREEN=
        YELLOW=
        BLUE=
        MAGENTA=
        CYAN=
        WHITE=
        BOLD=
        UNDERLINE=
        NORMAL=
else
        RED=$(tput -T ${TERM} setaf 1)
        GREEN=$(tput -T ${TERM} setaf 2)
        YELLOW=$(tput -T ${TERM} setaf 3)
        BLUE=$(tput -T ${TERM} setaf 4)
        MAGENTA=$(tput -T ${TERM} setaf 5)
        CYAN=$(tput -T ${TERM} setaf 6)
        WHITE=$(tput -T ${TERM} setaf 7)
        BOLD=$(tput -T ${TERM} bold)
        UNDERLINE=$(tput sgr 0 1)
        NORMAL=$(tput -T ${TERM} sgr0)
fi

# Dot-files to link
HOMEDOTFILES=('bash_logout'   \
              'bash_profile'  \
              'bashrc'        \
              'emacs.d'       \
              'gitconfig'     \
              'indent.pro'    \
              'perltidyrc'    \
              'tmux.conf'     \
              'screenrc'      \
              'Xresources')

# Use GNU Make if available, otherwise blindly assume that the system
# make is compatible.
makepaths=('/usr/pkg/bin/gmake'      '/usr/bin/gnumake'     \
           '/opt/csw/bin/gmake'      '/usr/sfw/bin/gmake'   \
           '/opt/freeware/bin/gmake' '/usr/ccs/bin/make'    \
           '/opt/csw/bin/make'       '/usr/bin/make')

for makeprog in ${makepaths[@]};
do
        if [ -e ${makeprog} ]; then
                MAKE=${makeprog}
                break
        fi
done

# Determine version and output to file
if [[ -f .fslckout || -f _FOSSIL_ ]]; then
        SHELLPAK_VERSION=$( fossil info | grep ^checkout | awk '{ printf "[%s] %s %s", substr($2, 0, 10), $3, $4 }' )
        echo ${SHELLPAK_VERSION} > VERSION
elif [ -f VERSION ]; then
        SHELLPAK_VERSION=$( cat VERSION )
fi

# Functions
# --------------------------------------------------------------------

# Function: doUninstall
#
# Uninstalls shellpak

doUninstall ()
{

        echo ''
        echo '===================================================================='
        echo -ne "${BOLD}${RED}"
        echo '                           WARNING!'
        echo -ne "${NORMAL}"
        cat <<EOF
--------------------------------------------------------------------

You are about to uninstall ShellPAK.  Are you SURE you want to do
this?

====================================================================

EOF

        select opt in Yes No
        do
                # Bash versions >= 4.0 support doing ${opt^^} to get uppercase.
                # Unfortunately, Apple still insists on shipping Bash 3.something
                # in OS X, so use tr(1) to make $opt uppercase.
                case $( echo $opt | tr '[:lower:]' '[:upper:]' ) in

                YES)
                        echo ''
                        echo '--> Proceeding'
                        break
                        ;;

                NO)
                        echo ''
                        echo '--> Aborting per request'
                        exit 0
                        ;;

                *)
                        echo '--> I did not understand your answer.  Try again'

                esac

        done

        # Remove linked dot files
        echo ''
        inform $L1 $TRUE "Removing linked dot-files"
        for file in ${HOMEDOTFILES[@]}; do

                DOTFILE=${HOME}/.${file}
                LINKFILE=$(readlink ${DOTFILE})

                if [[ -h ${DOTFILE} && ${SHELLDIR%/}/${file} == ${LINKFILE} ]]; then

                        inform $L2 $FALSE "Removing ${DOTFILE}"

                        if [ ${#DRYRUN} -eq 0 ]; then
                                rm ${DOTFILE}
                        fi

                        echo "${GREEN}done${NORMAL}"

                        if [ -e ${BACKUPDIR}/.${file} ]; then

                                if [ ${#DRYRUN} -eq 0 ]; then
                                        mv ${BACKUPDIR}/.${file} ${HOME}
                                        echo "    Restored original ${file}"
                                fi

                        fi

                fi

        done

        # Restore .emacs file if it exists
        if [ -f ${BACKUPDIR}/.emacs ]; then
            inform $L2 $FALSE "Restoring original .emacs"
            mv ${BACKUPDIR}/.emacs ${HOME}
            echo "${GREEN}done${NORMAL}"            
        fi

        # Finally remove SHELLDIR
        if [ -d ${SHELLDIR} ]; then
                inform $L2 $FALSE "Removing ${SHELLDIR}"

                if [ ${#DRYRUN} -eq 0 ]; then
                        rm -rf ${SHELLDIR}
                fi

                echo -e "${GREEN}done${NORMAL}"
        fi

        echo ''
        echo "${BOLD}ShellPAK${NORMAL} removed from the home directory of ${USER} on ${CYAN}${HOSTNAME}${NORMAL}!"

} # doUninstall()

# Function: headerDisplay
#
# Displays a header for the world to see

headerDisplay ()
{

        # Make the version string magenta
        PRETTY_VERSION=${SHELLPAK_VERSION/\[/\[${MAGENTA}}
        PRETTY_VERSION=${PRETTY_VERSION/\]/${NORMAL}\]}

        # Header information
        echo '------------------------------------------------------------------------'
        echo "${BOLD}ShellPAK${NORMAL} ${CYAN}${0##*/}${NORMAL}"
        echo ${PRETTY_VERSION}
        echo ''
        echo ${COPYRIGHT}
        echo '------------------------------------------------------------------------'

        # Date and host
        echo ''
        echo -n 'DATE : '
        date
        echo -n 'HOST : '
        hostname
        echo ''

} # headerDisplay()

# Function: inform
#
# Displays a message on STDOUT
#
# Parameters:
#
#   level   : 1 or 2
#   newline : [0|1] whether to append newline
#   msg     : Message to display

inform ()
{
        local level=$1
        local newline=$2
        local msg=$3

        if [ $level -eq $L1 ]; then
                local delimiter='=='
        else
                local delimiter='--'
        fi

        if [ $newline -eq $TRUE ]; then
                printf "%s> %s\n" $delimiter "$msg"
        else
                printf "%s> %-${OUTPUTSPACING}s ... " $delimiter "$msg"
        fi
}

# Function: usage
#
# Displays usage

usage ()
{

        cat 1>&2 <<STDERR
usage: ${0##*/} -h This screen                             \\
                -d (Default:\$HOME/SHELL) Target directory  \\
                -b (Default:\$HOME/Backup/shell) Location   \\
                   of backup files
                -n Do _not_ link files                     \\
                -p Automatically install emacs packages    \\
                -u Uninstall ShellPAK                      \\
                -r perform a trial run with no changes made
                   (implies -n)

STDERR

} # usage()


# --------------------------------------------------------------------

headerDisplay

args=$(getopt d:b:hpnru $*)

set -- $args

while [ $# -gt 0 ]
do

        case "$1" in
        -h)
                usage
                exit 0
                ;;

        -d)
                SHELLDIR=$2; shift
                ;;

        -n)
                NOLINK=1
                inform $L1 $TRUE 'Shell-related files will _not_ be linked to $HOME'
                ;;

        -p)
                BUILD_EMACS_PACKAGES=1
                inform $L1 $TRUE 'Emacs packages will automatically be installed'
                ;;

        -r)
                DRYRUN=' --dry-run '
                NOLINK=1
                inform $L1 $TRUE 'DRY-RUN: No changes will be propagated'
                ;;

        -b)
                BACKUPDIR=$2; shift;
                inform $L1 $TRUE "Existing files to be backed up in ${BACKUPDIR}"
                ;;

        -u)
                UNINSTALL=1
                inform $L1 $TRUE 'UNINSTALL invoked'
                ;;

        --)
                shift; break
                ;;
        esac
        shift

done

# Are we uninstalling?
if [ ${UNINSTALL} -eq 1 ]; then
        doUninstall
        exit 0
fi

# Is rsync installed on this system?
if ! type rsync >/dev/null; then
        inform $L1 $TRUE "${RED}ERROR${NORMAL}: rsync is not installed on this system.  Cowardly aborting!"
        exit 1
fi

# First make sure our backup directory exists but only if NOLINK is
# not 1
if [ ${NOLINK} -ne 1 ]; then

        BACKUPFLAG=0

        if [[ ! -d ${BACKUPDIR} ]]; then

                # Create backup directory for old files
                inform $L1 $FALSE "Creating ${BACKUPDIR} for archived files"
                mkdir -p ${BACKUPDIR}
                BACKUPFLAG=1
                echo -e "${GREEN}done${NORMAL}"

        fi

        # Iterate through each dot-file, copying it over as
        # appropriate
        for file in ${HOMEDOTFILES[@]}; do

                DOTFILE="${HOME}/.${file}"
                BACKUPFILE="${BACKUPDIR}/.${file}"

                # Only move the file if it exists and it is not a symlink
                if [[ -e ${DOTFILE} && ! -e ${BACKUPFILE} && ! -h ${DOTFILE} ]]; then
                        inform $L2 $FALSE "Moving .${file} to ${BACKUPDIR}"
                        mv ${DOTFILE} ${BACKUPDIR}
                        echo -e "${GREEN}done${NORMAL}"                        
                elif [[ -h ${DOTFILE} && ! ${BACKUPFLAG} ]]; then
                        inform $L2 $TRUE "Removing old symlink .${file}"
                        rm ${DOTFILE}
                fi

        done

        # See if there is a pre-existing .emacs file
        if [ -f ${HOME}/.emacs ]; then
            inform $L2 $FALSE "Moving .emacs to ${BACKUPDIR}"
            mv ${HOME}/.emacs ${BACKUPDIR}
            echo -e "${GREEN}done${NORMAL}"
        fi

fi

# Create SHELL directory if it doesn't exist
if [[ ${#DRYRUN} -eq 0 && ! -d ${SHELLDIR} ]]; then
        inform $L2 $FALSE "Creating ${SHELLDIR} for our shell-related files"
        mkdir -p ${SHELLDIR}
        echo -e "${GREEN}done${NORMAL}"
fi

# Now, rsync over necessary files, but only if we are not a directory
# that matches SHELLDIR.  This gets around issues such as when $HOME
# resides in a symlinked directory.
if [[ ${SHELLDIR#$(dirname "$(dirname "$SHELLDIR")")/} != ${PWD#$(dirname "$(dirname "$PWD")")/} ]]; then

        # Generate any necessary documentation prior to synchronizing,
        # but only if we are in a fossil checkout
        if [[ -f .fslckout || -f _FOSSIL_ ]]; then
                inform $L1 $TRUE 'Generating documentation'
                ${MAKE} ${DRYRUN} txt
        fi

        # And now rsync(1) things over
        inform $L1 $TRUE "Synchronize ${SHELLDIR}"
        ${RSYNC} ${DRYRUN} ${RSYNC_OPTS} . ${SHELLDIR}

else
        inform $L1 $TRUE 'Synchronization not necessary'
fi

# Set up appropriate symlinks
if [ ${NOLINK} -ne 1 ]; then

        # Set up links
        for file in ${HOMEDOTFILES[@]}; do

                # Set to current dot file (e.g., .bashrc)
                DOTFILE=${HOME}/.${file}

                # Should the link or file not exist, then link it as
                # appropriate
                if [ ! -e ${DOTFILE} ]; then
                        inform $L2 $FALSE "Linking ${DOTFILE}"
                        ln -s ${SHELLDIR}/${file} ${DOTFILE}
                        echo -e "${GREEN}done${NORMAL}"
                fi

        done

fi

# Create ~/tmp directory if it doesn't exist
if [[ ${NOLINK} -ne 1 && ! -d ${HOMETMPDIR} ]]; then
        inform $L1 $FALSE "Creating ${HOMETMPDIR}"
        mkdir ${HOMETMPDIR}
        echo -e "${GREEN}done${NORMAL}"
fi

# Automatically build emacs packages
if [[ ${BUILD_EMACS_PACKAGES} -eq 1 ]]; then
        inform $L1 $FALSE 'Building Emacs Packages'
        tmplog=$(mktemp /tmp/${0##*/}.XXXXX) || exit 1
        make emacs-packages >${tmplog} 2>&1
        echo -e "${GREEN}done${NORMAL}"
        mv $tmplog $EMACS_OUTPUT_LOG
        inform $L2 $TRUE "Package log located at ${EMACS_OUTPUT_LOG}"
fi

# We're done
echo ''
echo "${BOLD}ShellPAK${NORMAL} setup complete on ${CYAN}${HOSTNAME}${NORMAL}!"

# Ende
