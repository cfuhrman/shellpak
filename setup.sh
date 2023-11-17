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
#   bash setup.sh -h
#
# REQUIREMENTS:
#
#   - rsync(1)
#   - bash(1) v3.0 or greater
#   - GNU make compatible make(1)
#   - bc(1)
#
# ====================================================================

SHELLDIR=${HOME}/SHELL
BACKUPDIR=${HOME}/Backup/shell
HOMETMPDIR=${HOME}/tmp
RSYNC=rsync
RSYNC_EXCLUDE=global-excludes
RSYNC_OPTS="-Ccav --perms --chmod=go-rw --delete --exclude-from=${RSYNC_EXCLUDE}"
MAKE=make
COPYRIGHT='Copyright (c) 2000-2023 Christopher M. Fuhrman'
OUTPUTSPACING=55
DRYRUN=""			# Dry-run option to pass to rsync(1)
UNINSTALL=0
GOINSTALL=0
GOPATH=${HOME}/go
PYINSTALL=0
PLINSTALL=0
NOLINK=0            # If set to 1 (true), then the following apply:
                    #   - Linked files will not be created
                    #   - ~/tmp directory will not be created
                    #   - Existing files in $HOME will not be backed up

# Public: Indicates what python version to use
: ${PYTHON_VERSION:=3}

# Public: Indicates location of pip program
: ${PIP_BIN:=pip3}

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

# Private: List of dot-files to link
HOMEDOTFILES=('bash_logout'			\
              'bash_profile'			\
              'bashrc'				\
              'emacs.d'				\
              'gitconfig'			\
              'indent.pro'			\
              'mg'				\
              'nanorc'				\
              'perltidyrc'			\
              'tmux.conf'			\
              'screenrc'			\
              'Xmodmap'				\
              'Xresources'
             )

# Private: List of possible locations to search for make(1) binary
#
# Use GNU Make if available, otherwise blindly assume that the system
# make is compatible
MAKEPATHS=('/usr/pkg/bin/gmake'                 \
           '/usr/local/bin/gmake'               \
           '/usr/bin/gnumake'                   \
           '/opt/csw/bin/gmake'                 \
           '/usr/sfw/bin/gmake'                 \
           '/opt/freeware/bin/gmake'            \
           '/usr/ccs/bin/make'                  \
           '/opt/csw/bin/make'                  \
           '/usr/bin/make'
          )

for makeprog in ${MAKEPATHS[@]}; do
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

# Private: Uninstalls shellpak
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

EOF

        if [[ -d ${GOPATH} && $GOINSTALL -ne 0 ]]; then
        	cat <<EOF
Note that contents in ${GOPATH} will be removed as well!!!!
Better save it before continuing!

EOF
        fi

        echo '===================================================================='
        echo ''

        select opt in Yes No
        do
                case ${opt^^} in

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

        if [ $GOINSTALL -ne 0 ]; then
        	inform $L2 $FALSE \
        	       "Removing go environment"

        	rm -rf ${GOPATH}
        	echo "${GREEN}done${NORMAL}"
        fi

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
}

# Function: Sets up directory structure for the go programming language
goSetup ()
{
        local GOBIN=${GOPATH}/bin
        local GOSRC=${GOPATH}/src
        local GOGITHUB=${GOSRC}/github.com

        if [ -d $GOPATH ]; then
        	inform $L1 $TRUE \
        	       "Go development has already been set up"
        	return
        fi

        inform $L1 $TRUE "Setting up development environment for golang"

        # Note we assume the github username is the same as the login
        # name.
        local goSetup_git_user=${GITHUB_USER_NAME:=${USER}}

        # Create the directories
        for goDir in $GOPATH $GOBIN $GOSRC $GOGITHUB
        do
        	mkdir -p $goDir
        done

        if [ ! -d ${GOGITHUB}/$goSetup_git_user ]; then
        	mkdir ${GOGITHUB}/$goSetup_git_user
        fi

        inform $L2 $TRUE "Environment set up for golang"

        if ! type -p go >/dev/null; then
        	inform $L2 $TRUE \
        	       "${BOLD}$YELLOW}NOTICE:${NORMAL} Go binary not found in path"
                return
        fi

        local GOVERSION=$(go version | awk '{ print $3 }' | sed 's/^go\([0-9]*\.[0-9]*\).*/\1/')

        # Choose installation command based on go version
        if (( $(echo "$GOVERSION < 1.18" | bc -l) )); then
                GOINSTALLCMD="get"
                GOPKGVERSION=""
        else
                GOINSTALLCMD="install"
                GOPKGVERSION="@latest"
        fi

        # Install gocode for auto-completion
        if [ ! -f {$GOBIN}/gocode ]; then
        	inform $L2 $TRUE "Installing gocode"
        	go ${GOINSTALLCMD} github.com/nsf/gocode${GOPKGVERSION}
        fi

        # Install gotags for go-direx
        if [ ! -f {$GOBIN}/gotags ]; then
        	inform $L2 $TRUE "Installing gotags"
        	go ${GOINSTALLCMD} github.com/jstemmer/gotags${GOPKGVERSION}
        fi

        # Install gopls
        inform $L2 $TRUE "Installing go language server"
        GO111MODULE=on go ${GOINSTALLCMD} golang.org/x/tools/gopls@latest

        inform $L1 $TRUE "Installation of go modules complete!"
}

# Private: Installs Perl Language Server
#
# NOTE: Installation of the following perl libraries via your
# operating systems package manager (apt, zypper, dnf) strongly
# recommended prior to running this function:
#
#  * Coro
#  * IO-AIO
#  * Moose
#
plSetup ()
{
        # Should AnyEvent (a requirement for Coro) fail, be sure to
        # check output for DNS failures.  In some cases, an ISP may
        # have their DNS configured to return a bogus address should a
        # lookup fail, which can confuse the AnyEvent test suite.
        local PERL_MODULES=("Perl::LanguageServer"
        		   )

        export PATH=~/perl5/bin:$PATH
        export CC=$( which gcc )

        inform $L1 $TRUE "Install Perl Language Server (this may take a while)"

        for module in ${PERL_MODULES[@]}; do
        	inform $L2 $TRUE " ... ${module}"
        	cpan install $module
        done

        unset CC
        inform $L2 $TRUE "Done"
}

# Private: Installs the tools necessary for python development
pySetup ()
{
        local PYTHON_PKGS=('autopep8'		\
        		   'flake8'		\
        		   'jedi'		\
        		   'setuptools-black'	\
        		   'virtualenv'		\
        		   'yapf'
        		  )

        # Make sure that pip is installed
        if ! type ${PIP_BIN} >/dev/null; then
        	inform $L1 $TRUE "${RED}ERROR${NORMAL}: python ${PIP_BIN} is not installed on this system.  Cowardly aborting!"
        	exit 1
        fi

        inform $L1 $TRUE "Setting up development environment for python${PYTHON_VERSION}"

        for pyPkg in ${PYTHON_PKGS[@]}; do
        	inform $L2 $TRUE " ${pyPkg}"
        	${PIP_BIN} install --prefix=$HOME $pyPkg
        done
}

# Private: Displays a header for the world to see
headerDisplay ()
{
        # Private: Determines formatting of version string
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
}

# Private: Displays a message on STDOUT
#
# level   - 1 or 2
# newline - [0|1] whether to append newline
# msg     - Message to display
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

# Private: Displays usage
usage ()
{
        cat 1>&2 <<STDERR
usage: ${0##*/} -h This screen                             \\
                -d (Default:\$HOME/SHELL) Target directory  \\
                -b (Default:\$HOME/Backup/shell) Location   \\
                   of backup files
                -n Do _not_ link files                     \\
                -g Set up/remove GoLang Development        \\
                -l Set up Perl Language Server             \\
        	-p Set up Python Development               \\
                -u Uninstall ShellPAK                      \\
                -r perform a trial run with no changes made
                   (implies -n)

STDERR
}


# --------------------------------------------------------------------

headerDisplay

args=$(getopt d:b:hlnrpgu $*)

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
                inform $L1 $TRUE "Shell-related files will ${UNDERLINE}not${NORMAL} be linked to \$HOME"
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

        -g)
        	GOINSTALL=1
        	;;

        -p)
        	PYINSTALL=1
        	;;

        -l)
        	PLINSTALL=1
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

        # Now rsync(1) things over
        inform $L1 $TRUE "Synchronize ${SHELLDIR}"
        ${RSYNC} ${DRYRUN} ${RSYNC_OPTS} . ${SHELLDIR}

else
        inform $L1 $TRUE 'Synchronization not necessary'
fi

# Do we want to set up our environment for golang development?
if [ ${GOINSTALL} -ne 0 ]; then
        goSetup
fi

# Do we want to set up our environment for perl development?
if [ ${PLINSTALL} -ne 0 ]; then
        plSetup
fi

# Do we want to set up our environment for python development?
if [ ${PYINSTALL} -ne 0 ]; then
        pySetup
fi

# Set up appropriate symlinks
if [ ${NOLINK} -ne 1 ]; then

        # Set up links
        for file in ${HOMEDOTFILES[@]}; do

                DOTFILE=${HOME}/.${file}
                LINKFILE=$(readlink ${DOTFILE})

                # Remove existing link file if it does not link to
                # desired file
                if [[ -h ${DOTFILE} && ${SHELLDIR%/}/${file} != ${LINKFILE} ]]; then
			inform $L2 $FALSE "Removing old link ${DOTFILE}"
                        rm ${DOTFILE}
			echo -e "${YELLOW}removed${NORMAL}"
                fi
                
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

# We're done
echo ''
echo "${BOLD}ShellPAK${NORMAL} setup complete on ${CYAN}${HOSTNAME}${NORMAL}!"

# setup.sh ends here
