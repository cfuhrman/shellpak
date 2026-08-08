#
# GNUmakefile
#
# Common targets:
#
#   all         : push out to all machines
#   install     : install ShellPAK locally, while building Emacs packages
#   update      : Same as install, but does not auto-build Emacs packages
#   dist        : create a shellpak tarball
#   clean       : cleans up various files in source directory
#   clean-elc   : Remove all top-level *.elc files from ~/.emacs.d
#   clean-all   : clean + clean-elc + clean-xkcd + clean-tags
#   uninstall   : Removes ShellPAK via `shellpak -u`
#
# Documentation targets:
#
#   docbook     : Generate PDF documentation using docbook (org-mode < 8)
#   html        : Generate HTML-formatted documentation
#   markdown    : Generate markdown-formatted documentation (org-mode >= 8)
#   pdf         : Generate PDF documentation using LaTeX
#   txt         : Generate plain-text documentation (UTF-8 encoding when available)
#   texinfo     : Generate PDF documentation using texinfo (org-mode >= 8)
#
# Created Thu Jan 22 14:51:59 2015 PST
#

#
# Variables
#

SHELLDIR=SHELL

SUBDIRS+=docs
SUBDIRS+=emacs.d
SUBDIRS+=hosts

CLEANDIRS=$(SUBDIRS:%=clean-%)

# Documentation variables
DOCDIRS+=docs
DOCDIRS+=emacs.d
HTMDIRS=$(DOCDIRS:%=html-%)
PDFDIRS=$(DOCDIRS:%=pdf-%)
TXTDIRS=$(DOCDIRS:%=txt-%)
MDDIRS=$(DOCDIRS:%=markdown-%)
DBKDIRS=$(DOCDIRS:%=docbook-%)
TXIDIRS=$(DOCDIRS:%=texinfo-%)

# Host targets
LOCALHOSTS=
# REMOTEHOSTS=shell.example.com
REMOTEHOSTS=
PUBLICCO=../public/

# Determine distribution file.  These lines will produce spewage if
# this isn't a fossil checkout
FOSSIL_BRANCH=$(shell fossil info | awk -F\: '/^tags/ {print $$2}' | sed 's/ //g' | awk -F\, '{print $$NF}')
FOSSIL_REPO=${HOME}/repos/public.fossil
CKOUT_DATE=$(shell fossil info | awk '/^checkout/ { print $$3 }' | sed 's/[-: ]//g')
DISTFILE=shellpak-${FOSSIL_BRANCH}-${CKOUT_DATE}.tar.gz
ZIPFILE=shellpak-${FOSSIL_BRANCH}-${CKOUT_DATE}.zip
GIT_REPO=${HOME}/dev/shpak

# Command options
RCLONE_BIN=rclone
RCLONE_OPTS=-c --progress --stats-one-line -v --exclude-from=global-excludes
RSYNC_BIN=rsync
RSYNC_EXCLUDE=global-excludes
RSYNC_OPTS=-Ccavz --exclude='svn-commit*' --exclude='.AppleDouble' --exclude='*~' --exclude='.DS_Store' --exclude-from=${RSYNC_EXCLUDE} --delete --timeout=30
RSYNC_CONN_OPTS=-e ssh
RSYNC_PUBLIC_OPTS=-Ccavz --exclude=GPATH --exclude=GRTAGS --exclude=GTAGS --exclude=docs/*.txt \
  --exclude=.fslckout --exclude=config --exclude=ext --exclude='.AppleDouble' --exclude='*~' \
  --exclude=_FOSSIL_ --exclude=VERSION --delete
SETUP_BIN=${PWD}/setup.sh

SSH=ssh
SSH_SETUP_CMD="cd ${SHELLDIR} ; ./setup.sh"

TAR=$(shell if [ `uname` != "Darwin" ] && type gtar >/dev/null; then echo 'gtar'; else echo 'tar'; fi)
DRYRUN_OPT=$(shell if echo ${DRYRUN} | egrep '[Yy][Ee][Ss]' >/dev/null ; then echo -n '-r'; fi)
RM_OPTS=$(shell if [ `uname` != "OpenBSD" ]; then echo '-vf'; else echo '-f'; fi)

#
# Targets
#

all: install local remote
	@echo 'All known hosts updated'

uninstall: setup.sh VERSION
	@echo -n 'Removing ShellPAK '
	@cat VERSION
	@${SETUP_BIN} ${DRYRUN_OPT} -u

install: update

update: clean setup.sh
	@echo 'Executing setup.sh for ShellPAK update'
	@${SETUP_BIN} ${DRYRUN_OPT}

windows: rclone emacs-dir
	@echo 'Copying emacs configuration under MS-Windows'
	${RCLONE_BIN} copy emacs.d ${HOMEPATH}/AppData/Roaming/.emacs.d ${RCLONE_OPTS}

local: ${LOCALHOSTS}
	@echo 'All local hosts updated'

remote: ${REMOTEHOSTS}
	@echo 'All remote hosts updated'

disabled: ${DISABLEDHOSTS}
	@echo 'All disabled hosts updated (Are they working now?)'

clean: clean-echo ${CLEANDIRS} clean-dist
	@rm ${RM_OPTS} *.bak
	@rm ${RM_OPTS} *~
	@rm ${RM_OPTS} *-baseline*
	@rm ${RM_OPTS} *-merge*
	@rm ${RM_OPTS} *-original*

clean-echo:
	@echo 'Cleaning old files:'

clean-elc: clean-home-elc

clean-home-elc:
	@echo 'Removing installed elc files:'
	@rm ${RM_OPTS} ${HOME}/.emacs.d/*.elc
	@rm ${RM_OPTS} ${HOME}/.emacs.d/*~
	@rm ${RM_OPTS} ${HOME}/.emacs.d/lisp/*.elc
	@rm ${RM_OPTS} ${HOME}/.emacs.d/lisp/*~
	@rm ${RM_OPTS} ${HOME}/.emacs.d/thirdparty/*.elc
	@rm ${RM_OPTS} ${HOME}/.emacs.d/thirdparty/*~

clean-xkcd:
	@echo 'Removing left-over xkcd comics:'
	@rm ${RM_OPTS}r ${HOME}/.emacs.d/xkcd/*

clean-all: clean clean-elc clean-xkcd

clean-dist:
	@rm ${RM_OPTS} shellpak*.tar.gz
	@rm ${RM_OPTS} shellpak*.zip
	@rm ${RM_OPTS}r shellpak

clean-cache:
	@rm ${RM_OPTS}r ${HOME}/.emacs.d/eln-cache

# WARNING: Will remove *all* installed packages.  Use with care!
clean-elpa: clean-elc
	@rm ${RM_OPTS}r ${HOME}/.emacs.d/elpa/*

public: clean-all
	@echo 'Syncing with public repository'
	${RSYNC_BIN} ${RSYNC_PUBLIC_OPTS} . ${PUBLICCO}

# Target for exporting changes from public Fossil repository
git-export: ${GIT_REPO}
	@echo 'Exporting changes to ${GIT_REPO}'
	fossil git export ${GIT_REPO} --quiet

# Target for importing changes from public Fossil repository
git-import: .git
	@echo 'git-import target has been replaced by git-export target'

dist: ${DISTFILE}

zip: ${ZIPFILE}

version: VERSION

VERSION: fossil
	@fossil info | awk '/^checkout/ { printf "[%s] %s %s\n", substr($$2, 1, 10), $$3, $$4 }' > VERSION

subdirs: ${SUBDIRS}
${SUBDIRS} :
	${MAKE} -C $@

${CLEANDIRS} :
	${MAKE} -C $(@:clean-%=%) clean

# Binary targets
emacs:
	@which emacs >/dev/null

fossil:
	@which fossil >/dev/null

rclone:
	@which rclone >/dev/null

# Directory targets
emacs-dir: ${HOMEPATH}/AppData/Roaming/.emacs.d

${HOMEPATH}/AppData/Roaming/.emacs.d:
	mkdir -p ${HOMEPATH}/AppData/Roaming/.emacs.d

#
# Documentation Targets
#

readme: markdown
	@cp -rp docs/README.md .
	${MAKE} -C docs clean

docbook: emacs ${DBKDIRS}
${DBKDIRS}:
	${MAKE} -C $(@:docbook-%=%) docbook

html: emacs ${HTMDIRS}
${HTMDIRS}:
	${MAKE} -C $(@:html-%=%) html

markdown: emacs ${MDDIRS}
${MDDIRS}:
	${MAKE} -C $(@:markdown-%=%) markdown

pdf: emacs ${PDFDIRS}
${PDFDIRS}:
	${MAKE} -C $(@:pdf-%=%) pdf

txt: emacs ${TXTDIRS}
${TXTDIRS}:
	${MAKE} -C $(@:txt-%=%) txt

texinfo: emacs ${TXIDIRS}
${TXIDIRS}:
	${MAKE} -C $(@:texinfo-%=%) texinfo

#
# Host Targets
#

${LOCALHOSTS}:
	@echo "Propagating to $@"
	@${RSYNC_BIN} ${RSYNC_OPTS} ${RSYNC_CONN_OPTS} . $@:${SHELLDIR}
	@${SSH} $@ ${SSH_SETUP_CMD}

${REMOTEHOSTS}:
	@echo "Propagating to $@"
	@${RSYNC_BIN} ${RSYNC_OPTS} ${RSYNC_CONN_OPTS} . $@:${SHELLDIR}
	@${SSH} $@ ${SSH_SETUP_CMD}

${DISABLEDHOSTS}:
	@echo "Propagating to $@"
	@${RSYNC_BIN} ${RSYNC_OPTS} ${RSYNC_CONN_OPTS} . $@:${SHELLDIR}
	@${SSH} $@ ${SSH_SETUP_CMD}

#
# Distribution targets
#

${DISTFILE}: clean version
	@fossil tarball ${FOSSIL_BRANCH} ${DISTFILE}
	@echo "Successfully created ${DISTFILE}"

${ZIPFILE}: clean version
	@fossil zip ${FOSSIL_BRANCH} ${ZIPFILE}
	@echo "Successfully created ${ZIPFILE}"

.PHONY: subdirs ${SUBDIRS}
.PHONY: subdirs ${CLEANDIRS}
.PHONY: subdirs ${HTMDIRS}
.PHONY: subdirs ${PDFDIRS}
.PHONY: subdirs ${TXTDIRS}
.PHONY: subdirs ${MDDIRS}
.PHONY: subdirs ${DBKDIRS}
.PHONY: subdirs ${TXIDIRS}
.PHONY: clean clean-elc docbook html pdf txt markdown texinfo

# GNUmakefile ends here
