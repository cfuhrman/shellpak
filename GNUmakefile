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
FOSSIL_BRANCH=$(shell fossil info | grep "^tags" | awk -F\: '{print $$2}' | sed 's/ //g' | awk -F\, '{print $$NF}')
FOSSIL_REPO=${HOME}/repos/public.fossil
CKOUT_DATE=$(shell fossil info | grep "^checkout" | awk '{ print $$3 }' | sed 's/[-: ]//g')
DISTFILE=shellpak-${FOSSIL_BRANCH}-${CKOUT_DATE}.tar.gz

# Command options
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

update: txt setup.sh
	@echo 'Executing setup.sh for ShellPAK update'
	@${SETUP_BIN} ${DRYRUN_OPT}

tags: gtags

# Note that this target may not work as expected when used on a case
# insensitive file system (e.g., default HFS+ Apple file system)
gtags:
	@echo 'Generating tags using global(1)'
	@gtags

local: ${LOCALHOSTS}
	@echo 'All local hosts updated'

remote: ${REMOTEHOSTS} ${POLARHOSTS}
	@echo 'All remote hosts updated'

disabled: ${DISABLEDHOSTS}
	@echo 'All disabled hosts updated (Are they working now?)'

clean: clean-echo ${CLEANDIRS} clean-dist clean-tags
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

clean-tags: clean-gtags

clean-gtags:
	@rm ${RM_OPTS} GPATH GTAGS GRTAGS

clean-all: clean clean-elc clean-xkcd clean-tags

clean-dist: 
	@rm ${RM_OPTS} shellpak*.tar.gz
	@rm ${RM_OPTS}r shellpak

# WARNING: Will remove *all* installed packages.  Use with care!
clean-elpa: clean-elc
	@rm ${RM_OPTS}r ${HOME}/.emacs.d/elpa/*

public: readme
	@rm ${RM_OPTS} docs/README.md
	@echo 'Syncing with public repository'
	${RSYNC_BIN} ${RSYNC_PUBLIC_OPTS} . ${PUBLICCO}

# Target for importing changes from public Fossil repository
git-import: .git
	@echo 'Importing changes from Fossil'
	fossil export --git ${FOSSIL_REPO} | \
          sed 's/^committer cfuhrman <cfuhrman>/committer Christopher M. Fuhrman <cfuhrman@pobox.com>/g' | \
          git fast-import
	git reset --hard trunk

dist: ${DISTFILE}

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

#
# Documentation Targets
#

readme: markdown
	@cp -rp docs/README.md .

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

${LOCALHOSTS}: txt
	@echo "Propagating to $@"
	@${RSYNC_BIN} ${RSYNC_OPTS} ${RSYNC_CONN_OPTS} . ${USER}@$@:${SHELLDIR}
	@${SSH} $@ ${SSH_SETUP_CMD}

${REMOTEHOSTS}: txt
	@echo "Propagating to $@"
	@${RSYNC_BIN} ${RSYNC_OPTS} ${RSYNC_CONN_OPTS} . ${USER}@$@:${SHELLDIR}
	@${SSH} $@ ${SSH_SETUP_CMD}

${DISTFILE}: clean clean-tags txt version
	@fossil tarball ${FOSSIL_BRANCH} ${DISTFILE}
	@echo "Successfully created ${DISTFILE}"

.PHONY: subdirs ${SUBDIRS}
.PHONY: subdirs ${CLEANDIRS}
.PHONY: subdirs ${HTMDIRS}
.PHONY: subdirs ${PDFDIRS}
.PHONY: subdirs ${TXTDIRS}
.PHONY: subdirs ${MDDIRS}
.PHONY: subdirs ${DBKDIRS}
.PHONE: subdirs ${TXIDIRS}
.PHONY: clean clean-elc docbook html pdf txt markdown texinfo

# Ende
