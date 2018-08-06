<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. Description</a>
<ul>
<li><a href="#sec-1-1">1.1. Requirements</a></li>
<li><a href="#sec-1-2">1.2. Supported Platforms</a></li>
</ul>
</li>
<li><a href="#sec-2">2. Obtaining ShellPAK</a>
<ul>
<li><a href="#sec-2-1">2.1. Alternate Repositories</a></li>
</ul>
</li>
<li><a href="#sec-3">3. Installation</a>
<ul>
<li><a href="#sec-3-1">3.1. Other setup options</a></li>
</ul>
</li>
<li><a href="#sec-4">4. Bugs</a></li>
<li><a href="#sec-5">5. Author</a></li>
<li><a href="#sec-6">6. Copyright</a></li>
</ul>
</div>
</div>



# Description<a id="sec-1" name="sec-1"></a>

ShellPAK allows for the automatic installation and configuration of
my personal [GNU Bash](http://www.gnu.org/software/bash/) shell environment as well as the configuration
of the following programs:

-   [Git](http://git-scm.com)
-   [GNU Emacs](http://www.gnu.org/software/emacs/)
-   [GNU Nano](https://www.nano-editor.org/)
-   [GNU Screen](http://www.gnu.org/software/screen/)
-   [GNU Indent](https://www.gnu.org/software/indent/)
-   [mg Text Editor](http://homepage.boetes.org/software/mg/)
-   [Perl::Tidy](http://search.cpan.org/~shancock/Perl-Tidy-20140711/lib/Perl/Tidy.pod)
-   [tmux](http://tmux.sourceforge.net)

This code is specific to my own needs and is here in the hopes it
will be useful as a reference.

## Requirements<a id="sec-1-1" name="sec-1-1"></a>

-   GNU Bash version 3.2 or greater
-   GNU Emacs 24.1 or greater (Emacs 24.4 or greater preferred)
-   GNU Make
-   rsync

## Supported Platforms<a id="sec-1-2" name="sec-1-2"></a>

The following operating systems are known to be supported:

-   Mac OS X 10.13 (Darwin)
-   NetBSD 6.1.x
-   OpenBSD 5.8
-   CentOS 6.5
-   Debian 9 (Stretch) and derivatives
-   SunOS solaris 5.10
-   AIX 1 7

In addition, ShellPAK has been known to run on OpenSUSE, SuSE
Enterprise Linux, Ubuntu, FreeBSD, and Cygwin.

# Obtaining ShellPAK<a id="sec-2" name="sec-2"></a>

As of <span class="timestamp-wrapper"><span class="timestamp">&lt;2014-07-30 Wed&gt;</span></span>, [ShellPAK](https://chiselapp.com/user/cfuhrman/repository/shpak/home) is managed by the [Fossil](http://fossil-scm.org)
distributed version control system.  To check out a repository:

    # Clone the repository
    $ fossil clone https://chiselapp.com/user/cfuhrman/repository/shpak ~/shellpak.fossil
    
    # Check out the repository
    $ mkdir shellpak
    $ cd shellpak
    $ fossil open ~/shellpak.fossil

Further information on Fossil can be found at this [Quick Start
Guide](http://www.fossil-scm.org/index.html/doc/trunk/www/quickstart.wiki).

## Alternate Repositories<a id="sec-2-1" name="sec-2-1"></a>

[ShellPAK](https://chiselapp.com/user/cfuhrman/repository/shpak/home) is also available for checkout from the following
locations:

-   **Git:** [GitHub](https://github.com/cfuhrman/shpak), [BitBucket](https://bitbucket.org/chrisfuhrman/shellpak)
-   **Fossil:** [FuhrWerks](https://fossil.fuhrwerks.com/shpak)
-   **Bazaar:** [Launchpad](https://code.launchpad.net/~cfuhrman/shpak/trunk)

Be aware that portions of the ShellPAK infrastructure relies on
Fossil.  Conversion to support the conventions of an alternative
revision control system is an exercise left to the user.

# Installation<a id="sec-3" name="sec-3"></a>

    # Install ShellPAK
    $ ./setup.sh

Note that during installation, ShellPAK will store dot-files (e.g.,
`bashrc`, `bash_profile`, etc) in `~/Backup/shell` (or whatever is
specified by the `-b` flag).  These files will be restored during
un-installation (via the `-u` flag)

## Other setup options<a id="sec-3-1" name="sec-3-1"></a>

    # Perform a dry-run
    $ ./setup.sh -r
    
    # Install into another directory
    $ ./setup.sh -d /path/to/directory
    
    # Do not automatically link files during installation
    $ ./setup.sh -n
    
    # Set up go programming environment
    $ ./setup.sh -g
    
    # Set up python development environment
    $ ./setup.sh -p
    
    # Set up python3 development environment
    $ PYTHON_VERSION=3 PIP_BIN=pip3 setup.sh -p
    
    # Remove ShellPAK
    $ ./setup.sh -u
    
    # Remove ShellPAK & go environment
    $ ./setup.sh -u -g
    
    # Change backup directory
    $ ./setup.sh -b /path/to/directory
    
    # List runtime options
    $ ./setup.sh -h

Previous versions of `setup.sh` provided options for installation
of Emacs packages.  This is no longer required as Emacs will now
automatically install necessary packages on startup.

# Bugs<a id="sec-4" name="sec-4"></a>

Please report any bugs or feature requests by [filing a ticket](https://chiselapp.com/user/cfuhrman/repository/shpak/reportlist) via
the ShellPAK repository website.

# Author<a id="sec-5" name="sec-5"></a>

Christopher M. Fuhrman
[cfuhrman@pobox.com](cfuhrman@pobox.com)

# Copyright<a id="sec-6" name="sec-6"></a>

Copyright (c) 2000-2018 Christopher M. Fuhrman
All rights reserved.

All files contained herein can be redistributed and/or modified
under the terms of the Simplified BSD License (also known as the
"2-Clause License" or "FreeBSD License".) unless otherwise noted.