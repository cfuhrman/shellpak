
# Table of Contents

1.  [Description](#orgca81bf4)
    1.  [Requirements](#org2c768cc)
    2.  [Supported Platforms](#org43c250a)
2.  [Obtaining ShellPAK](#orgb072061)
    1.  [Alternate Repositories](#org822ffdf)
3.  [Installation](#org7a04f13)
    1.  [Other setup options](#orga848fba)
4.  [Bugs](#orga13e6aa)
5.  [Author](#org19bcca2)
6.  [Copyright](#org994c8f3)



<a id="orgca81bf4"></a>

# Description

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


<a id="org2c768cc"></a>

## Requirements

-   GNU Bash version 3.2 or greater
-   GNU Emacs 24.1 or greater (Emacs 24.4 or greater preferred)
-   GNU Make
-   rsync


<a id="org43c250a"></a>

## Supported Platforms

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


<a id="orgb072061"></a>

# Obtaining ShellPAK

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


<a id="org822ffdf"></a>

## Alternate Repositories

[ShellPAK](https://chiselapp.com/user/cfuhrman/repository/shpak/home) is also available for checkout from the following
locations:

-   **Git:** [GitHub](https://github.com/cfuhrman/shpak), [BitBucket](https://bitbucket.org/chrisfuhrman/shellpak)
-   **Fossil:** [FuhrWerks](https://fossil.fuhrwerks.com/shpak)
-   **Bazaar:** [Launchpad](https://code.launchpad.net/~cfuhrman/shpak/trunk)

Be aware that portions of the ShellPAK infrastructure relies on
Fossil.  Conversion to support the conventions of an alternative
revision control system is an exercise left to the user.


<a id="org7a04f13"></a>

# Installation

    # Install ShellPAK
    $ ./setup.sh

Note that during installation, ShellPAK will store dot-files (e.g.,
`bashrc`, `bash_profile`, etc) in `~/Backup/shell` (or whatever is
specified by the `-b` flag).  These files will be restored during
un-installation (via the `-u` flag)


<a id="orga848fba"></a>

## Other setup options

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


<a id="orga13e6aa"></a>

# Bugs

Please report any bugs or feature requests by [filing a ticket](https://chiselapp.com/user/cfuhrman/repository/shpak/reportlist) via
the ShellPAK repository website.


<a id="org19bcca2"></a>

# Author

Christopher M. Fuhrman
[cfuhrman@pobox.com](mailto:cfuhrman@pobox.com)


<a id="org994c8f3"></a>

# Copyright

Copyright (c) 2000-2018 Christopher M. Fuhrman
All rights reserved.

All files contained herein can be redistributed and/or modified
under the terms of the Simplified BSD License (also known as the
"2-Clause License" or "FreeBSD License".) unless otherwise noted.
