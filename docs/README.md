# Description<a id="sec-2" name="sec-2"></a>

[ShellPAK](http://chiselapp.com/user/cfuhrman/repository/shpak) allows for the automatic installation and configuration of
my personal [GNU Bash](http://www.gnu.org/software/bash/) shell environment as well as the configuration
of the following programs:

-   [Git](http://git-scm.com)
-   [GNU Emacs](http://www.gnu.org/software/emacs/)
-   [GNU Screen](http://www.gnu.org/software/screen/)
-   [GNU Indent](https://www.gnu.org/software/indent/)
-   [Perl::Tidy](http://search.cpan.org/~shancock/Perl-Tidy-20140711/lib/Perl/Tidy.pod)
-   [tmux](http://tmux.sourceforge.net)

## Requirements<a id="sec-2-1" name="sec-2-1"></a>

-   GNU Bash version 3.2 or greater
-   GNU Emacs 23.1 or greater (Emacs >24.1 preferred)
-   rsync

## Supported Platforms<a id="sec-2-2" name="sec-2-2"></a>

The following operating systems are known to be supported:

-   Mac OS X 10.9 (Darwin)
-   NetBSD 6.1.x
-   OpenBSD 5.4
-   CentOS 6.5
-   Debian 7 (Wheezy) and derivatives
-   SunOS solaris 5.10
-   AIX 1 7

In addition, ShellPAK has been known to run on OpenSuSE, SuSE
Enterprise Linux, Ubuntu, FreeBSD, and Cygwin.

# Obtaining ShellPAK<a id="sec-3" name="sec-3"></a>

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

## Alternate Repositories<a id="sec-3-1" name="sec-3-1"></a>

ShellPAK is also available for checkout from the following
locations:

-   **Git:** [GitHub](https://github.com/cfuhrman/shpak), [BitBucket](https://bitbucket.org/chrisfuhrman/shellpak)
-   **Fossil:** [FuhrWerks](https://www.fuhrwerks.com/fossil/shpak)
-   **Bazaar:** [Launchpad](https://code.launchpad.net/~cfuhrman/shpak/trunk)

Be aware that portions of the ShellPAK infrastructure relies on
Fossil.  Conversion to support the conventions of an alternative
revision control system is an exercise left to the user.

# Installation<a id="sec-4" name="sec-4"></a>

    # Install ShellPAK
    $ ./setup.sh

Note that during installation, ShellPAK will store dot-files (e.g.,
`bashrc`, `bash_profile`, etc) in `~/Backup/shell` (or whatever is
specified by the `-b` flag).  These files will be restored during
un-installation (via the `-u` flag)

## Other setup options<a id="sec-4-1" name="sec-4-1"></a>

    # Perform a dry-run
    $ ./setup.sh -r
    
    # Install into another directory
    $ ./setup.sh -d /path/to/directory
    
    # Do not automatically link files during installation
    $ ./setup.sh -n
    
    # Remove ShellPAK
    $ ./setup.sh -u
    
    # Change backup directory
    $ ./setup.sh -b /path/to/directory
    
    # List runtime options
    $ ./setup.sh -h

# Bugs<a id="sec-5" name="sec-5"></a>

Please report any bugs or feature requests by [filing a ticket](https://chiselapp.com/user/cfuhrman/repository/shpak/reportlist) via
the ShellPAK repository website.

# Author<a id="sec-6" name="sec-6"></a>

Christopher M. Fuhrman &lt;`cfuhrman atta pobox dotta com`&gt;

# Copyright<a id="sec-7" name="sec-7"></a>

Copyright (c) 2000-2014 Christopher M. Fuhrman<br>
All rights reserved.

All files contained herein can be redistributed and/or modified
under the terms of the Simplified BSD License (also known as the
"2-Clause License" or "FreeBSD License".) unless otherwise noted.
