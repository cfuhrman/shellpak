#!/bin/bash
# ====================================================================
#
# bash_logout
# 
# Copyright (c) 2000 Christopher M. Fuhrman
# All rights reserved.
# 
# This program is free software; you can redistribute it and/or
# modify it under the terms of the Simplified BSD License (also
# known as the "2-Clause License" or "FreeBSD License".)
#
# Created Sat Nov 11 16:07:11 2000 UTC
#
# ====================================================================

#
#   Any programs that are run on shell exit/logout
#

# Display a friendly message on logout
echo -n 'Logged out at '
date
echo 'Have a nice day :)'

# bash_logout ends here
