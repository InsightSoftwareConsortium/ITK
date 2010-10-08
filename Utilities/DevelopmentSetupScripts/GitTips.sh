#!/usr/bin/env bash
#=============================================================================
# Copyright (c) 2010 Insight Software Consortium. All rights reserved.
# See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.
#
# This software is distributed WITHOUT ANY WARRANTY; without even
# the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
# PURPOSE.  See the above copyright notices for more information.
#=============================================================================


# This script makes optional suggestions for working with git.


if test "$(git config color.ui)" != "auto"; then
  cat << EOF

You may want to enable color output from Git commands with

  git config --global color.ui auto

EOF
fi

if ! bash -i -c 'echo $PS1' | grep -q '__git_ps1'; then
  cat << EOF

A dynamic, informative Git shell prompt can be obtained by sourcing the git
bash-completion script in your ~/.bashrc.  Set the PS1 environmental variable as
suggested in the comments at the top of the bash-completion script.  You may
need to install the bash-completion package from your distribution to obtain the
script.

EOF
fi

if ! git config merge.tool >/dev/null; then
  cat << EOF

A merge tool can be configured with

  git config merge.tool <toolname>

For more information, see

  git help mergetool

EOF
fi

if ! git config hooks.uncrustify >/dev/null; then
  cat << EOF

ITK comes with a pre-commit hook to help committed code to conform to the ITK
Style Guidelines (See Documentation/Style.pdf).  When committing code, it can be
passed through uncrustify (http://uncrustify.sourceforge.net).  However, this
feature is disabled by default.  To enable this feature,

  git config --bool hooks.uncrustify true

EOF
fi

echo "Done."
