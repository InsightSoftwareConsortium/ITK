#!/usr/bin/env bash
#==========================================================================
#
#   Copyright Insight Software Consortium
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          http://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
#==========================================================================*/


# This script makes optional suggestions for working with git.

egrep-q() {
  egrep "$@" >/dev/null 2>/dev/null
}

if test "$(git config color.ui)" != "auto"; then
  cat << EOF

You may want to enable color output from Git commands with

  git config --global color.ui auto

EOF
fi

if ! bash -i -c 'echo $PS1' | egrep-q '__git_ps1'; then
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
Style Guidelines (See Documentation/Style.pdf). When committing code, it can be
passed through uncrustify (https://github.com/uncrustify/uncrustify/).
Uncrustify version should be dated 2016-08-26 or newer,
SHA: 16a96b489f394b5c60cd72a4f5e0e3d230e4da71
However, this feature is disabled by default. To enable this feature,

  git config --bool hooks.uncrustify true

EOF
fi

echo "Done."
