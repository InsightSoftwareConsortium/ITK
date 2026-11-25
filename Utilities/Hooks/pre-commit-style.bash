#!/usr/bin/env bash
#=============================================================================
# Copyright 2010-2011 Kitware, Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     https://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#=============================================================================

# Run KWStyle pre-commit hooks.
#
# 'git config' is used to enable the hooks and set their configuration files.
# The repository .gitattributes must also enable the hooks on the targeted
# files.

die() {
          echo 'pre-commit hook failure' 1>&2
          echo '-----------------------' 1>&2
          echo '' 1>&2
          echo "$@" 1>&2
          exit 1
}

do_KWStyle=$(git config --bool hooks.KWStyle) || do_KWStyle=true

#-----------------------------------------------------------------------------
# Check if we want to run the style on a given file.  Uses git attributes.  If
# the hook.style attribute is set, then all styles are executed.  If the
# hook.style attribute is set to a value, only the values given are executed.
# Also, do not run the style check if there are unstaged changes in the file.
# The first positional parameter is the file to check.
# The second positional parameter is the style to check.
# Returns 0 for execute, 1 for don't execute.
run_style_on_file() {
  # Do not run on submodule changes.
  if git diff-index --cached HEAD -- "$1" | grep -q '^:...... 160000'; then
    return 1
  fi
  style=$(git check-attr hooks.style -- "$1" |
      sed 's/^[^:]*: hooks.style: //')
  has_style_attr=1
  case "$style" in
    'unset')        has_style_attr=1 ;;
    'set')          has_style_attr=0 ;;
    'unspecified')  has_style_attr=1 ;;
    *)              echo ",$style," | grep -iq ",$2," && has_style_attr=0 ;;
  esac
  if ! git diff-files --quiet -- "$1" && test $has_style_attr -eq 0; then
    # A way to always allow skipping.
    skip_unstaged=$(git config --bool hooks.styleSkipUnstaged) ||
    skip_unstaged=false
    file_sha=$(git diff-index --cached --abbrev=7 HEAD -- "$1" | \
    awk '{print substr($3,1,9) substr($4,1,7)}')
    if file_skip_unstaged=$(git config "hooks.$1.styleSkipUnstaged"); then
      if test ",$file_skip_unstaged," = ",$file_sha," -o \
        ",$file_skip_unstaged," = ",true,"; then
        skip_unstaged=true
      fi
    fi

    if $skip_unstaged; then
      echo "The file '$1' contains unstaged stages.  Skipping style \
check '$2'."
    else
      die "Style check '$2' cannot run on '$1' with unstaged stages.

Allow skipping the style check for this commit with

  git config \"hooks.$1.styleSkipUnstaged\" $file_sha"
    fi
    return 1
  fi
  return $has_style_attr
}

#-----------------------------------------------------------------------------
# KWStyle.
check_for_KWStyle() {
  KWStyle_path=$(git config hooks.KWStyle.path) ||
  KWStyle_path=$(which KWStyle)
  if [ $? != 0 ] ; then
    echo "KWStyle executable was not found.

  No style verification will be performed with KWStyle!

A KWStyle executable will be built and configured when ITK
is built with the BUILD_TESTING CMake configuration option enabled.
Alternatively, set the KWStyle executable location with

  git config hooks.KWStyle.path /path/to/KWStyle

See https://kitware.github.io/KWStyle/
" >&2
    return 1
  fi
  KWStyle_conf=$(git config hooks.KWStyle.conf)
  if ! test -f "$KWStyle_conf"; then
    die "The file '$KWStyle_conf' does not exist.

Please run

  git config hooks.KWStyle.conf path/to/KWStyle.conf.xml"
  fi
  KWStyle_overWriteRulesConf=$(git config hooks.KWStyle.overwriteRulesConf)
  if test $? -eq 0 && ! test -f "$KWStyle_overWriteRulesConf"; then
    die "The hooks.KWStyle.overwriteRulesConf file '$KWStyle_overWriteRulesConf' does not exist."
  fi
}

run_KWStyle_on_file() {
  local_KWStyle_overWriteRulesConf="`pwd`/${1%/*}/../ITKKWStyleOverwrite.txt"
  if test -f "$local_KWStyle_overWriteRulesConf"; then
    "$KWStyle_path" -gcc -xml "$KWStyle_conf" -o "$local_KWStyle_overWriteRulesConf" "$1"
  elif test -z "$KWStyle_overWriteRulesConf"; then
    "$KWStyle_path" -gcc -xml "$KWStyle_conf" "$1"
  else
    "$KWStyle_path" -gcc -xml "$KWStyle_conf" -o "$KWStyle_overWriteRulesConf" "$1"
  fi

  if test $? -ne 0; then
    # https://stackoverflow.com/a/9107028
    this_script="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/$(basename "${BASH_SOURCE[0]}")"
    cp -- "$1"{,.kws}
    die "KWStyle check failed.

Line numbers in the errors shown refer to the file:
${1}.kws

Run the following to test current failure in isolation:
${this_script} $1
"
  fi
  return 0
}

run_KWStyle() {
  git diff-index --cached --diff-filter=ACMR --name-only HEAD -- |
  while read f; do
    if run_style_on_file "$f" KWStyle; then
      run_KWStyle_on_file "$f"
    fi || return
  done
}

# If first arugment exists and is a file, then run KWStyle on that file
if [ ! -z "$1" ] && [ -f "$1" ]; then
  echo "Running KWStyle in stand-alone mode for file $1"
  if check_for_KWStyle; then
    if run_style_on_file "$1" KWStyle; then
      run_KWStyle_on_file "$1"
    fi || return
  fi
else
  # Do not run during merge commits for now.
  if test -f "$GIT_DIR/MERGE_HEAD"; then
  :
  elif $do_KWStyle; then
    if check_for_KWStyle; then
      run_KWStyle || exit 1
    fi
  fi
fi
# vim: set filetype=sh tabstop=8 softtabstop=8 shiftwidth=8 noexpandtab :
