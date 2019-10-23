#=============================================================================
# Copyright 2010-2011 Kitware, Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#=============================================================================

# Run clangformat and KWStyle pre-commit hooks.
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

do_clangformat=$(git config --bool hooks.clangformat) || do_clangformat=true

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
    echo "$KWStyle_overWriteRulesConf"
    "$KWStyle_path" -gcc -xml "$KWStyle_conf" -o "$KWStyle_overWriteRulesConf" "$1"
  fi

  if test $? -ne 0; then
    cp -- "$1"{,.kws}
    die "KWStyle check failed.

Line numbers in the errors shown refer to the file:
${1}.kws"
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

#-----------------------------------------------------------------------------
# clangformat.
check_for_clangformat() {
  clangformat_required_version=8.0
  system_tools="
    clang-format-$clangformat_required_version
    clang-format
"
  for tool in $system_tools; do
    if type -p "$tool" >/dev/null; then
      system_clang_format="$tool"
      break
    fi
  done
  clangformat_path=$(git config clangFormat.binary) ||
  clangformat_path=$(type -p "$system_clang_format" >/dev/null) ||
  die "clang-format executable was not found.

A clang-format binary will be downloaded and configured when ITK
is built with the BUILD_TESTING CMake configuration option enabled.

Alternatively, install clang-format version $clangformat_required_version or set the executable location with

  git config clangFormat.binary /path/to/clang-format
"
  if ! "$clangformat_path" --version | grep "clang-format version $clangformat_required_version" >/dev/null 2>/dev/null; then
    die "clang-format version $clangformat_required_version is required

Set the path the clang-format $clangformat_required_version executable with

  git config clangFormat.binary /path/to/clang-format

or disable the clang-format pre-commit hook with

  git config hooks.clangformat false
"
  fi
}

run_clang_format_check_attr()
{
    IN=$1
    OUT=$2
    ERR=$3
    "${clangformat_path}" -style=file "$IN" > "$OUT" 2> "$ERR"
    return $?
}

run_clangformat_on_file() {
  MERGED="$1"
  if run_style_on_file "$MERGED" "clangformat"; then
    ext="$$$(expr "$MERGED" : '.*\(\.[^/]*\)$')"
    BACKUP="./$MERGED.BACKUP.$ext"
    LOCAL="./$MERGED.STAGED.$ext"
    REMOTE="./$MERGED.CLANGFORMAT.$ext"
    NEW_MERGED="./$MERGED.NEW.$ext"
    ERROR_LOG="./$MERGED.$ext.log"
    OLD_MERGED="$MERGED"

    mv -- "$MERGED" "$BACKUP"
    # We temporarily change MERGED because the file might already be open, and
    # the text editor may complain.
    MERGED="$NEW_MERGED"
    cp -- "$BACKUP" "$LOCAL"

    run_clang_format_check_attr "$LOCAL" "$REMOTE" "$ERROR_LOG"
    clang_format_status=$?
    if [  $clang_format_status -ne 0 ]; then
      mv -- "$BACKUP" "$OLD_MERGED"

      if $merge_keep_temporaries; then
        rm -f -- "$LOCAL" "$REMOTE" "$BACKUP"
      fi
      die "error when running clang-format on $OLD_MERGED"
    fi
    cp -- "$REMOTE" "$MERGED"

    if test $(git hash-object -- "$LOCAL") != $(git hash-object -- "$REMOTE"); then
      if [ "$(uname)" == "Darwin" ]; then
        run_merge_tool "$merge_tool" "false" </dev/tty
      elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
        run_merge_tool "$merge_tool" "false" </dev/tty
      else
        run_merge_tool "$merge_tool" "false"
      fi
      if [ $? -ne 0 ]; then
        mv -- "$BACKUP" "$OLD_MERGED"

        if $merge_keep_temporaries; then
          rm -f -- "$LOCAL" "$REMOTE" "$BACKUP" "$NEW_MERGED"
        fi

        die "clang-format merge of $OLD_MERGED failed
        Error log: $ERROR_LOG"
      fi
    fi

    mv -- "$NEW_MERGED" "$OLD_MERGED"
    MERGED="$OLD_MERGED"

    if $merge_keep_backup; then
      mv -- "$BACKUP" "$MERGED.orig"
    else
      rm -- "$BACKUP"
    fi

    git add -- "$MERGED"
    rm -f -- "$LOCAL" "$REMOTE" "$BACKUP" "$ERROR_LOG" "$NEW_MERGED"

  fi # end if run clang-format on file

  if $do_KWStyle &&
    $have_KWStyle &&
    run_style_on_file "$MERGED" KWStyle
  then
    run_KWStyle_on_file "$MERGED"
  else
    return 0
  fi
}

run_clangformat() {
  $do_KWStyle && check_for_KWStyle
  if test $?; then
    have_KWStyle=true
  else
    have_KWStyle=false
  fi

  merge_tool=$(get_merge_tool "$merge_tool") || die "Merge tool not configured.

Set the merge tool with

  git config merge.tool <toolname>

For more information, see

  git help mergetool"
  merge_keep_backup="$(git config --bool mergetool.keepBackup || echo true)"
  merge_keep_temporaries="$(git config --bool mergetool.keepTemporaries || echo false)"
  git diff-index --cached --diff-filter=ACMR --name-only HEAD -- |
  while read MERGED; do
    run_clangformat_on_file "$MERGED" || return
  done # end for changed files
}

# Do not run during merge commits for now.
if test -f "$GIT_DIR/MERGE_HEAD"; then
  :
elif $do_clangformat; then
  # We use git-mergetool settings to review the clangformat changes.
  TOOL_MODE=merge
  . "$(git --exec-path)/git-mergetool--lib"
  # Redefine check_unchanged because we do not need to check if the merge was
  # successful.
  check_unchanged() {
    status=0
  }
  check_for_clangformat
  run_clangformat || exit 1
# do_clangformat will run KWStyle on the files incrementally so excessive
# clangformat merges do not have to occur.
elif $do_KWStyle; then
  if check_for_KWStyle; then
    run_KWStyle || exit 1
  fi
fi

# vim: set filetype=sh tabstop=8 softtabstop=8 shiftwidth=8 noexpandtab :
