# This file should not have its executable bit set.

# This script should not be run directly. Instead, create an executable file
# "UpdateFromUpstream.sh", which defines the following shell variables:
#
#   - thirdparty_module_name :
#       The name of the ThirdParty module to be updated. This must match the
#       name of the ITK ThirdParty module's base directory (which contains
#       "itk-module.cmake").
#       i.e. "Modules/ThirdParty/<thirdparty_module_name>/itk-module.cmake"
#   - upstream_git_url :
#       The full URL (including the scheme) of the upstream Git repository
#       that the ThirdParty module tracks.
#   - upstream_git_branch :
#      The upstream Git branch name that the ThirdParty module tracks.
#   - snapshot_author_name :
#      The author name to which the upstream files should be attributed,
#      in ITK's Git repository.
#   - snapshot_author_email :
#      The author email to which the upstream files should be attributed.
#      in ITK's Git repository.
#   - snapshot_redact_cmd :
#      A string, containing a command to be eval'd within the CWD of the fresh
#      upstream snapshot, in order to prepare it for being committed to ITK's
#      repository. The string may be empty or contain multiple commands chained
#      with "&&", etc. The command typically removes extraneous files
#      or directories that shouldn't be committed to ITK for size, simplicity,
#      or licensing reasons.
#   - snapshot_relative_path :
#      The path, relative to the ThirdParty module's base directory, that
#      the upstream snapshot's file should be merged into.
#      e.g. "src/mylib/"
#   - snapshot_paths :
#      An optional list of file patterns to be included in the snapshot. E.g.
#        snapshot_paths='
#         dir1
#         dir2/*.c
#         file1.txt
#        '
#
# The script, "UpdateFromUpstream.sh", must be located in the associated
# ThirdParty module's base directory.
#

die()
{
  echo "Error: $@" 1>&2
  exit 1
}

update_from_upstream()
{
## Set up paths ##
local module_path=$( cd "$( dirname "$0" )" && pwd )
local toplevel_path=$(cd "$module_path" && git rev-parse --show-toplevel)
if [[ $? -ne 0 ]]; then
  die "Could not find the top-level of a Git repository in \"$module_path\""
fi
cd "$toplevel_path"


## Validate ##
local required_commands=( git grep sed egrep tar dirname basename tr )
for required_command in ${required_commands[@]}; do
  type -p $required_command >/dev/null 2>&1
  if [[ $? -ne 0 ]]; then
    die "Command \"$required_command\" not found"
  fi
done

local input_variables=( thirdparty_module_name snapshot_relative_path \
                        upstream_git_url upstream_git_branch \
                        snapshot_author_name snapshot_author_email \
                        snapshot_relative_path )
for input_variable in ${input_variables[@]}; do
  if [[ -z $(eval echo "\$$input_variable") ]]; then
    die "\$$input_variable must be set"
  fi
done

local module_path_basename=$(basename "$module_path")
if [[ "$thirdparty_module_name" != "$module_path_basename" ]]; then
  die "The script \"$0\" must be in the base directory for the \"$thirdparty_module_name\" module"
fi


## Old snapshot commit ##
local regex_date='20[0-9][0-9]-[0-9][0-9]-[0-9][0-9]'
local snapshot_old_regex="$thirdparty_module_name $regex_date ([0-9a-f]*)"
local snapshot_old_sha=$(git rev-list --author="$snapshot_author_email" --grep="$snapshot_old_regex" -n 1 HEAD)
if [[ -z "$snapshot_old_sha" ]]; then
  read -ep "Create a new snapshot branch? [N/y]: " create_new
  if [[ "$create_new" != "y" && "$create_new" != "Y" ]]; then
    die "Could not find previous snapshot."
  fi
fi

## New upstream commit ##
git fetch --quiet "$upstream_git_url" "$upstream_git_branch" 2> >(sed '/warning: no common commits/d' >&2)
if [[ $? -ne 0 ]]; then
  die "Could not \"git fetch\" from $upstream_git_url:$upstream_git_branch"
fi
local upstream_new_sha=$(git rev-parse FETCH_HEAD)
local upstream_new_sha_short=$(git rev-parse --short=8 $upstream_new_sha)
local upstream_new_datetime=$(git rev-list $upstream_new_sha --format='%ci' -n 1 | grep "^$regex_date")
local upstream_new_date=$(echo "$upstream_new_datetime" | grep -o "$regex_date")


## Old upstream commit ##
if [[ -n "$snapshot_old_sha" ]]; then
  local upstream_old_sha_short=$(
    git cat-file commit $snapshot_old_sha |
    sed -n '/'"$snapshot_old_regex"'/ {s/.*(//;s/)//;p;}' |
    egrep '^[0-9a-f]+$'
  )
  local upstream_old_sha=$(git rev-parse --verify -q "$upstream_old_sha_short")
fi
if [[ "$upstream_old_sha" == "$upstream_new_sha" ]]; then
  echo 'Upstream has no updates'
  exit 0
fi


## New shapshot tree ##
local snapshot_branch_name=`echo "upstream-${thirdparty_module_name}" | tr '[:upper:]' '[:lower:]'` # make string lowercase
local snapshot_temp_dir="$snapshot_branch_name"
local snapshot_temp_path="$toplevel_path/$snapshot_temp_dir/"
local snapshot_temp_index="$toplevel_path/$snapshot_branch_name.index"
rm -rf "$snapshot_temp_path" "$snapshot_temp_index"
cd "$toplevel_path" && git archive --prefix="$snapshot_temp_dir/" $upstream_new_sha -- $snapshot_paths | tar x
( cd "$snapshot_temp_path" && eval "$snapshot_redact_cmd" )
if [[ $? -ne 0 ]]; then
  rm -rf "$snapshot_temp_path" "$snapshot_temp_index"
  die 'Could not eval $snapshot_redact_cmd in upstream snapshot'
fi
local snapshot_new_tree=$(
  GIT_WORK_TREE="$snapshot_temp_path" &&
  GIT_INDEX_FILE="$snapshot_temp_index" &&
  export GIT_WORK_TREE GIT_INDEX_FILE &&
  git add --all &&
  git write-tree
)
rm -rf "$snapshot_temp_path" "$snapshot_temp_index"


## New shapshot commit ##
if [[ -z "$snapshot_old_sha" ]]; then
  local snapshot_new_shortlog="Initial import of $upstream_new_sha"
  local snapshot_new_change_id=$(git commit-tree $snapshot_new_tree </dev/null)
  local snapshot_log_command=""
else
  local snapshot_new_change_id=$(git commit-tree $snapshot_new_tree -p $snapshot_old_sha </dev/null)
  if [[ $github_compare == true ]]; then
    local snapshot_log_command=""
    local snapshot_new_shortlog="${upstream_git_url%.*}/compare/$(git rev-list $upstream_old_sha -n 1)...$upstream_new_sha"
  else
    local snapshot_new_shortlog=$(git shortlog --perl-regexp --author='^((?!Kitware Robot).*)$' --no-merges --abbrev=8 --format='%h %s' $upstream_old_sha..$upstream_new_sha)
    local snapshot_log_command="\$ git shortlog --perl-regexp --author='^((?!Kitware Robot).*)$' --no-merges --abbrev=8 --format='%h %s' $upstream_old_sha_short..$upstream_new_sha_short"
  fi
fi

local snapshot_new_commit_msg="$thirdparty_module_name $upstream_new_date ($upstream_new_sha_short)

Run the UpdateFromUpstream.sh script to extract upstream $thirdparty_module_name
using the following shell commands.

\$ git archive --prefix=$snapshot_branch_name/ $upstream_new_sha_short -- $snapshot_paths | tar x
$snapshot_log_command

$snapshot_new_shortlog

Change-Id: I$snapshot_new_change_id"
if [[ -z "$snapshot_old_sha" ]]; then
  local snapshot_new_sha=$(
    echo "$snapshot_new_commit_msg" |
    GIT_AUTHOR_NAME="$snapshot_author_name" \
    GIT_AUTHOR_EMAIL="$snapshot_author_email" \
    GIT_AUTHOR_DATE="$upstream_new_datetime" \
    git commit-tree $snapshot_new_tree
  )
else
  local snapshot_new_sha=$(
    echo "$snapshot_new_commit_msg" |
    GIT_AUTHOR_NAME="$snapshot_author_name" \
    GIT_AUTHOR_EMAIL="$snapshot_author_email" \
    GIT_AUTHOR_DATE="$upstream_new_datetime" \
    git commit-tree $snapshot_new_tree -p $snapshot_old_sha
  )
fi


## New shapshot branch ##
git update-ref refs/heads/$snapshot_branch_name $snapshot_new_sha
local module_relative_path=${module_path#"$toplevel_path/"}
echo "Created upstream snapshot branch '$snapshot_branch_name'."
if [[ -z "$snapshot_old_sha" ]]; then
    echo "Perform initial merge with commands:

    cd \"$toplevel_path\" &&
    git merge -s ours --no-commit $snapshot_branch_name &&
    git read-tree -u --prefix=$module_relative_path/$snapshot_relative_path/ $snapshot_branch_name &&
    git commit
"
else
    cd "$toplevel_path" &&
    git merge -X subtree=$module_relative_path/$snapshot_relative_path $snapshot_branch_name
    git branch -d "$snapshot_branch_name"
fi

}
