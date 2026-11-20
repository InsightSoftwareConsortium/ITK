#!/usr/bin/env bash
#==========================================================================
#
#   Copyright NumFOCUS
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          https://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
#==========================================================================*/

#% NAME
#%     SourceTarball.bash - create ITK source and data release archives
#%
#% SYNOPSIS
#%     SourceTarball.bash [--tgz] [--txz] [--zip] [--verbose]
#%                        [-v <version>] [--] [<tag> | <commit>]
#%     SourceTarball.bash -h | --help
#%
#% DESCRIPTION
#%     Write the release archives for one ITK commit to the current
#%     directory. For each format requested it writes:
#%
#%     - InsightToolkit-<version>.<ext>: the source tree, without the
#%       .ExternalData directory.
#%     - InsightData-<version>.<ext>: the .ExternalData directory of the
#%       commit plus every test data object named by a .cid content link,
#%       stored as .ExternalData/CID/<cid>.
#%
#%     Both archives unpack into InsightToolkit-<version>/, so the data
#%     archive unpacks over the source archive. Each archive is written to
#%     a temporary name and renamed when complete. The exit status is
#%     nonzero if a data object cannot be found or downloaded, or if
#%     compressing or renaming an archive fails; it is 1 for an argument
#%     that is not understood or a commit that is not valid. A git archive
#%     failure inside the compression pipe is not detected.
#%
#% OPTIONS
#%     --tgz, --txz, --zip
#%         Archive format: .tar.gz, .tar.xz, or .zip. Give several to write
#%         several. The default is --tgz. The .zip archive is made with
#%         core.autocrlf=true, the tar archives with core.autocrlf=false.
#%     --verbose
#%         Pass -v to git archive.
#%     -v <version>
#%         Version used in the archive names. Without it, the version is
#%         git describe of the commit with its leading v removed. The
#%         script stops if that description does not start with v, but
#%         continues with an empty version if git describe fails.
#%     <tag> | <commit>
#%         The commit to archive. The default is HEAD.
#%     --
#%         End of options; no argument may follow it.
#%     -h, --help
#%         Print this help and exit.
#%
#% IMPLEMENTATION
#%     Run inside the ITK git repository; archives are written to the
#%     current directory. Needs git, gzip, xz, and curl. Each data object is
#%     taken from .ExternalData/CID/ in the current directory or in the
#%     home directory; a missing object is downloaded from an IPFS gateway
#%     that must be running at 127.0.0.1 (spelled 127.0.01 in the code), port
#%     8080, and saved under
#%     .ExternalData/CID/ in the current directory. Data objects are added
#%     to the git object store. A temporary git index file, tmp-<pid>-index,
#%     is created in the current directory and removed on exit.
#%
#% EXAMPLES
#%     ./Utilities/Maintenance/SourceTarball.bash --tgz --zip v6.0.0
#%         Write the .tar.gz and .zip archives for tag v6.0.0.
#%
#%     ./Utilities/Maintenance/SourceTarball.bash -v 6.0.0 <commit>
#%         Write the .tar.gz archives for a commit not yet tagged.
#%
#% SEE ALSO
#%     Documentation/docs/contributing/script_documentation.md:
#%         How these help sections are written.
#%     Documentation/Maintenance/Release.md:
#%         The release process that uses these archives.
#%     Documentation/docs/contributing/data.md:
#%         How ITK test data are stored.

usage() {
  sed -n 's/^#% \{0,1\}//p' "${BASH_SOURCE[0]}"
}

info() {
  echo "$@" 1>&2
}

die() {
  echo "$@" 1>&2; exit 1
}

return_pipe_status() {
  echo ${PIPESTATUS[@]} |grep -q -v "[1-9]"
}

find_data_objects() {
  git ls-tree --full-tree -r "$1" |
  egrep '\.(cid)$' |
  while read mode type obj path; do
    case "$path" in
      *.cid)  echo CID/$(git cat-file blob $obj) ;;
      *)      die "Unknown ExternalData content link: $path" ;;
    esac
  done | sort | uniq
  return_pipe_status
}

download_object() {
  algo="$1" ; hash="$2" ; path="$3"
  mkdir -p $(dirname "$path") &&
  if curl -L "http://127.0.01:8080/ipfs/$hash" -o "$path.tmp$$" 1>&2; then
    mv "$path.tmp$$" "$path"
  else
    rm -f "$path.tmp$$"
    false
  fi
}

index_data_objects() {
  # Input lines have format <algo>/<hash>
  while IFS=/ read algo hash; do
    # Final path in source tarball
    path=".ExternalData/$algo/$hash"
    # Find the object file on disk
    if test -f "$path"; then
      file="$path" # available in place
    elif test -f ~/"$path" ; then
      file=~/"$path" # available in home dir
    else
      download_object "$algo" "$hash" "$path" &&
      file="$path"
    fi &&
    obj=$(git hash-object -t blob -w "$file") &&
    echo "100644 blob $obj	$path" ||
    return
  done |
  git update-index --index-info
  return_pipe_status
}

load_data_objects() {
  find_data_objects "$@" |
  index_data_objects
  return_pipe_status
}

load_data_files() {
  git ls-tree -r "$1" -- '.ExternalData' |
  git update-index --index-info
  return_pipe_status
}

git_archive_tgz() {
  out="$2.tar.gz" && tmp="$out.tmp$$" &&
  if test -n "$3"; then prefix="$3"; else prefix="$2"; fi &&
  git -c core.autocrlf=false archive $verbose --format=tar --prefix=$prefix/ $1 |
  gzip -9 > "$tmp" &&
  mv "$tmp" "$out" &&
  info "Wrote $out"
}

git_archive_txz() {
  out="$2.tar.xz" && tmp="$out.tmp$$" &&
  if test -n "$3"; then prefix="$3"; else prefix="$2"; fi &&
  git -c core.autocrlf=false archive $verbose --format=tar --prefix=$prefix/ $1 |
  xz -9 > "$tmp" &&
  mv "$tmp" "$out" &&
  info "Wrote $out"
}

git_archive_zip() {
  out="$2.zip" && tmp="$out.tmp$$" &&
  if test -n "$3"; then prefix="$3"; else prefix="$2"; fi &&
  git -c core.autocrlf=true archive $verbose --format=zip --prefix=$prefix/ $1 > "$tmp" &&
  mv "$tmp" "$out" &&
  info "Wrote $out"
}

#-----------------------------------------------------------------------------

formats=
commit=
version=
verbose=

# Parse command line options.
while test $# != 0; do
  case "$1" in
    --tgz) formats="$formats tgz" ;;
    --txz) formats="$formats txz" ;;
    --zip) formats="$formats zip" ;;
    --verbose) verbose=-v ;;
    --) shift; break ;;
    -v) shift; version="$1" ;;
    -h | --help)
      usage
      exit 0
      ;;
    -*) usage 1>&2; exit 1 ;;
    *) test -z "$commit" && commit="$1" || { usage 1>&2; exit 1; } ;;
  esac
  shift
done
test $# = 0 || { usage 1>&2; exit 1; }
test -n "$commit" || commit=HEAD
test -n "$formats" || formats=tgz

if ! git rev-parse --verify -q "$commit" >/dev/null ; then
  die "'$commit' is not a valid commit"
fi
if test -z "$version"; then
  desc=$(git describe $commit) &&
  if test "${desc:0:1}" != "v"; then
    die "'git describe $commit' is '$desc'; use -v <version>"
  fi &&
  version=${desc#v} &&
  echo "$commit is version $version"
fi

# Create temporary git index to construct source tree
export GIT_INDEX_FILE="$(pwd)/tmp-$$-index" &&
trap "rm -f '$GIT_INDEX_FILE'" EXIT &&

result=0 &&

info "Loading source tree from $commit..." &&
rm -f "$GIT_INDEX_FILE" &&
git read-tree -m -i $commit &&
git rm -rf -q --cached '.ExternalData' &&
tree=$(git write-tree) &&

info "Generating source archive(s)..." &&
for fmt in $formats; do
  git_archive_$fmt $tree "InsightToolkit-$version" || result=1
done &&

info "Loading data for $commit..." &&
rm -f "$GIT_INDEX_FILE" &&
load_data_objects $commit &&
load_data_files $commit &&
tree=$(git write-tree) &&

info "Generating data archive(s)..." &&
for fmt in $formats; do
  git_archive_$fmt $tree "InsightData-$version" "InsightToolkit-$version" || result=1
done &&

exit $result
