#!/bin/bash
#
# UpdatepygccxmlFromUpstream.sh
#
# See ITK/Modules/ThirdParty/VNL/src/README-ITK.txt
# for more commentary on how this update process works.
#
echo "-------------------------------------------------------"
echo This script will update source code for the pygccxml
echo library from github.com/gccxml/pygccxml
echo "-------------------------------------------------------"
if [ ! -f Modules/ThirdParty/pygccxml/src/UpdatepygccxmlFromUpstream.sh ]
then
    echo The current working directory $(pwd) is not the top level
    echo of the ITK source code tree. Please change to the
    echo ITK source directory and re-run this script
    exit 1
fi

# Update the git tag for the version you are merging
git_url="https://github.com/gccxml/pygccxml"
git_tag="v1.9.0"
upstream_sha="c1b521e79bb897ec10853cedc589fa1bc66dc105"

#
# Once the merge has been done
# EDIT THIS SCRIPT to change the hash tag at which to begin the
# next update...
#
# This merge was done on: May 29, 2017
git branch pygccxml-upstream c67b33aabe9b9edfd1b9ef82f82bdb8bc0642a98

#
# Make a temp directory to handle the import of the upstream source
mkdir pygccxml-Tmp
cd pygccxml-Tmp
#
# base a temporary git repo off of the parent ITK directory
git init
#
# pull in the upstream branch
git pull .. pygccxml-upstream
#
# empty out all existing source
rm -rf *
# clone into the pygccxml-upstream branch
git clone ${git_url} tmp-pygccxml-upstream
#
# recover the upstream commit date.
cd tmp-pygccxml-upstream
git checkout ${upstream_sha}
upstream_date="$(git log -n 1 --format='%cd')"
cd ..

# Copy only the source code, which is located in the pygccxml subfolder
cp -r tmp-pygccxml-upstream/pygccxml/* .
# get rid of pygccxml clone
rm -fr tmp-pygccxml-upstream
# add upstream files in Git
git add --all

# get a Change-Id to keep track of the change in gerrit
tree=$(git write-tree)
change_id=$(git commit-tree $tree -p HEAD </dev/null)

# commit new source
GIT_AUTHOR_NAME='GCC-XML Upstream' \
GIT_AUTHOR_EMAIL='pygccxml@gccxml.org' \
GIT_AUTHOR_DATE="${upstream_date}" \
git commit -q -m "ENH: pygccxml ${git_tag} (reduced)

This corresponds to commit hash
  ${upstream_sha}
from upstream repository
  ${git_url}

Change-Id: I${change_id}"

#
# push to the pygccxml-upstream branch in the
# ITK tree
git push .. HEAD:pygccxml-upstream
cd ..
#
# get rid of temporary repository
rm -fr pygccxml-Tmp

#
# checkout a new update branch off master
git checkout -b pygccxml-update master
#
# use subtree merge to bring in upstream changes
git merge -s recursive -X subtree=Modules/ThirdParty/pygccxml/src/pygccxml pygccxml-upstream
echo "---------------------------------"
echo NOW Fix all conflicts and test new code.
echo "---------------------------------"
echo Commit the merged/fixed/verified code and run this command
echo git rev-parse pygccxml-upstream
echo "---------------------------------"
echo to get the commit hash from which the pygccxml-upstream
echo branch must be started on the next update.
echo "---------------------------------"
echo edit the line \"git branch pygccxml-upstream\" above.
echo Once you have commited this change to the UpdatepygccxmlFromUpstream.sh script,
echo use \"git gerrit-push\" to push this new update branch back to itk.org.
