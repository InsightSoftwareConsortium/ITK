#
# The ITK DoubleConversion is based on the Google double-conversion
# library. See https://code.google.com/p/double-conversion
#
# This is a very small library of functions in one library, so
# only the library source from the src subdirectory is included.
#

# PROCEDURE TO UPDATE DoubleConversion
#
# assuming that the current directory is ../ITK
# where ITK is your ITK source repository
#
#
# start from directory containing ITK source tree
#
# NOTE: THE FOLLOWING LINE NEEDS TO BE EDITED WHEN A NEW
# MERGE IS DONE
current_branch_hash=8e5d197038404faff408a33c7a58d29be67b209e

# go to ITK tree
cd ITK
#
# get on a branch for changes
git checkout -b MergeUpstream
mkdir tmp
cd tmp

#
# checkout google code
git clone https://code.google.com/p/double-conversion
cd double-conversion

# find out what the current head revision hash is:
set new_google_head_revision=$(git log | head -1 | awk '{ print $2 }')
# $new_google_head_revison will be the hash our updated code is based on.
# See above, about replacing current_branch_hash
#

# create current branch name
set branchname=GoogleMerge-$(date +%f)

#
# make a branch based on current branch hash
git branch ${branchname} ${current_branch_hash}
# go to that new branch
git checkout ${branchname}
#
# copy local ITK changes on top of src
cp ../../ITK/Modules/ThirdParty/DoubleConversion/src/double-conversion/* src

# commit locally
git add .
git commit -a -m "COMP: Add ITK amentment/additions"

# now try rebase
git rebase origin/master ${branchname}

# if rebase conflicts, fix conflict files, and do this
# git add .
# git rebase --continue

# no conflicts? copy back into ITK
cp src/* ../../ITK/Modules/ThirdParty/DoubleConversion/src/double-conversion

# build and test new code brought in from the google repo.

# when satisfied, EDIT THIS FILE to replace the hash from the google
# source repo that the new changes are branched from.

# when you're done, the temp repo from google can be removed.
cd ..
rm -fr double-conversion
