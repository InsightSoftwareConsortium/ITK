The vxl subdirectory contains a reduced distribution of the
vxl source tree with only vnl and supporting code.  It is not a
submodule; the actual content is part of our source tree and
changes can be made and committed directly.

We update from upstream using Git's "subtree" merge strategy.
A special branch contains commits of upstream vxl snapshots and
nothing else.  No Git ref points explicitly to the head of this
branch, but it is merged into our history.  The history has form:

 ...o----o....o....o....o  master
        /    /    /
       V----X----L  (branch not named, but merged)

where V, X, and L are a series of snapshots of the upstream source.
Each commit message records a script used to extract its exact
source from upstream so it is easy to reproduce and verify.

Update vxl from upstream as follows.  Create a local branch to
explicitly name the branch (commit L in above diagram):

 cd /path/to/top/of/src/ITK
 git branch vxl-upstream 03fff203           # (*)

The above example shows the real commit hash.  Use it literally.

Use a temporary directory to checkout the branch:

 cd /path/to/top/of/src/ITK
 mkdir vxl-tmp
 cd vxl-tmp
 git init
 git pull .. vxl-upstream
 rm -rf *

Now place the (reduced) vxl content in this directory by extracting
it from the upstream repository.  See instructions recorded in the
commit messages of the branch for details:

 cd /path/to/top/of/src/ITK/vxl-tmp
 git log

Write a script similar to those in the commit messages but updated
for the new upstream version to be extracted.  Run the script in
a completely separate directory of your choosing and move the content
left in the "-reduced" subdirectory to the current vxl-tmp directory
by hand. Then run the following commands to commit the new version.
Substitute the appropriate date and version number:

 cd /path/to/top/of/src/ITK/vxl-tmp
 git add -u
 git add .

 GIT_AUTHOR_NAME='VXL Maintainers' \
 GIT_AUTHOR_EMAIL='vxl-maintainers@lists.sourceforge.net' \
 GIT_AUTHOR_DATE='2011-04-28 07:00:14 -0400' \
 git commit -m 'vxl 1.15.0.20110428-r31940 (reduced)' &&
 echo "Change-Id: I$(git rev-parse HEAD)" &&
 git commit --amend

Edit the commit message to describe the procedure used to obtain the
content.  It should include a copy of the script constructed above to
obtain the upstream source.  Also add a blank line at the *bottom* of
the commit message followed by the Change-Id line printed out above.
(This is needed because the vxl-tmp directory has no local hooks to
add the Gerrit Change-Id automatically.)

Then push the changes back up to the main local repository:

 cd /path/to/top/of/src/ITK/vxl-tmp
 git push .. HEAD:vxl-upstream
 cd ..
 rm -rf vxl-tmp

Create a topic in the main repository on which to perform the update:

 cd /path/to/top/of/src/ITK
 git checkout -b update-vxl master

Now the local repository history looks like this:

 ...o----o....o....o....o  update-vxl
        /    /    /
       V----X----L---------U  vxl-upstream

where commit U is the one just created above using the vxl-tmp dir.
Merge the (local) vxl-upstream branch as a subtree:

 cd /path/to/top/of/src/ITK
 git merge -s recursive -X subtree=Modules/ThirdParty/VNL/src/vxl \
           vxl-upstream

If there are conflicts, resolve them and commit.  Now the local
repository history looks like this:

 ...o----o....o....o....o----M  update-vxl
        /    /    /         /
       V----X----L---------U  vxl-upstream

where commit M is the merge just created.  Build and test the tree.
Commit any additional changes needed to succeed.  If any fixup
commits were needed the local history now looks like:

 ...o----o....o....o....o----M----F  update-vxl
        /    /    /         /
       V----X----L---------U  vxl-upstream

where commit F contains the fixes.

Now run

 cd /path/to/top/of/src/ITK
 git rev-parse --short=8 vxl-upstream

to get the commit from which the vxl-upstream branch must be started
on the next update.  Edit the "git branch vxl-upstream" line above (*)
to record it, and commit this file.  Then delete the local snapshot
branch:

 cd /path/to/top/of/src/ITK
 git branch -d vxl-upstream

Now the local history looks like this:

 ...o----o....o....o....o----M----F----I  update-vxl
        /    /    /         /
       V----X----L---------U  (branch not named, but merged)

where commit 'I' is the update to these instructions just committed.

Finally, "git gerrit-push" the update-vxl branch like any other.
