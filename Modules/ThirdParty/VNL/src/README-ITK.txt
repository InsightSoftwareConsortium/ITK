The vxl subdirectory contains a reduced distribution of the
vxl source tree with only vnl and supporting code.  It is not a
submodule; the actual content is part of our source tree and
changes can be made and committed directly.

We update from upstream using Git's "subtree" merge strategy.
A special branch contains commits of upstream vxl snapshots and
nothing else.  No Git ref points explicitly to the head of this
branch, but it is merged into our history.

Update vxl from upstream as follows.  Create a local branch to
explicitly reference the upstream snapshot branch head:

 git branch vxl-upstream 9c5b855f

Use a temporary directory to checkout the branch:

 mkdir vxl-tmp
 cd vxl-tmp
 git init
 git pull .. vxl-upstream
 rm -rf *

Now place the (reduced) vxl content in this directory.  See
instructions shown by

 git log 9c5b855f

for help extracting the content from the upstream svn repo.  Then run
the following commands to commit the new version.  Substitute the
appropriate date and version number:

 git add -u
 git add .

 GIT_AUTHOR_NAME='VXL Maintainers' \
 GIT_AUTHOR_EMAIL='vxl-maintainers@lists.sourceforge.net' \
 GIT_AUTHOR_DATE='2011-04-28 07:00:14 -0400' \
 git commit -m 'vxl 1.15.0.20110428-r31940 (reduced)' &&
 git commit --amend

Edit the commit message to describe the procedure used to obtain the
content.  Then push the changes back up to the main local repository:

 git push .. HEAD:vxl-upstream
 cd ..
 rm -rf vxl-tmp

Create a topic in the main repository on which to perform the update:

 git checkout -b update-vxl master

Merge the vxl-upstream branch as a subtree:

 git merge -s recursive -X subtree=Modules/ThirdParty/VNL/src/vxl \
           vxl-upstream

If there are conflicts, resolve them and commit.  Build and test the
tree.  Commit any additional changes needed to succeed.

Finally, run

 git rev-parse --short=8 vxl-upstream

to get the commit from which the vxl-upstream branch must be started
on the next update.  Edit the "git branch vxl-upstream" line above to
record it, and commit this file.
