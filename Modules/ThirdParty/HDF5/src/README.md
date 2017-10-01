HDF5
====

The `itkhdf5` subdirectory contains a reduced distribution of the hdf5 source
tree with only the library source code and [CMake] build system. It is not a
submodule; the actual content is part of our source tree and changes can be
made and committed directly.

We update from upstream using Git's "subtree" merge strategy. A special branch
contains commits of upstream hdf5 snapshots and nothing else. No Git ref points
explicitly to the head of this branch, but it is merged into our history.

Update hdf5 from upstream as follows. Create a local branch to explicitly
reference the upstream snapshot branch head:

```sh
   $ git branch hdf5-upstream 5d643456
```

Use a temporary directory to checkout the branch:

```sh
   $ mkdir hdf5-tmp
   $ cd hdf5-tmp
   $ git init
   $ git pull .. hdf5-upstream
   $ rm -rf *
```

Now place the (reduced) hdf5 content in this directory. Edit and run the script

```sh
   $ ../Modules/ThirdParty/HDF5/src/DownloadAndReduceUpstream.sh
```

to extract the content from the upstream SVN repository. Then run the following
commands to commit the new version. Substitute the appropriate date and version
number:

```sh
   $ cp -a hdf5-*-reduced/* .
   $ rm -rf hdf5-*

   $ git add -u
   $ git add .

   $ GIT_AUTHOR_NAME='HDF Group' \
   $ GIT_AUTHOR_EMAIL='hdf-forum@hdfgroup.org' \
   $ GIT_AUTHOR_DATE='2016-07-24 23:06:03 -0400' \
   $ git commit -m 'ENH: hdf5 1.8.17-r30218 (reduced)'
```

Then push the changes back up to the main repository:

```sh
   $ git push .. HEAD:hdf5-upstream
   $ cd ..
   $ rm -rf hdf5-tmp
```

Create a topic in the main repository on which to perform the update:

```sh
   $ git checkout -b update-hdf5 master
```

Merge the `hdf5-upstream` branch as a subtree:

```sh
   $ git merge -s recursive -X subtree=Modules/ThirdParty/HDF5/src/itkhdf5 \
           hdf5-upstream
```

If there are conflicts, resolve them and commit. Build and test the tree.
Commit any additional changes needed to succeed.

Finally, run

```sh
   $ git rev-parse --short=8 hdf5-upstream
```

to get the commit from which the `hdf5-upstream` branch must be started on the
next update. Edit the `git branch hdf5-upstream` line above to record it, and
commit this file.



[CMake]: https://cmake.org/
