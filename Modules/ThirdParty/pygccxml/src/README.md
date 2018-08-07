pygccxml
========

This subdirectory contains a reduced distribution of the `pygccxml` source tree
with just the library source.

This module is used by `WrapITK` to build the wrappings. We distribute our own
version so that the user does not have to install this dependency. This
module does not need to be compiled as it is a pure Python module. We do not
allow (for the moment) the usage the system's `pygccxml` and rely on the one
shipped here.

To update pygccxml in ITK:

1. In *UpdatepygccxmlFromUpstream.sh*, update `git_tag` and `upstream_sha`,
   and commit the result
2. Run *UpdatepygccxmlFromUpstream.sh* from the top level of the ITK
   repository
3. Follow the instructions to resolve any merged conflicts.
4. In *UpdatepygccxmlFromUpstream.sh*, update the sha on the `git branch
   pygccxml-upstream` line and commit the result
