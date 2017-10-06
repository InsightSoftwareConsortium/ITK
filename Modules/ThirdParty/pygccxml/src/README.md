pygccxml
========

This subdirectory contains a reduced distribution of the `pygccxml` source tree
with just the library source.

This module is used by `WrapITK` to build the wrappings. We distribute our own
version so that the user does not have to install this dependency. This
module does not need to be compiled as it is a pure Python module. We do not
allow (for the moment) the usage the system's `pygccxml` and rely on the one
shipped here.

See `Modules/ThirdParty/VNL/src/README.md` for update procedure of the
snapshot using a subtree merge.

The last merged upstream snapshot commit hash is:
`9f6f8dfecd74fdec274c1dbc273f785b529b5909`
