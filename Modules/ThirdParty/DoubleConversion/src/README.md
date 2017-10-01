DoubleConversion
================

The `./double-conversion` subdirectory contains a reduced distribution of the
Google `double-conversion` source tree with just the library source.

The script `./UpdateDoubleConversionFromGoogle.sh` describes the procedure to
merge in any new changes in the upstream Google repository.

**Note**: After the upstream `double-conversion` code has been merged and tested,
and BEFORE using `git gerrit-push` to push the topic to the [Kitware Gerrit],
use the procedure documented at the bottom of the update script to recover
the upstream merge hash and put it in the update script.

Search for "EDIT THIS SCRIPT" in `UpdateDoubleConversionFromGoogle.sh` to
find the point to change the hash number.



[Kitware Gerrit]: http://review.source.kitware.com/