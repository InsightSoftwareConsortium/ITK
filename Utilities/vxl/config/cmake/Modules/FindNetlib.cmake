#
# Try to find netlib
#

# AGAP: Is it even possible to have a "native" netlib? Even if not, it is
# good to have this file in place, so that all things in v3p are found
# via a module.

SET( NETLIB_FOUND "YES" )
SET( NETLIB_INCLUDE_DIR ${vxl_SOURCE_DIR}/v3p/netlib )
SET( NETLIB_LIBRARIES netlib )


