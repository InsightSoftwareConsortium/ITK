#
# Try to find netlib
#

# AGAP: Is it even possible to have a "native" netlib? Even if not, it is
# good to have this file in place, so that all things in v3p are found
# via a module.

set( NETLIB_FOUND "YES" )
set( NETLIB_INCLUDE_DIR ${VXL_ROOT_SOURCE_DIR}/v3p/netlib )
set( NETLIB_INSTALL_INCLUDE_DIR ${CMAKE_INSTALL_PREFIX}/include/vxl/v3p/netlib )
set( NETLIB_LIBRARIES ${VXL_LIB_PREFIX}netlib ${VXL_LIB_PREFIX}v3p_netlib )
