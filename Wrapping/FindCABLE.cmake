#
# Find CABLE in an installation or a build tree.
#
# Sets CABLE_DIR to the "CABLEConfig.cmake".
#

FIND_PATH(CABLE_DIR CABLEConfig.cmake
  /usr/local/lib/Cable
  /usr/lib/Cable
)

IF(CABLE_DIR)
  INCLUDE(${CABLE_DIR}/CABLEConfig.cmake)
  SET(CABLE_FOUND 1)
ENDIF(CABLE_DIR)
