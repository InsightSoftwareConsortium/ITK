#
# Find CABLE in an installation or a build tree.
#
# Sets CABLE_USE_FILE to the "UseCABLE.cmake".
#

FIND_FILE(CABLE_USE_FILE UseCABLE.cmake
  /usr/local/lib/Cable
  /usr/lib/Cable
)
