PROJECT( vxlcore )

SET(global_sources
 vxl_copyright.h
 vxl_version.h
 vxl_config.h.in
)

# common test executable
SUBDIRS(testlib)

# numerics
SUBDIRS(vnl)

IF(NOT VXL_INSTALL_NO_DEVELOPMENT)
  INSTALL_FILES(${VXL_INSTALL_ROOT}/core ".h" vxl_config vxl_copyright vxl_version)
ENDIF(NOT VXL_INSTALL_NO_DEVELOPMENT)
