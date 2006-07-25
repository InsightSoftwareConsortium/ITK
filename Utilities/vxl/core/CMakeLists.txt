PROJECT( vxlcore )

SET(global_sources
 vxl_copyright.h
 vxl_version.h
 vxl_config.h.in
)

IF(NOT VXL_INSTALL_NO_DEVELOPMENT)
  INSTALL_NOBASE_HEADER_FILES(${VXL_INSTALL_ROOT}/core ${global_sources})
ENDIF(NOT VXL_INSTALL_NO_DEVELOPMENT)

# common test executable
SUBDIRS(testlib)

# numerics
SUBDIRS(vnl)
