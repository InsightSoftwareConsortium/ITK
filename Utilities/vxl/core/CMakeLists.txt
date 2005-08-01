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

INSTALL_FILES(${VXL_INSTALL_ROOT}/core ".h" vxl_config vxl_copyright vxl_version)
