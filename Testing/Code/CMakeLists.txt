IF(ITK_WRAP_TCL)
  INCLUDE_DIRECTORIES(${TCL_INCLUDE_PATH}
                      ${CABLE_INCLUDE_DIR}
                      ${ITK_SOURCE_DIR}/Code/Numerics/vxl/vcl/doc
                      ${ITK_SOURCE_DIR}/Code/Numerics/vxl/vnl)
ENDIF(ITK_WRAP_TCL)

SUBDIRS( Numerics Common BasicFilters Algorithms IO SpatialObject )
