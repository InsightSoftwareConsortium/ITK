INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Wrapping/Tcl/Numerics)

ADD_LIBRARY(VXLNumericsTcl SHARED)
CABLE_WRAP_TCL(VXLNumericsTcl
  wrap_VXLNumerics.cxx
  wrap_vnl_matrix.cxx
  wrap_vnl_vector.cxx
  wrap_vnl_c_vector.cxx
  wrap_vnl_diag_matrix.cxx
  wrap_vnl_file_matrix.cxx
  wrap_vnl_file_vector.cxx
  wrap_vnl_fortran_copy.cxx
)

LINK_LIBRARIES(VXLNumerics ${CABLE_TCL_LIBRARY})
