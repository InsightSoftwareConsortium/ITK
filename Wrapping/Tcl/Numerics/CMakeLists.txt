INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Wrapping/Tcl/Numerics)

ADD_LIBRARY(ITKNumericsTcl)
ITK_WRAP_TCL(ITKNumericsTcl
  wrap_ITKNumerics.cxx
  wrap_ITKCostFunctions.cxx
  wrap_ITKOptimizers.cxx
)

TARGET_LINK_LIBRARIES(ITKNumericsTcl ITKNumerics ${CABLE_TCL_LIBRARY})

ADD_LIBRARY(VXLNumericsTcl)
ITK_WRAP_TCL(VXLNumericsTcl
  wrap_VXLNumerics.cxx
  wrap_vnl_matrix.cxx
  wrap_vnl_vector.cxx
  wrap_vnl_c_vector.cxx
  wrap_vnl_diag_matrix.cxx
  wrap_vnl_file_matrix.cxx
  wrap_vnl_file_vector.cxx
  wrap_vnl_fortran_copy.cxx
  wrap_vnl_matrix_fixed.cxx
  wrap_vnl_matrix_fixed_ref.cxx
  wrap_vnl_matrix_ref.cxx
  wrap_vnl_vector_ref.cxx
)

TARGET_LINK_LIBRARIES(VXLNumericsTcl ${VXL_NUMERICS_LIBRARIES} ${CABLE_TCL_LIBRARY})

INSTALL_TARGETS(/lib/InsightToolkit ITKNumericsTcl)
INSTALL_TARGETS(/lib/InsightToolkit VXLNumericsTcl)
