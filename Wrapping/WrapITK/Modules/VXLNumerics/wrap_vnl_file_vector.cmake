# auto include feature must be disable because the class is not in the file
# with the same name
SET(WRAPPER_AUTO_INCLUDE_HEADERS OFF)
WRAP_INCLUDE("vnl/vnl_file_vector.h")

WRAP_CLASS("vnl_file_vector")
  WRAP_TEMPLATE("${ITKM_D}" "${ITKT_D}")
END_WRAP_CLASS()
