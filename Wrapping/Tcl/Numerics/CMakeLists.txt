CABLE_CLASS_SET(Scalar float)

ADD_LIBRARY(VXLNumericsTcl SHARED)
CABLE_WRAP_TCL(VXLNumericsTcl
  "vnl_vector<$Scalar>;vnl_matrix.h"
  "vnl_matrix<$Scalar>;vnl_vector.h"
)

LINK_LIBRARIES(
VXLNumerics
${CABLE_TCL_LIBRARY}
${TCL_LIBRARY}
)
