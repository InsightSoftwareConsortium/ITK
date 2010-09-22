###############################################################################
# Set up variables to store "mangled names" and proper C++ type names for the
# basic types.
###############################################################################

set(ITKT_UC "unsigned char")  # Type
set(ITKM_UC "UC")             # Mangle

set(ITKT_US "unsigned short") # Type
set(ITKM_US "US")             # Mangle

set(ITKT_UI "unsigned int")   # Type
set(ITKM_UI "UI")             # Mangle

set(ITKT_UL "unsigned long")  # Type
set(ITKM_UL "UL")             # Mangle

set(ITKT_SC "signed char")    # Type
set(ITKM_SC "SC")             # Mangle

set(ITKT_SS "signed short")   # Type
set(ITKM_SS "SS")             # Mangle

set(ITKT_SI "signed int")     # Type
set(ITKM_SI "SI")             # Mangle

set(ITKT_SL "signed long")    # Type
set(ITKM_SL "SL")             # Mangle

set(ITKT_F  "float")          # Type
set(ITKM_F  "F")              # Mangle

set(ITKT_D  "double")         # Type
set(ITKM_D  "D")              # Mangle

set(ITKT_LD  "long double")   # Type
set(ITKM_LD  "LD")            # Mangle

set(ITKT_B  "bool")           # Type
set(ITKM_B  "B")              # Mangle


###############################################################################
# Create some variable which can be used later
###############################################################################
set(EXPLICIT_ITK_INT "")
if(EXPLICIT_unsigned_char)
  set(EXPLICIT_ITK_INT ${EXPLICIT_ITK_INT} "UC")
endif(EXPLICIT_unsigned_char)
if(EXPLICIT_unsigned_long)
  set(EXPLICIT_ITK_INT ${EXPLICIT_ITK_INT} "UL")
endif(EXPLICIT_unsigned_long)
if(EXPLICIT_unsigned_short)
  set(EXPLICIT_ITK_INT ${EXPLICIT_ITK_INT} "US")
endif(EXPLICIT_unsigned_short)
if(EXPLICIT_unsigned_int)
  set(EXPLICIT_ITK_INT ${EXPLICIT_ITK_INT} "UI")
endif(EXPLICIT_unsigned_int)

set(EXPLICIT_ITK_SIGN_INT "")
if(EXPLICIT_signed_char)
  set(EXPLICIT_ITK_SIGN_INT ${EXPLICIT_ITK_SIGN_INT} "SC")
endif(EXPLICIT_signed_char)
if(EXPLICIT_signed_long)
  set(EXPLICIT_ITK_SIGN_INT ${EXPLICIT_ITK_SIGN_INT} "SL")
endif(EXPLICIT_signed_long)
if(EXPLICIT_signed_short)
  set(EXPLICIT_ITK_SIGN_INT ${EXPLICIT_ITK_SIGN_INT} "SS")
endif(EXPLICIT_signed_short)
if(EXPLICIT_signed_int)
  set(EXPLICIT_ITK_SIGN_INT ${EXPLICIT_ITK_SIGN_INT} "SI")
endif(EXPLICIT_signed_int)

set(EXPLICIT_ITK_REAL "")
if(EXPLICIT_float)
  set(EXPLICIT_ITK_REAL ${EXPLICIT_ITK_REAL} "F")
endif(EXPLICIT_float)
if(EXPLICIT_double)
  set(EXPLICIT_ITK_REAL ${EXPLICIT_ITK_REAL} "D")
endif(EXPLICIT_double)

set(EXPLICIT_ITK_RGB "")
if(EXPLICIT_rgb_unsigned_char)
  set(EXPLICIT_ITK_RGB ${EXPLICIT_ITK_RGB} "RGBUC")
endif(EXPLICIT_rgb_unsigned_char)
if(EXPLICIT_rgb_unsigned_short)
  set(EXPLICIT_ITK_RGB ${EXPLICIT_ITK_RGB} "RGBUS")
endif(EXPLICIT_rgb_unsigned_short)

set(EXPLICIT_ITK_VECTOR_REAL "")
if(EXPLICIT_vector_double)
  set(EXPLICIT_ITK_VECTOR_REAL ${EXPLICIT_ITK_VECTOR_REAL} "VD")
endif(EXPLICIT_vector_double)
if(EXPLICIT_vector_float)
  set(EXPLICIT_ITK_VECTOR_REAL ${EXPLICIT_ITK_VECTOR_REAL} "VF")
endif(EXPLICIT_vector_float)

set(EXPLICIT_ITK_COV_VECTOR_REAL "")
if(EXPLICIT_covariant_vector_double)
  set(EXPLICIT_ITK_COV_VECTOR_REAL ${EXPLICIT_ITK_COV_VECTOR_REAL} "CVD")
endif(EXPLICIT_covariant_vector_double)
if(EXPLICIT_covariant_vector_float)
  set(EXPLICIT_ITK_COV_VECTOR_REAL ${EXPLICIT_ITK_COV_VECTOR_REAL} "CVF")
endif(EXPLICIT_covariant_vector_float)

set(EXPLICIT_ITK_COMPLEX_REAL "")
if(EXPLICIT_complex_double)
  set(EXPLICIT_ITK_COMPLEX_REAL ${EXPLICIT_ITK_VECTOR_REAL} "CD")
endif(EXPLICIT_complex_double)
if(EXPLICIT_complex_float)
  set(EXPLICIT_ITK_COMPLEX_REAL ${EXPLICIT_ITK_VECTOR_REAL} "CF")
endif(EXPLICIT_complex_float)

set(EXPLICIT_ITK_INTEGRAL ${EXPLICIT_ITK_SIGN_INT} ${EXPLICIT_ITK_INT})
set(EXPLICIT_ITK_SCALAR ${EXPLICIT_ITK_INTEGRAL} ${EXPLICIT_ITK_REAL})
set(EXPLICIT_ITK_VECTOR ${EXPLICIT_ITK_VECTOR_REAL} ${EXPLICIT_ITK_COV_VECTOR_REAL})
set(EXPLICIT_ITK_ALL_TYPES ${EXPLICIT_ITK_RGB} ${EXPLICIT_ITK_VECTOR} ${EXPLICIT_ITK_SCALAR} ${EXPLICIT_ITK_COMPLEX_REAL})

# Make a list of all selected types "smaller than" a given type
INTERSECTION(SMALLER_THAN_D  "F;UL;US;UC;SL;SS;SC" "${EXPLICIT_ITK_SCALAR}")
INTERSECTION(SMALLER_THAN_UL "US;UC;SL;SS;SC" "${EXPLICIT_ITK_INTEGRAL}")
INTERSECTION(SMALLER_THAN_US "UC;SC" "${EXPLICIT_ITK_INTEGRAL}")
INTERSECTION(SMALLER_THAN_SL "US;UC;SS;SC" "${EXPLICIT_ITK_INTEGRAL}")
INTERSECTION(SMALLER_THAN_SS "UC;SC" "${EXPLICIT_ITK_INTEGRAL}")
