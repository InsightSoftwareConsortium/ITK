###############################################################################
# Set up variables to store "mangled names" and proper C++ type names for the
# basic types.
###############################################################################

SET(ITKT_UC "unsigned char")  # Type
SET(ITKM_UC "UC")             # Mangle

SET(ITKT_US "unsigned short") # Type
SET(ITKM_US "US")             # Mangle

SET(ITKT_UI "unsigned int")   # Type
SET(ITKM_UI "UI")             # Mangle

SET(ITKT_UL "unsigned long")  # Type
SET(ITKM_UL "UL")             # Mangle

SET(ITKT_SC "signed char")    # Type
SET(ITKM_SC "SC")             # Mangle

SET(ITKT_SS "signed short")   # Type
SET(ITKM_SS "SS")             # Mangle

SET(ITKT_SI "signed int")     # Type
SET(ITKM_SI "SI")             # Mangle

SET(ITKT_SL "signed long")    # Type
SET(ITKM_SL "SL")             # Mangle

SET(ITKT_F  "float")          # Type
SET(ITKM_F  "F")              # Mangle

SET(ITKT_D  "double")         # Type
SET(ITKM_D  "D")              # Mangle

SET(ITKT_LD  "long double")   # Type
SET(ITKM_LD  "LD")            # Mangle

SET(ITKT_B  "bool")           # Type
SET(ITKM_B  "B")              # Mangle


###############################################################################
# Create some variable which can be used later
###############################################################################
SET(EXPLICIT_ITK_INT "")
IF(EXPLICIT_unsigned_char)
  SET(EXPLICIT_ITK_INT ${EXPLICIT_ITK_INT} "UC")
ENDIF(EXPLICIT_unsigned_char)
IF(EXPLICIT_unsigned_long)
  SET(EXPLICIT_ITK_INT ${EXPLICIT_ITK_INT} "UL")
ENDIF(EXPLICIT_unsigned_long)
IF(EXPLICIT_unsigned_short)
  SET(EXPLICIT_ITK_INT ${EXPLICIT_ITK_INT} "US")
ENDIF(EXPLICIT_unsigned_short)
IF(EXPLICIT_unsigned_int)
  SET(EXPLICIT_ITK_INT ${EXPLICIT_ITK_INT} "UI")
ENDIF(EXPLICIT_unsigned_int)

SET(EXPLICIT_ITK_SIGN_INT "")
IF(EXPLICIT_signed_char)
  SET(EXPLICIT_ITK_SIGN_INT ${EXPLICIT_ITK_SIGN_INT} "SC")
ENDIF(EXPLICIT_signed_char)
IF(EXPLICIT_signed_long)
  SET(EXPLICIT_ITK_SIGN_INT ${EXPLICIT_ITK_SIGN_INT} "SL")
ENDIF(EXPLICIT_signed_long)
IF(EXPLICIT_signed_short)
  SET(EXPLICIT_ITK_SIGN_INT ${EXPLICIT_ITK_SIGN_INT} "SS")
ENDIF(EXPLICIT_signed_short)
IF(EXPLICIT_signed_int)
  SET(EXPLICIT_ITK_SIGN_INT ${EXPLICIT_ITK_SIGN_INT} "SI")
ENDIF(EXPLICIT_signed_int)

SET(EXPLICIT_ITK_REAL "")
IF(EXPLICIT_float)
  SET(EXPLICIT_ITK_REAL ${EXPLICIT_ITK_REAL} "F")
ENDIF(EXPLICIT_float)
IF(EXPLICIT_double)
  SET(EXPLICIT_ITK_REAL ${EXPLICIT_ITK_REAL} "D")
ENDIF(EXPLICIT_double)

SET(EXPLICIT_ITK_RGB "")
IF(EXPLICIT_rgb_unsigned_char)
  SET(EXPLICIT_ITK_RGB ${EXPLICIT_ITK_RGB} "RGBUC")
ENDIF(EXPLICIT_rgb_unsigned_char)
IF(EXPLICIT_rgb_unsigned_short)
  SET(EXPLICIT_ITK_RGB ${EXPLICIT_ITK_RGB} "RGBUS")
ENDIF(EXPLICIT_rgb_unsigned_short)

SET(EXPLICIT_ITK_VECTOR_REAL "")
IF(EXPLICIT_vector_double)
  SET(EXPLICIT_ITK_VECTOR_REAL ${EXPLICIT_ITK_VECTOR_REAL} "VD")
ENDIF(EXPLICIT_vector_double)
IF(EXPLICIT_vector_float)
  SET(EXPLICIT_ITK_VECTOR_REAL ${EXPLICIT_ITK_VECTOR_REAL} "VF")
ENDIF(EXPLICIT_vector_float)

SET(EXPLICIT_ITK_COV_VECTOR_REAL "")
IF(EXPLICIT_covariant_vector_double)
  SET(EXPLICIT_ITK_COV_VECTOR_REAL ${EXPLICIT_ITK_COV_VECTOR_REAL} "CVD")
ENDIF(EXPLICIT_covariant_vector_double)
IF(EXPLICIT_covariant_vector_float)
  SET(EXPLICIT_ITK_COV_VECTOR_REAL ${EXPLICIT_ITK_COV_VECTOR_REAL} "CVF")
ENDIF(EXPLICIT_covariant_vector_float)

SET(EXPLICIT_ITK_COMPLEX_REAL "")
IF(EXPLICIT_complex_double)
  SET(EXPLICIT_ITK_COMPLEX_REAL ${EXPLICIT_ITK_VECTOR_REAL} "CD")
ENDIF(EXPLICIT_complex_double)
IF(EXPLICIT_complex_float)
  SET(EXPLICIT_ITK_COMPLEX_REAL ${EXPLICIT_ITK_VECTOR_REAL} "CF")
ENDIF(EXPLICIT_complex_float)

SET(EXPLICIT_ITK_INTEGRAL ${EXPLICIT_ITK_SIGN_INT} ${EXPLICIT_ITK_INT})
SET(EXPLICIT_ITK_SCALAR ${EXPLICIT_ITK_INTEGRAL} ${EXPLICIT_ITK_REAL})
SET(EXPLICIT_ITK_VECTOR ${EXPLICIT_ITK_VECTOR_REAL} ${EXPLICIT_ITK_COV_VECTOR_REAL})
SET(EXPLICIT_ITK_ALL_TYPES ${EXPLICIT_ITK_RGB} ${EXPLICIT_ITK_VECTOR} ${EXPLICIT_ITK_SCALAR} ${EXPLICIT_ITK_COMPLEX_REAL})

# Make a list of all selected types "smaller than" a given type
INTERSECTION(SMALLER_THAN_D  "F;UL;US;UC;SL;SS;SC" "${EXPLICIT_ITK_SCALAR}")
INTERSECTION(SMALLER_THAN_UL "US;UC;SL;SS;SC" "${EXPLICIT_ITK_INTEGRAL}")
INTERSECTION(SMALLER_THAN_US "UC;SC" "${EXPLICIT_ITK_INTEGRAL}")
INTERSECTION(SMALLER_THAN_SL "US;UC;SS;SC" "${EXPLICIT_ITK_INTEGRAL}")
INTERSECTION(SMALLER_THAN_SS "UC;SC" "${EXPLICIT_ITK_INTEGRAL}")
