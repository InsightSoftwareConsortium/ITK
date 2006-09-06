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
SET(WRAP_ITK_USIGN_INT "")
IF(WRAP_unsigned_char)
  SET(WRAP_ITK_USIGN_INT ${WRAP_ITK_USIGN_INT} "UC")
ENDIF(WRAP_unsigned_char)
IF(WRAP_unsigned_long)
  SET(WRAP_ITK_USIGN_INT ${WRAP_ITK_USIGN_INT} "UL")
ENDIF(WRAP_unsigned_long)
IF(WRAP_unsigned_short)
  SET(WRAP_ITK_USIGN_INT ${WRAP_ITK_USIGN_INT} "US")
ENDIF(WRAP_unsigned_short)

SET(WRAP_ITK_SIGN_INT "")
IF(WRAP_signed_char)
  SET(WRAP_ITK_SIGN_INT ${WRAP_ITK_SIGN_INT} "SC")
ENDIF(WRAP_signed_char)
IF(WRAP_signed_long)
  SET(WRAP_ITK_SIGN_INT ${WRAP_ITK_SIGN_INT} "SL")
ENDIF(WRAP_signed_long)
IF(WRAP_signed_short)
  SET(WRAP_ITK_SIGN_INT ${WRAP_ITK_SIGN_INT} "SS")
ENDIF(WRAP_signed_short)

SET(WRAP_ITK_REAL "")
IF(WRAP_float)
  SET(WRAP_ITK_REAL ${WRAP_ITK_REAL} "F")
ENDIF(WRAP_float)
IF(WRAP_double)
  SET(WRAP_ITK_REAL ${WRAP_ITK_REAL} "D")
ENDIF(WRAP_double)

SET(WRAP_ITK_RGB "")
IF(WRAP_rgb_unsigned_char)
  SET(WRAP_ITK_RGB ${WRAP_ITK_RGB} "RGBUC")
ENDIF(WRAP_rgb_unsigned_char)
IF(WRAP_rgb_unsigned_short)
  SET(WRAP_ITK_RGB ${WRAP_ITK_RGB} "RGBUS")
ENDIF(WRAP_rgb_unsigned_short)

SET(WRAP_ITK_VECTOR_REAL "")
IF(WRAP_vector_double)
  SET(WRAP_ITK_VECTOR_REAL ${WRAP_ITK_VECTOR_REAL} "VD")
ENDIF(WRAP_vector_double)
IF(WRAP_vector_float)
  SET(WRAP_ITK_VECTOR_REAL ${WRAP_ITK_VECTOR_REAL} "VF")
ENDIF(WRAP_vector_float)

SET(WRAP_ITK_COV_VECTOR_REAL "")
IF(WRAP_covariant_vector_double)
  SET(WRAP_ITK_COV_VECTOR_REAL ${WRAP_ITK_COV_VECTOR_REAL} "CVD")
ENDIF(WRAP_covariant_vector_double)
IF(WRAP_covariant_vector_float)
  SET(WRAP_ITK_COV_VECTOR_REAL ${WRAP_ITK_COV_VECTOR_REAL} "CVF")
ENDIF(WRAP_covariant_vector_float)

SET(WRAP_ITK_COMPLEX_REAL "")
IF(WRAP_complex_double)
  SET(WRAP_ITK_COMPLEX_REAL ${WRAP_ITK_COMPLEX_REAL} "CD")
ENDIF(WRAP_complex_double)
IF(WRAP_complex_float)
  SET(WRAP_ITK_COMPLEX_REAL ${WRAP_ITK_COMPLEX_REAL} "CF")
ENDIF(WRAP_complex_float)

SET(WRAP_ITK_INT ${WRAP_ITK_SIGN_INT} ${WRAP_ITK_USIGN_INT})
SET(WRAP_ITK_SCALAR ${WRAP_ITK_INT} ${WRAP_ITK_REAL})
SET(WRAP_ITK_VECTOR ${WRAP_ITK_VECTOR_REAL} ${WRAP_ITK_COV_VECTOR_REAL})
SET(WRAP_ITK_ALL_TYPES ${WRAP_ITK_RGB} ${WRAP_ITK_VECTOR} ${WRAP_ITK_SCALAR} ${WRAP_ITK_COMPLEX_REAL})

# Make a list of all selected types "smaller than" a given type
INTERSECTION(SMALLER_THAN_D  "F;UL;US;UC;SL;SS;SC" "${WRAP_ITK_SCALAR}")
INTERSECTION(SMALLER_THAN_F  "UL;US;UC;SL;SS;SC" "${WRAP_ITK_SCALAR}")
INTERSECTION(SMALLER_THAN_UL "US;UC;SL;SS;SC" "${WRAP_ITK_INT}")
INTERSECTION(SMALLER_THAN_US "UC;SC" "${WRAP_ITK_INT}")
INTERSECTION(SMALLER_THAN_SL "US;UC;SS;SC" "${WRAP_ITK_INT}")
INTERSECTION(SMALLER_THAN_SS "UC;SC" "${WRAP_ITK_INT}")
