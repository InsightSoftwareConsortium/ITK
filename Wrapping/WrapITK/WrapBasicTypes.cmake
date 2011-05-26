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
set(WRAP_ITK_USIGN_INT "")
if(WRAP_unsigned_char)
  set(WRAP_ITK_USIGN_INT ${WRAP_ITK_USIGN_INT} "UC")
endif(WRAP_unsigned_char)
if(WRAP_unsigned_long)
  set(WRAP_ITK_USIGN_INT ${WRAP_ITK_USIGN_INT} "UL")
endif(WRAP_unsigned_long)
if(WRAP_unsigned_short)
  set(WRAP_ITK_USIGN_INT ${WRAP_ITK_USIGN_INT} "US")
endif(WRAP_unsigned_short)

set(WRAP_ITK_SIGN_INT "")
if(WRAP_signed_char)
  set(WRAP_ITK_SIGN_INT ${WRAP_ITK_SIGN_INT} "SC")
endif(WRAP_signed_char)
if(WRAP_signed_long)
  set(WRAP_ITK_SIGN_INT ${WRAP_ITK_SIGN_INT} "SL")
endif(WRAP_signed_long)
if(WRAP_signed_short)
  set(WRAP_ITK_SIGN_INT ${WRAP_ITK_SIGN_INT} "SS")
endif(WRAP_signed_short)

set(WRAP_ITK_REAL "")
if(WRAP_float)
  set(WRAP_ITK_REAL ${WRAP_ITK_REAL} "F")
endif(WRAP_float)
if(WRAP_double)
  set(WRAP_ITK_REAL ${WRAP_ITK_REAL} "D")
endif(WRAP_double)

set(WRAP_ITK_RGB "")
if(WRAP_rgb_unsigned_char)
  set(WRAP_ITK_RGB ${WRAP_ITK_RGB} "RGBUC")
endif(WRAP_rgb_unsigned_char)
if(WRAP_rgb_unsigned_short)
  set(WRAP_ITK_RGB ${WRAP_ITK_RGB} "RGBUS")
endif(WRAP_rgb_unsigned_short)

set(WRAP_ITK_RGBA "")
if(WRAP_rgba_unsigned_char)
  set(WRAP_ITK_RGBA ${WRAP_ITK_RGBA} "RGBAUC")
endif(WRAP_rgba_unsigned_char)
if(WRAP_rgba_unsigned_short)
  set(WRAP_ITK_RGBA ${WRAP_ITK_RGBA} "RGBAUS")
endif(WRAP_rgba_unsigned_short)

set(WRAP_ITK_VECTOR_REAL "")
if(WRAP_vector_double)
  set(WRAP_ITK_VECTOR_REAL ${WRAP_ITK_VECTOR_REAL} "VD")
endif(WRAP_vector_double)
if(WRAP_vector_float)
  set(WRAP_ITK_VECTOR_REAL ${WRAP_ITK_VECTOR_REAL} "VF")
endif(WRAP_vector_float)

set(WRAP_ITK_COV_VECTOR_REAL "")
if(WRAP_covariant_vector_double)
  set(WRAP_ITK_COV_VECTOR_REAL ${WRAP_ITK_COV_VECTOR_REAL} "CVD")
endif(WRAP_covariant_vector_double)
if(WRAP_covariant_vector_float)
  set(WRAP_ITK_COV_VECTOR_REAL ${WRAP_ITK_COV_VECTOR_REAL} "CVF")
endif(WRAP_covariant_vector_float)

set(WRAP_ITK_COMPLEX_REAL "")
if(WRAP_complex_double)
  set(WRAP_ITK_COMPLEX_REAL ${WRAP_ITK_COMPLEX_REAL} "CD")
endif(WRAP_complex_double)
if(WRAP_complex_float)
  set(WRAP_ITK_COMPLEX_REAL ${WRAP_ITK_COMPLEX_REAL} "CF")
endif(WRAP_complex_float)

set(WRAP_ITK_INT ${WRAP_ITK_SIGN_INT} ${WRAP_ITK_USIGN_INT})
set(WRAP_ITK_SCALAR ${WRAP_ITK_INT} ${WRAP_ITK_REAL})
set(WRAP_ITK_VECTOR ${WRAP_ITK_VECTOR_REAL} ${WRAP_ITK_COV_VECTOR_REAL})
set(WRAP_ITK_COLOR ${WRAP_ITK_RGB} ${WRAP_ITK_RGBA})
set(WRAP_ITK_ALL_TYPES ${WRAP_ITK_RGB} ${WRAP_ITK_RGBA} ${WRAP_ITK_VECTOR} ${WRAP_ITK_SCALAR} ${WRAP_ITK_COMPLEX_REAL})

# Make a list of all selected types "smaller than" a given type
INTERSECTION(SMALLER_THAN_D  "F;UL;US;UC;SL;SS;SC" "${WRAP_ITK_SCALAR}")
INTERSECTION(SMALLER_THAN_F  "UL;US;UC;SL;SS;SC" "${WRAP_ITK_SCALAR}")
INTERSECTION(SMALLER_THAN_UL "US;UC;SL;SS;SC" "${WRAP_ITK_INT}")
INTERSECTION(SMALLER_THAN_US "UC;SC" "${WRAP_ITK_INT}")
INTERSECTION(SMALLER_THAN_SL "US;UC;SS;SC" "${WRAP_ITK_INT}")
INTERSECTION(SMALLER_THAN_SS "UC;SC" "${WRAP_ITK_INT}")
