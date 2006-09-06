
# SET(WRAPPER_AUTO_INCLUDE_HEADERS OFF)
# WRAP_INCLUDE("itkNumericTraits.h")
WRAP_INCLUDE("itkNumericTraitsRGBPixel.h")
WRAP_INCLUDE("itkNumericTraitsTensorPixel.h")
WRAP_INCLUDE("itkNumericTraitsVariableLengthVectorPixel.h")
WRAP_INCLUDE("itkNumericTraitsVectorPixel.h")


WRAP_CLASS("itk::NumericTraits")
  # the basic types
  FOREACH(t UC US UI UL SC SS SI SL F D LD B)
    WRAP_TEMPLATE("${ITKM_${t}}" "${ITKT_${t}}")
  ENDFOREACH(t)
  
  # the ITK types
  # TODO: Get why build fail with RBG and vectors types and wrap them 
  FOREACH(t ${WRAP_ITK_COMPLEX_REAL})   # ${WRAP_ITK_RGB} 
    WRAP_TEMPLATE("${ITKM_${t}}" "${ITKT_${t}}")
  ENDFOREACH(t)
  
#  FOREACH(d ${WRAP_ITK_DIMS})
#    FOREACH(t ${WRAP_ITK_VECTOR_REAL} ${WRAP_ITK_COV_VECTOR_REAL})
#      WRAP_TEMPLATE("${ITKM_${t}${d}}" "${ITKT_${t}${d}}")
#    ENDFOREACH(t)
#  ENDFOREACH(d)
  
  # TODO: the VariableLengthVectorPixel, which is not defined in WraITKTypes.cmake
  
  # save the template parameters declared here to reuse them for the superclass
  SET(param_set ${WRAPPER_TEMPLATES})
  
END_WRAP_CLASS()


# disable auto include at that point. vcl_numeric_limits is defined in
# vcl_limits.h, and already included in itkNumericTraits.h
SET(WRAPPER_AUTO_INCLUDE_HEADERS OFF)

WRAP_CLASS(vcl_numeric_limits FORCE_INSTANTIATE)
  SET(WRAPPER_TEMPLATES ${param_set})
END_WRAP_CLASS()
