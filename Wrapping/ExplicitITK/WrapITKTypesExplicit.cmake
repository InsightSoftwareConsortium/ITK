#------------------------------------------------------------------------------

# define some macro to help creation of types vars

MACRO(WRAP_EXPLICIT_TYPE class prefix)
   # begin the creation of a type vars
   # call to this macro should be followed by one or several call to WRAP_TEMPLATE()
   # and one call to END_WRAP_EXPLICIT_TYPE to really create the vars
   SET(WRAPPER_TEMPLATES "")
   SET(itk_Wrap_Explicit_Prefix "${prefix}")
   SET(itk_Wrap_Explicit_Class "${class}")
ENDMACRO(WRAP_EXPLICIT_TYPE)

MACRO(END_WRAP_EXPLICIT_TYPE)
   # create the type vars.
   # must be called after END_WRAP_EXPLICIT_TYPE
   # Create the vars used for to designe types in all the cmake
   # files. This method ensure all the type names are constructed
   # with the same method
   FOREACH(wrap ${WRAPPER_TEMPLATES})
      STRING(REGEX REPLACE "([0-9A-Za-z]*)[ ]*#[ ]*(.*)" "\\1" wrapTpl "${wrap}")
      STRING(REGEX REPLACE "([0-9A-Za-z]*)[ ]*#[ ]*(.*)" "\\2" wrapType "${wrap}")
      SET(ITKT_${itk_Wrap_Explicit_Prefix}${wrapTpl} "Templates::${itk_Wrap_Explicit_Class}${wrapTpl}")
      SET(ITKM_${itk_Wrap_Explicit_Prefix}${wrapTpl} "${itk_Wrap_Explicit_Prefix}${wrapTpl}")
   ENDFOREACH(wrap)
ENDMACRO(END_WRAP_EXPLICIT_TYPE)

#------------------------------------------------------------------------------

# now, define types vars
# the result is stored in itk_Wrap_Explicit_XXX where XXX is the name of the class
# to be wrapped in there own file, most of the time in CommonA


WRAP_EXPLICIT_TYPE("Offset" "O")
  FOREACH(d ${WRAP_ITK_DIMS})
    WRAP_TEMPLATE("${d}"  "${d}")
  ENDFOREACH(d)
END_WRAP_EXPLICIT_TYPE()
SET(itk_Wrap_Explicit_Offset ${WRAPPER_TEMPLATES})

WRAP_EXPLICIT_TYPE("Vector" "V")
  FOREACH(d ${WRAP_ITK_DIMS})
    WRAP_TEMPLATE("${ITKM_F}${d}"  "${ITKT_F},${d}")
    WRAP_TEMPLATE("${ITKM_D}${d}"  "${ITKT_D},${d}")
  ENDFOREACH(d)
END_WRAP_EXPLICIT_TYPE()
SET(itk_Wrap_Explicit_Vector ${WRAPPER_TEMPLATES})

WRAP_EXPLICIT_TYPE("CovariantVector" "CV")
  FOREACH(d ${WRAP_ITK_DIMS})
    WRAP_TEMPLATE("${ITKM_F}${d}"  "${ITKT_F},${d}")
    WRAP_TEMPLATE("${ITKM_D}${d}"  "${ITKT_D},${d}")
  ENDFOREACH(d)
END_WRAP_EXPLICIT_TYPE()
SET(itk_Wrap_Explicit_CovariantVector ${WRAPPER_TEMPLATES})

WRAP_EXPLICIT_TYPE("ContinuousIndex" "CI")
  FOREACH(d ${WRAP_ITK_DIMS})
    WRAP_TEMPLATE("${ITKM_F}${d}"  "${ITKT_F},${d}")
    WRAP_TEMPLATE("${ITKM_D}${d}"  "${ITKT_D},${d}")
  ENDFOREACH(d)
END_WRAP_EXPLICIT_TYPE()
SET(itk_Wrap_Explicit_ContinuousIndex ${WRAPPER_TEMPLATES})

WRAP_EXPLICIT_TYPE("Array" "A")
  WRAP_TEMPLATE("${ITKM_D}" "${ITKT_D}")
  WRAP_TEMPLATE("${ITKM_F}" "${ITKT_F}")
END_WRAP_EXPLICIT_TYPE()
SET(itk_Wrap_Explicit_Array ${WRAPPER_TEMPLATES})

WRAP_EXPLICIT_TYPE("Array2D" "A2D")
  WRAP_TEMPLATE("${ITKM_D}" "${ITKT_D}")
  WRAP_TEMPLATE("${ITKM_F}" "${ITKT_F}")
END_WRAP_EXPLICIT_TYPE()
SET(itk_Wrap_Explicit_Array2D ${WRAPPER_TEMPLATES})

WRAP_EXPLICIT_TYPE("FixedArray" "FA")
  UNIQUE(array_sizes "${WRAP_ITK_DIMS};1")
  # make sure that 1-D FixedArrays are wrapped. Also wrap for each selected
  # image dimension.
  # TODO: Do we need fixed arrays for all of these types? For just the selected
  # pixel types plus some few basic cases? Or just for a basic set of types?
  FOREACH(d ${array_sizes})
    WRAP_TEMPLATE("${ITKM_D}${d}"  "${ITKT_D},${d}")
    WRAP_TEMPLATE("${ITKM_F}${d}"  "${ITKT_F},${d}")
    WRAP_TEMPLATE("${ITKM_UL}${d}" "${ITKT_UL},${d}")
    WRAP_TEMPLATE("${ITKM_US}${d}" "${ITKT_US},${d}")
    WRAP_TEMPLATE("${ITKM_UC}${d}" "${ITKT_UC},${d}")
    WRAP_TEMPLATE("${ITKM_UI}${d}" "${ITKT_UI},${d}")
    WRAP_TEMPLATE("${ITKM_SL}${d}" "${ITKT_SL},${d}")
    WRAP_TEMPLATE("${ITKM_SS}${d}" "${ITKT_SS},${d}")
    WRAP_TEMPLATE("${ITKM_SC}${d}" "${ITKT_SC},${d}")
    WRAP_TEMPLATE("${ITKM_B}${d}"  "${ITKT_B},${d}")
  ENDFOREACH(d)
END_WRAP_EXPLICIT_TYPE()
SET(itk_Wrap_Explicit_FixedArray ${WRAPPER_TEMPLATES})

WRAP_EXPLICIT_TYPE("RGBPixel" "RGB")
  IF(WRAP_rgb_unsigned_char)
    WRAP_TEMPLATE("${ITKM_UC}" "${ITKT_UC}")
  ENDIF(WRAP_rgb_unsigned_char)
  
  IF(WRAP_rgb_unsigned_short)
    WRAP_TEMPLATE("${ITKM_US}" "${ITKT_US}")
  ENDIF(WRAP_rgb_unsigned_short)
END_WRAP_EXPLICIT_TYPE()
SET(itk_Wrap_Explicit_RGBPixel ${WRAPPER_TEMPLATES})

WRAP_EXPLICIT_TYPE("Image" "I")
  # Make a list of all of the selected image pixel types and also double (for
  # BSplineDeformableTransform), uchar (for 8-bit image output), and ulong
  # (for the watershed and relabel filters).
  UNIQUE(wrap_image_types "${WRAP_ITK_ALL_TYPES};D;UC;UL")
  
  FOREACH(d ${WRAP_ITK_DIMS})
    FOREACH(type ${wrap_image_types})
      IF("${WRAP_ITK_VECTOR}" MATCHES "(^|;)${type}(;|$)")
        # if the type is a vector type with no dimension specified, make the 
        # vector dimension match the image dimension.
        SET(type "${type}${d}")
      ENDIF("${WRAP_ITK_VECTOR}" MATCHES "(^|;)${type}(;|$)")
      
      WRAP_TEMPLATE("${ITKM_${type}}${d}"  "${ITKT_${type}},${d}")
    ENDFOREACH(type)
  ENDFOREACH(d)
END_WRAP_EXPLICIT_TYPE()
SET(itk_Wrap_Explicit_Image ${WRAPPER_TEMPLATES})

WRAP_EXPLICIT_TYPE("VectorImage" "VI")
  # Make a list of all of the selected image pixel types and also uchar 
  # (for 8-bit image output)
  UNIQUE(wrap_image_types "${WRAP_ITK_SCALAR};UC")
  
  FOREACH(d ${WRAP_ITK_DIMS})
    FOREACH(type ${wrap_image_types})
      WRAP_TEMPLATE("${ITKM_${type}}${d}"  "${ITKT_${type}},${d}")
    ENDFOREACH(type)
  ENDFOREACH(d)
END_WRAP_EXPLICIT_TYPE()
SET(itk_Wrap_Explicit_VectorImage ${WRAPPER_TEMPLATES})

WRAP_EXPLICIT_TYPE("VariableLengthVector" "VLV")
  # Make a list of all of the selected image pixel types and also uchar 
  # (for 8-bit image output)
  UNIQUE(wrap_image_types "${WRAP_ITK_SCALAR};UC")
  
  FOREACH(type ${wrap_image_types})
    WRAP_TEMPLATE("${ITKM_${type}}"  "${ITKT_${type}}")
  ENDFOREACH(type)
END_WRAP_EXPLICIT_TYPE()
SET(itk_Wrap_Explicit_VariableLengthVector ${WRAPPER_TEMPLATES})

WRAP_EXPLICIT_TYPE("Point" "P")
  FOREACH(d ${WRAP_ITK_DIMS})
    WRAP_TEMPLATE("${ITKM_F}${d}"  "${ITKT_F},${d}")
    WRAP_TEMPLATE("${ITKM_D}${d}"  "${ITKT_D},${d}")
  ENDFOREACH(d)
END_WRAP_EXPLICIT_TYPE()
SET(itk_Wrap_Explicit_Point ${WRAPPER_TEMPLATES})

WRAP_EXPLICIT_TYPE("LevelSetNode" "LSN")
  # Only make level set nodes for the selected image pixel types
  FOREACH(d ${WRAP_ITK_DIMS})
    FOREACH(type ${WRAP_ITK_SCALAR})
      WRAP_TEMPLATE("${ITKM_${type}}${d}"  "${ITKT_${type}},${d}")
    ENDFOREACH(type)
  ENDFOREACH(d)
END_WRAP_EXPLICIT_TYPE()
SET(itk_Wrap_Explicit_LevelSetNode ${WRAPPER_TEMPLATES})

WRAP_EXPLICIT_TYPE("BinaryBallStructuringElement" "SE")
  # Only make structuring elements for the selected image pixel types
  FOREACH(d ${WRAP_ITK_DIMS})
    FOREACH(type ${WRAP_ITK_SCALAR})
      WRAP_TEMPLATE("${ITKM_${type}}${d}"  "${ITKT_${type}},${d}")
    ENDFOREACH(type)
    WRAP_TEMPLATE("${ITKM_B}${d}"  "${ITKT_B},${d}")
  ENDFOREACH(d)
END_WRAP_EXPLICIT_TYPE()
SET(itk_Wrap_Explicit_StructuringElement ${WRAPPER_TEMPLATES})

WRAP_EXPLICIT_TYPE("SpatialObject" "SO")
  FOREACH(d ${WRAP_ITK_DIMS})
    WRAP_TEMPLATE("${d}"  "${d}")
  ENDFOREACH(d)
END_WRAP_EXPLICIT_TYPE()
SET(itk_Wrap_Explicit_SpatialObject ${WRAPPER_TEMPLATES})

