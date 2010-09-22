#------------------------------------------------------------------------------

# set the default include files for the generated wrapper files
# itk::Command has to be available in all ITK wrapped files
set(WRAPPER_DEFAULT_INCLUDE "itkCommand.h")

# define some macro to help creation of types vars

macro(WRAP_TYPE class prefix)
   # begin the creation of a type vars
   # call to this macro should be followed by one or several call to WRAP_TEMPLATE()
   # and one call to END_WRAP_TYPE to really create the vars
   set(WRAPPER_TEMPLATES "")
   set(itk_Wrap_Prefix "${prefix}")
   set(itk_Wrap_Class "${class}")

   # If the type is ITK class, add apropriate include file
   if("${class}" MATCHES "itk::")
     string(REGEX REPLACE "itk.*::(.*)" "itk\\1" includeFileName "${class}")
     set(WRAPPER_DEFAULT_INCLUDE ${WRAPPER_DEFAULT_INCLUDE} "${includeFileName}.h")
   endif("${class}" MATCHES "itk::")
endmacro(WRAP_TYPE)

macro(END_WRAP_TYPE)
   # create the type vars.
   # must be called after END_WRAP_TYPE
   # Create the vars used for to designe types in all the cmake
   # files. This method ensure all the type names are constructed
   # with the same method
   foreach(wrap ${WRAPPER_TEMPLATES})
      string(REGEX REPLACE "([0-9A-Za-z]*)[ ]*#[ ]*(.*)" "\\1" wrapTpl "${wrap}")
      string(REGEX REPLACE "([0-9A-Za-z]*)[ ]*#[ ]*(.*)" "\\2" wrapType "${wrap}")
      set(ITKT_${itk_Wrap_Prefix}${wrapTpl} "${itk_Wrap_Class}< ${wrapType} >")
      set(ITKM_${itk_Wrap_Prefix}${wrapTpl} "${itk_Wrap_Prefix}${wrapTpl}")
   endforeach(wrap)
endmacro(END_WRAP_TYPE)

macro(ADD_TEMPLATE name types)
  WRAP_TEMPLATE("${name}" "${types}")
endmacro(ADD_TEMPLATE)

#------------------------------------------------------------------------------

# now, define types vars
# the result is stored in itk_Wrap_XXX where XXX is the name of the class
# to be wrapped in there own file, most of the time in CommonA


WRAP_TYPE("itk::Offset" "O")
  foreach(d ${WRAP_ITK_DIMS})
    WRAP_TEMPLATE("${d}"  "${d}")
  endforeach(d)
END_WRAP_TYPE()
set(itk_Wrap_Offset ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::Vector" "V")
  foreach(d ${WRAP_ITK_DIMS})
    WRAP_TEMPLATE("${ITKM_F}${d}"  "${ITKT_F},${d}")
    WRAP_TEMPLATE("${ITKM_D}${d}"  "${ITKT_D},${d}")
  endforeach(d)
END_WRAP_TYPE()
set(itk_Wrap_Vector ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::CovariantVector" "CV")
  foreach(d ${WRAP_ITK_DIMS})
    WRAP_TEMPLATE("${ITKM_F}${d}"  "${ITKT_F},${d}")
    WRAP_TEMPLATE("${ITKM_D}${d}"  "${ITKT_D},${d}")
  endforeach(d)
END_WRAP_TYPE()
set(itk_Wrap_CovariantVector ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::ContinuousIndex" "CI")
  foreach(d ${WRAP_ITK_DIMS})
    WRAP_TEMPLATE("${ITKM_F}${d}"  "${ITKT_F},${d}")
    WRAP_TEMPLATE("${ITKM_D}${d}"  "${ITKT_D},${d}")
  endforeach(d)
END_WRAP_TYPE()
set(itk_Wrap_ContinuousIndex ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::Array" "A")
  WRAP_TEMPLATE("${ITKM_D}" "${ITKT_D}")
  WRAP_TEMPLATE("${ITKM_F}" "${ITKT_F}")
END_WRAP_TYPE()
set(itk_Wrap_Array ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::FixedArray" "FA")
  UNIQUE(array_sizes "${WRAP_ITK_DIMS};1")
  # make sure that 1-D FixedArrays are wrapped. Also wrap for each selected
  # image dimension.
  # TODO: Do we need fixed arrays for all of these types? For just the selected
  # pixel types plus some few basic cases? Or just for a basic set of types?
  foreach(d ${array_sizes})
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
  endforeach(d)
END_WRAP_TYPE()
set(itk_Wrap_FixedArray ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::RGBPixel" "RGB")
  WRAP_TEMPLATE("${ITKM_UC}" "${ITKT_UC}")

  if(WRAP_rgb_unsigned_short)
    WRAP_TEMPLATE("${ITKM_US}" "${ITKT_US}")
  endif(WRAP_rgb_unsigned_short)
END_WRAP_TYPE()
set(itk_Wrap_RGBPixel ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::RGBAPixel" "RGBA")
  WRAP_TEMPLATE("${ITKM_UC}" "${ITKT_UC}")

  if(WRAP_rgba_unsigned_short)
    WRAP_TEMPLATE("${ITKM_US}" "${ITKT_US}")
  endif(WRAP_rgba_unsigned_short)
END_WRAP_TYPE()
set(itk_Wrap_RGBAPixel ${WRAPPER_TEMPLATES})

WRAP_TYPE("std::complex" "C")
  if(WRAP_complex_float)
    WRAP_TEMPLATE("${ITKM_F}"  "${ITKT_F}")
  endif(WRAP_complex_float)
  if(WRAP_complex_double)
    WRAP_TEMPLATE("${ITKM_D}"  "${ITKT_D}")
  endif(WRAP_complex_double)
END_WRAP_TYPE()
set(itk_Wrap_vcl_complex ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::Image" "I")
  # Make a list of all of the selected image pixel types and also double (for
  # BSplineDeformableTransform), uchar (for 8-bit image output), and ulong
  # (for the watershed and relabel filters).
  UNIQUE(wrap_image_types "${WRAP_ITK_ALL_TYPES};D;UC;UL;RGBUC;RGBAUC")

  foreach(d ${WRAP_ITK_DIMS})
    foreach(type ${wrap_image_types})
      if("${WRAP_ITK_VECTOR}" MATCHES "(^|;)${type}(;|$)")
        # if the type is a vector type with no dimension specified, make the
        # vector dimension match the image dimension.
        set(type "${type}${d}")
      endif("${WRAP_ITK_VECTOR}" MATCHES "(^|;)${type}(;|$)")

      WRAP_TEMPLATE("${ITKM_${type}}${d}"  "${ITKT_${type}},${d}")
    endforeach(type)
  endforeach(d)
END_WRAP_TYPE()
set(itk_Wrap_Image ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::VectorImage" "VI")
  # Make a list of all of the selected image pixel types and also uchar
  # (for 8-bit image output)
  UNIQUE(wrap_image_types "${WRAP_ITK_SCALAR};UC")

  foreach(d ${WRAP_ITK_DIMS})
    foreach(type ${wrap_image_types})
      WRAP_TEMPLATE("${ITKM_${type}}${d}"  "${ITKT_${type}},${d}")
    endforeach(type)
  endforeach(d)
END_WRAP_TYPE()
set(itk_Wrap_VectorImage ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::VariableLengthVector" "VLV")
  # Make a list of all of the selected image pixel types and also uchar
  # (for 8-bit image output)
  UNIQUE(wrap_image_types "${WRAP_ITK_SCALAR};UC")

  foreach(type ${wrap_image_types})
    WRAP_TEMPLATE("${ITKM_${type}}"  "${ITKT_${type}}")
  endforeach(type)
END_WRAP_TYPE()
set(itk_Wrap_VariableLengthVector ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::Point" "P")
  foreach(d ${WRAP_ITK_DIMS})
    WRAP_TEMPLATE("${ITKM_F}${d}"  "${ITKT_F},${d}")
    WRAP_TEMPLATE("${ITKM_D}${d}"  "${ITKT_D},${d}")
  endforeach(d)
END_WRAP_TYPE()
set(itk_Wrap_Point ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::LevelSetNode" "LSN")
  # Only make level set nodes for the selected image pixel types
  foreach(d ${WRAP_ITK_DIMS})
    foreach(type ${WRAP_ITK_SCALAR})
      WRAP_TEMPLATE("${ITKM_${type}}${d}"  "${ITKT_${type}},${d}")
    endforeach(type)
  endforeach(d)
END_WRAP_TYPE()
set(itk_Wrap_LevelSetNode ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::FlatStructuringElement" "SE")
  foreach(d ${WRAP_ITK_DIMS})
    WRAP_TEMPLATE("${d}"  "${d}")
  endforeach(d)
END_WRAP_TYPE()
set(itk_Wrap_StructuringElement ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::SpatialObject" "SO")
  foreach(d ${WRAP_ITK_DIMS})
    WRAP_TEMPLATE("${d}"  "${d}")
  endforeach(d)
END_WRAP_TYPE()
set(itk_Wrap_SpatialObject ${WRAPPER_TEMPLATES})

#------------------------------------------------------------------------------
