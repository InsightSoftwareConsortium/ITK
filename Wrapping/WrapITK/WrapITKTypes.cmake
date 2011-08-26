#------------------------------------------------------------------------------

# set the default include files for the generated wrapper files
# itk::Command has to be available in all ITK wrapped files
set(WRAPPER_DEFAULT_INCLUDE
  itkCommand.h
  itkStatisticsLabelObject.h
)

# define some macro to help creation of types vars

macro(WRAP_TYPE class prefix)
   # begin the creation of a type vars
   # call to this macro should be followed by one or several call to ADD_TEMPLATE()
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
      if("${itk_Wrap_Class}" MATCHES "::")
        # there's at least one namespace in the name
        string(REGEX REPLACE ".*::" "" base_name "${itk_Wrap_Class}")
        string(REGEX REPLACE "^([^:]*::)?.+" "\\1" top_namespace "${itk_Wrap_Class}")
        string(REGEX REPLACE "::" "" top_namespace "${top_namespace}") # drop the :: from the namespace
        set(swig_name "${top_namespace}${base_name}")
      else("${itk_Wrap_Class}" MATCHES "::")
        # no namespaces
        set(swig_name "${itk_Wrap_Class}")
      endif("${itk_Wrap_Class}" MATCHES "::")
      set(ITKT_${itk_Wrap_Prefix}${wrapTpl} "${itk_Wrap_Class}< ${wrapType} >")
      set(ITKM_${itk_Wrap_Prefix}${wrapTpl} "${itk_Wrap_Prefix}${wrapTpl}")
      set(ITKN_${itk_Wrap_Prefix}${wrapTpl} "${swig_name}${wrapTpl}")
   endforeach(wrap)
endmacro(END_WRAP_TYPE)

macro(ADD_TEMPLATE name types)
  set(WRAPPER_TEMPLATES ${WRAPPER_TEMPLATES} "${name} # ${types}")
endmacro(ADD_TEMPLATE)


#------------------------------------------------------------------------------

# now, define types vars
# the result is stored in itk_Wrap_XXX where XXX is the name of the class
# to be wrapped in there own file, most of the time in CommonA


WRAP_TYPE("itk::Offset" "O")
  UNIQUE(dims "${WRAP_ITK_DIMS};1;2")
  foreach(d ${dims})
    ADD_TEMPLATE("${d}"  "${d}")
  endforeach(d)
END_WRAP_TYPE()
set(itk_Wrap_Offset ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::Vector" "V")
  # dim 6 is used by ScaleSkewVersor3DTransform
  UNIQUE(vector_sizes "1;${WRAP_ITK_DIMS};6")
  UNIQUE(vector_types "UC;F;D;${WRAP_ITK_SCALAR}")
  foreach(d ${vector_sizes})
    foreach(t ${vector_types})
      ADD_TEMPLATE("${ITKM_${t}}${d}"  "${ITKT_${t}},${d}")
    endforeach(t)
  endforeach(d)
END_WRAP_TYPE()
set(itk_Wrap_Vector ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::CovariantVector" "CV")
  foreach(d ${WRAP_ITK_DIMS})
    ADD_TEMPLATE("${ITKM_F}${d}"  "${ITKT_F},${d}")
    ADD_TEMPLATE("${ITKM_D}${d}"  "${ITKT_D},${d}")
  endforeach(d)
END_WRAP_TYPE()
set(itk_Wrap_CovariantVector ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::ContinuousIndex" "CI")
  foreach(d ${WRAP_ITK_DIMS})
    ADD_TEMPLATE("${ITKM_F}${d}"  "${ITKT_F},${d}")
    ADD_TEMPLATE("${ITKM_D}${d}"  "${ITKT_D},${d}")
  endforeach(d)
END_WRAP_TYPE()
set(itk_Wrap_ContinuousIndex ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::Array" "A")
  ADD_TEMPLATE("${ITKM_D}" "${ITKT_D}")
  ADD_TEMPLATE("${ITKM_F}" "${ITKT_F}")
  ADD_TEMPLATE("${ITKM_UL}" "${ITKT_UL}")
  ADD_TEMPLATE("${ITKM_SL}" "${ITKT_SL}")
END_WRAP_TYPE()
set(itk_Wrap_Array ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::FixedArray" "FA")
  set(dims ${WRAP_ITK_DIMS})
  foreach(d ${WRAP_ITK_DIMS})
    math(EXPR d2 "${d} * 2")
    # for itk::SymmetricSecondRankTensor
    math(EXPR d3 "${d} * (${d} + 1) / 2")
    set(dims ${dims} ${d2} ${d3})
  endforeach(d)
  UNIQUE(array_sizes "${dims};1;2;3;4;6")
  # make sure that 1-D FixedArrays are wrapped. Also wrap for each selected
  # image dimension.
  # 3-D FixedArrays are required as superclass of rgb pixels
  # TODO: Do we need fixed arrays for all of these types? For just the selected
  # pixel types plus some few basic cases? Or just for a basic set of types?
  foreach(d ${array_sizes})
    ADD_TEMPLATE("${ITKM_D}${d}"  "${ITKT_D},${d}")
    ADD_TEMPLATE("${ITKM_F}${d}"  "${ITKT_F},${d}")
    ADD_TEMPLATE("${ITKM_UL}${d}" "${ITKT_UL},${d}")
    ADD_TEMPLATE("${ITKM_US}${d}" "${ITKT_US},${d}")
    ADD_TEMPLATE("${ITKM_UC}${d}" "${ITKT_UC},${d}")
    ADD_TEMPLATE("${ITKM_UI}${d}" "${ITKT_UI},${d}")
    ADD_TEMPLATE("${ITKM_SL}${d}" "${ITKT_SL},${d}")
    ADD_TEMPLATE("${ITKM_SS}${d}" "${ITKT_SS},${d}")
    ADD_TEMPLATE("${ITKM_SC}${d}" "${ITKT_SC},${d}")
    ADD_TEMPLATE("${ITKM_B}${d}"  "${ITKT_B},${d}")
  endforeach(d)
END_WRAP_TYPE()
set(itk_Wrap_FixedArray ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::RGBPixel" "RGB")
  ADD_TEMPLATE("${ITKM_UC}" "${ITKT_UC}")

  if(WRAP_rgb_unsigned_short)
    ADD_TEMPLATE("${ITKM_US}" "${ITKT_US}")
  endif(WRAP_rgb_unsigned_short)
END_WRAP_TYPE()
set(itk_Wrap_RGBPixel ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::RGBAPixel" "RGBA")
  ADD_TEMPLATE("${ITKM_UC}" "${ITKT_UC}")

  if(WRAP_rgba_unsigned_short)
     ADD_TEMPLATE("${ITKM_US}" "${ITKT_US}")
  endif(WRAP_rgba_unsigned_short)

  # required by spatial objects
  ADD_TEMPLATE("${ITKM_F}" "${ITKT_F}")
END_WRAP_TYPE()
set(itk_Wrap_RGBAPixel ${WRAPPER_TEMPLATES})

WRAP_TYPE("std::complex" "C")
  if(WRAP_complex_float)
    ADD_TEMPLATE("${ITKM_F}"  "${ITKT_F}")
  endif(WRAP_complex_float)
  if(WRAP_complex_double)
    ADD_TEMPLATE("${ITKM_D}"  "${ITKT_D}")
  endif(WRAP_complex_double)
END_WRAP_TYPE()
set(itk_Wrap_vcl_complex ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::SymmetricSecondRankTensor" "SSRT")
  foreach(d ${WRAP_ITK_DIMS})
    ADD_TEMPLATE("${ITKM_D}${d}" "${ITKT_D}, ${d}")
    ADD_TEMPLATE("${ITKM_F}${d}" "${ITKT_F}, ${d}")
  endforeach(d)
END_WRAP_TYPE()
set(itk_Wrap_SymmetricSecondRankTensor ${WRAPPER_TEMPLATES})

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

      ADD_TEMPLATE("${ITKM_${type}}${d}"  "${ITKT_${type}},${d}")
    endforeach(type)

    # FixedArray types required by level set filters
    ADD_TEMPLATE("${ITKM_FAF${d}}${d}"  "${ITKT_FAF${d}},${d}")

    # Offset, used by Danielsson's filter
    ADD_TEMPLATE("${ITKM_O${d}}${d}"  "${ITKT_O${d}},${d}")

    # SymmetricSecondRankTensor types required by level set filters
    ADD_TEMPLATE("${ITKM_SSRT${ITKM_D}${d}}${d}"  "${ITKT_SSRT${ITKM_D}${d}}, ${d}")

  endforeach(d)
END_WRAP_TYPE()
set(itk_Wrap_Image ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::VectorImage" "VI")
  # Make a list of all of the selected image pixel types and also uchar
  # (for 8-bit image output)
  UNIQUE(wrap_image_types "${WRAP_ITK_SCALAR};UC")

  foreach(d ${WRAP_ITK_DIMS})
    foreach(type ${wrap_image_types})
      ADD_TEMPLATE("${ITKM_${type}}${d}"  "${ITKT_${type}},${d}")
    endforeach(type)
  endforeach(d)
END_WRAP_TYPE()
set(itk_Wrap_VectorImage ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::VariableLengthVector" "VLV")
  # Make a list of all of the selected image pixel types and also uchar
  # (for 8-bit image output)
  UNIQUE(wrap_image_types "${WRAP_ITK_SCALAR};UC;D")

  foreach(type ${wrap_image_types})
    ADD_TEMPLATE("${ITKM_${type}}"  "${ITKT_${type}}")
  endforeach(type)
END_WRAP_TYPE()
set(itk_Wrap_VariableLengthVector ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::Point" "P")
  foreach(d ${WRAP_ITK_DIMS})
    ADD_TEMPLATE("${ITKM_F}${d}"  "${ITKT_F},${d}")
    ADD_TEMPLATE("${ITKM_D}${d}"  "${ITKT_D},${d}")
  endforeach(d)
END_WRAP_TYPE()
set(itk_Wrap_Point ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::LevelSetNode" "LSN")
  # Only make level set nodes for the selected image pixel types
  foreach(d ${WRAP_ITK_DIMS})
    foreach(type ${WRAP_ITK_SCALAR})
      ADD_TEMPLATE("${ITKM_${type}}${d}"  "${ITKT_${type}},${d}")
    endforeach(type)
  endforeach(d)
END_WRAP_TYPE()
set(itk_Wrap_LevelSetNode ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::FlatStructuringElement" "SE")
  foreach(d ${WRAP_ITK_DIMS})
    ADD_TEMPLATE("${d}"  "${d}")
  endforeach(d)
END_WRAP_TYPE()
set(itk_Wrap_StructuringElement ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::SpatialObject" "SO")
  foreach(d ${WRAP_ITK_DIMS})
    ADD_TEMPLATE("${d}"  "${d}")
  endforeach(d)
END_WRAP_TYPE()
set(itk_Wrap_SpatialObject ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::Statistics::Histogram" "H")
    ADD_TEMPLATE("${ITKM_F}"  "${ITKT_F}")
    ADD_TEMPLATE("${ITKM_D}"  "${ITKT_D}")
END_WRAP_TYPE()
set(itk_Wrap_Histogram ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::LabelMap" "LM")
  foreach(d ${WRAP_ITK_DIMS})
    ADD_TEMPLATE("${d}" "itk::StatisticsLabelObject< ${ITKT_UL}, ${d} >")
endforeach(d)
END_WRAP_TYPE()
set(itk_Wrap_LabelMap ${WRAPPER_TEMPLATES})

#------------------------------------------------------------------------------
