#------------------------------------------------------------------------------

# set the default include files for the generated wrapper files
# itk::Command has to be available in all ITK wrapped files
set(WRAPPER_DEFAULT_INCLUDE
  itkCommand.h
)

# define some macro to help creation of types vars

macro(WRAP_TYPE class prefix)
   # begin the creation of a type vars
   # call to this macro should be followed by one or several call to ADD_TEMPLATE()
   # and one call to END_WRAP_TYPE to really create the vars
   set(WRAPPER_TEMPLATES "")
   set(itk_Wrap_Prefix "${prefix}")
   set(itk_Wrap_Class "${class}")

   # Add any include's specified in ARGN to WRAPPER_DEFAULT_INCLUDE
   if(NOT "${ARGN}" STREQUAL "")
     list(APPEND WRAPPER_DEFAULT_INCLUDE ${ARGN})
   endif()
endmacro()

macro(END_WRAP_TYPE)
   # create the type vars.
   # must be called after END_WRAP_TYPE
   # Create the vars used to design types in all the cmake
   # files. This method ensure all the type names are constructed
   # with the same method.
   foreach(wrap ${WRAPPER_TEMPLATES})
      string(REGEX REPLACE "([0-9A-Za-z]*)[ ]*#[ ]*(.*)" "\\1" wrapTpl "${wrap}")
      string(REGEX REPLACE "([0-9A-Za-z]*)[ ]*#[ ]*(.*)" "\\2" wrapType "${wrap}")
      if("${itk_Wrap_Class}" MATCHES "::")
        # there's at least one namespace in the name
        string(REGEX REPLACE ".*::" "" base_name "${itk_Wrap_Class}")
        string(REGEX REPLACE "^([^:]*::)?.+" "\\1" top_namespace "${itk_Wrap_Class}")
        string(REGEX REPLACE "::" "" top_namespace "${top_namespace}") # drop the :: from the namespace
        set(swig_name "${top_namespace}${base_name}")
      else()
        # no namespaces
        set(swig_name "${itk_Wrap_Class}")
      endif()
      set(ITKT_${itk_Wrap_Prefix}${wrapTpl} "${itk_Wrap_Class}< ${wrapType} >")
      set(ITKM_${itk_Wrap_Prefix}${wrapTpl} "${itk_Wrap_Prefix}${wrapTpl}")
      set(ITKN_${itk_Wrap_Prefix}${wrapTpl} "${swig_name}${wrapTpl}")
   endforeach()
endmacro()

macro(ADD_TEMPLATE name types)
  list(APPEND WRAPPER_TEMPLATES "${name} # ${types}")
endmacro()


#------------------------------------------------------------------------------

# now, define types vars
# the result is stored in itk_Wrap_XXX where XXX is the name of the class
# to be wrapped in their own file, most of the time in Common.


WRAP_TYPE("itk::Offset" "O" "itkOffset.h")
  UNIQUE(dims "${ITK_WRAP_IMAGE_DIMS_INCREMENTED};1;2")
  foreach(d ${dims})
    ADD_TEMPLATE("${d}"  "${d}")
  endforeach()
END_WRAP_TYPE()
set(itk_Wrap_Offset ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::Vector" "V" "itkVector.h")
  # dim 6 is used by ScaleSkewVersor3DTransform
  UNIQUE(vector_dims "1;${ITK_WRAP_VECTOR_COMPONENTS_INCREMENTED};6")
  UNIQUE(vector_types "UC;F;D;${WRAP_ITK_SCALAR}")
  foreach(vector_dim ${vector_dims})
    foreach(t ${vector_types})
      ADD_TEMPLATE(
        "${ITKM_${t}}${vector_dim}"
        "${ITKT_${t}},${vector_dim}")
    endforeach()
  endforeach()
END_WRAP_TYPE()
set(itk_Wrap_Vector ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::CovariantVector" "CV" "itkCovariantVector.h")
  foreach(vector_dim ${ITK_WRAP_VECTOR_COMPONENTS_INCREMENTED})
    ADD_TEMPLATE(
      "${ITKM_F}${vector_dim}"
      "${ITKT_F},${vector_dim}")
    ADD_TEMPLATE(
      "${ITKM_D}${vector_dim}"
      "${ITKT_D},${vector_dim}")
  endforeach()
END_WRAP_TYPE()
set(itk_Wrap_CovariantVector ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::ContinuousIndex" "CI" "itkContinuousIndex.h")
foreach(d ${ITK_WRAP_IMAGE_DIMS_INCREMENTED})
    ADD_TEMPLATE("${ITKM_F}${d}"  "${ITKT_F},${d}")
    ADD_TEMPLATE("${ITKM_D}${d}"  "${ITKT_D},${d}")
  endforeach()
END_WRAP_TYPE()
set(itk_Wrap_ContinuousIndex ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::Array" "A" "itkArray.h")
  ADD_TEMPLATE("${ITKM_D}" "${ITKT_D}")
  ADD_TEMPLATE("${ITKM_F}" "${ITKT_F}")
  ADD_TEMPLATE("${ITKM_UL}" "${ITKT_UL}")
  ADD_TEMPLATE("${ITKM_SL}" "${ITKT_SL}")
  ADD_TEMPLATE("${ITKM_UI}" "${ITKT_UI}")
  if(WIN32 AND ITK_USE_64BITS_IDS)
    ADD_TEMPLATE("${ITKM_ULL}" "${ITKT_ULL}")
    ADD_TEMPLATE("${ITKM_SLL}" "${ITKT_SLL}")
  endif()
END_WRAP_TYPE()
set(itk_Wrap_Array ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::FixedArray" "FA" "itkFixedArray.h")

  set(dims ${ITK_WRAP_IMAGE_DIMS_INCREMENTED})
  foreach(d ${ITK_WRAP_IMAGE_DIMS})
    math(EXPR d2 "${d} * 2")
    # for itk::SymmetricSecondRankTensor
    math(EXPR d3 "${d} * (${d} + 1) / 2")
    list(APPEND dims ${d2} ${d3})
  endforeach()

  # FixedArray can be used with dimensions describing
  # image dimensions or vector components. Therefore it
  # needs to have defined sizes for:
  # - ITK_WRAP_IMAGE_DIMS_INCREMENTED
  # - ITK_WRAP_VECTOR_COMPONENTS_INCREMENTED
  # Dimensions 1;2;3;4;6 should always be wrapped.

  UNIQUE(array_sizes "${dims};1;2;3;4;6;${ITK_WRAP_VECTOR_COMPONENTS_INCREMENTED}")

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
  endforeach()
END_WRAP_TYPE()
set(itk_Wrap_FixedArray ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::RGBPixel" "RGB" "itkRGBPixel.h")

  # Required by InterpolateImageFunction
  ADD_TEMPLATE("${ITKM_D}" "${ITKT_D}")

  ADD_TEMPLATE("${ITKM_F}" "${ITKT_F}")

  ADD_TEMPLATE("${ITKM_UC}" "${ITKT_UC}")

  # Required by itkTIFFImageIO
  ADD_TEMPLATE("${ITKM_US}" "${ITKT_US}")

END_WRAP_TYPE()
set(itk_Wrap_RGBPixel ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::RGBAPixel" "RGBA" "itkRGBAPixel.h")

  # Required by InterpolateImageFunction
  ADD_TEMPLATE("${ITKM_D}" "${ITKT_D}")

  # Required by spatial objects
  ADD_TEMPLATE("${ITKM_F}" "${ITKT_F}")

  ADD_TEMPLATE("${ITKM_UC}" "${ITKT_UC}")

  if(ITK_WRAP_rgba_unsigned_short)
    ADD_TEMPLATE("${ITKM_US}" "${ITKT_US}")
  endif()

END_WRAP_TYPE()
set(itk_Wrap_RGBAPixel ${WRAPPER_TEMPLATES})

WRAP_TYPE("std::complex" "C" "complex")
  ADD_TEMPLATE("${ITKM_F}"  "${ITKT_F}")
  ADD_TEMPLATE("${ITKM_D}"  "${ITKT_D}")
END_WRAP_TYPE()
set(itk_Wrap_std_complex ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::SymmetricSecondRankTensor" "SSRT" "itkSymmetricSecondRankTensor.h")
  # itkDiffusionTensor3D always needs SymmetricSecondRankTensor with dim 2 and 3
  UNIQUE(image_dims "${ITK_WRAP_IMAGE_DIMS};2;3")
  foreach(d ${image_dims})
    ADD_TEMPLATE("${ITKM_D}${d}" "${ITKT_D}, ${d}")
    ADD_TEMPLATE("${ITKM_F}${d}" "${ITKT_F}, ${d}")
  endforeach()
END_WRAP_TYPE()
set(itk_Wrap_SymmetricSecondRankTensor ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::Image" "I" "itkImage.h")
  # Make a list of all of the selected image pixel types and also double (for
  # BSplineDeformableTransform), uchar (for 8-bit image output), ulong
  # (for the watershed and relabel filters), bool for (FlatStructuringElement)
  UNIQUE(wrap_image_types "${WRAP_ITK_ALL_TYPES};D;UC;UL;RGBUC;RGBAUC;VD;B;${ITKM_IT}")

  set(defined_vector_list )
  foreach(d ${ITK_WRAP_IMAGE_DIMS})

    foreach(type ${wrap_image_types})

      if("VF;VD;CVF;CVD" MATCHES "(^|;)${type}(;|$)")

        # Vectorial types
        set(orig_type ${type})
        foreach(vec_dim ${ITK_WRAP_VECTOR_COMPONENTS})
          set(type "${orig_type}${vec_dim}")
          ADD_TEMPLATE("${ITKM_${type}}${d}" "${ITKT_${type}},${d}")
          # Make a list of all defined vector/covariantvector image types.
          list(APPEND defined_vector_list ${ITKM_${type}}${d})
        endforeach()

      else()
        # Scalar types
        ADD_TEMPLATE("${ITKM_${type}}${d}" "${ITKT_${type}},${d}")
      endif()
    endforeach()

    # FixedArray types required by level set filters
    if(ITK_WRAP_float)
      ADD_TEMPLATE("${ITKM_FAF${d}}${d}"  "${ITKT_FAF${d}},${d}")
    endif()
    if(ITK_WRAP_double)
      ADD_TEMPLATE("${ITKM_FAD${d}}${d}"  "${ITKT_FAD${d}},${d}")
    endif()

    # Offset, used by Danielsson's filter
    ADD_TEMPLATE("${ITKM_O${d}}${d}"  "${ITKT_O${d}},${d}")

    # SymmetricSecondRankTensor types required by level set filters
    ADD_TEMPLATE("${ITKM_SSRT${ITKM_D}${d}}${d}"  "${ITKT_SSRT${ITKM_D}${d}}, ${d}")

  endforeach()

  # The next templates need to be always present, but should not be
  # defined two times; so we check if they are already defined or not
  # using the defined_vector_list.

  # Vector types required by VelocityFieldTranform classes.
  foreach(d ${ITK_WRAP_IMAGE_DIMS})
    INCREMENT(d_inc ${d})
    foreach(vector_dim ${ITK_WRAP_VECTOR_COMPONENTS})
      list(FIND defined_vector_list "${ITKM_VD${vector_dim}}${d_inc}" index)
      if(index EQUAL -1)
        ADD_TEMPLATE("${ITKM_VD${vector_dim}}${d_inc}" "${ITKT_VD${vector_dim}},${d_inc}")
      endif()
    endforeach()
    # For N4BiasFieldCorrectionImageFilter
    list(FIND defined_vector_list "${ITKM_VF1}${d}" index)
    if(index EQUAL -1)
      ADD_TEMPLATE("${ITKM_VF1}${d}" "${ITKT_VF1},${d}")
    endif()
  endforeach()

  # CovariantVector types required by ImageToImageMetric class
  # for the ITKRegistration module.
  foreach(d ${ITK_WRAP_IMAGE_DIMS})
    foreach(vector_dim ${ITK_WRAP_VECTOR_COMPONENTS})
      list(FIND defined_vector_list "${ITKM_CVD${vector_dim}}${d}" index)
      if(index EQUAL -1)
        ADD_TEMPLATE("${ITKM_CVD${vector_dim}}${d}" "${ITKT_CVD${vector_dim}},${d}")
      endif()
    endforeach()
  endforeach()

END_WRAP_TYPE()
set(itk_Wrap_Image ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::VectorImage" "VI" "itkVectorImage.h")
  # Make a list of all of the selected image pixel types and also uchar
  # (for 8-bit image output)
  UNIQUE(wrap_image_types "${WRAP_ITK_COMPLEX_REAL};${WRAP_ITK_SCALAR};UC")

  foreach(d ${ITK_WRAP_IMAGE_DIMS})
    foreach(type ${wrap_image_types})
      ADD_TEMPLATE("${ITKM_${type}}${d}"  "${ITKT_${type}},${d}")
    endforeach()
  endforeach()
END_WRAP_TYPE()
set(itk_Wrap_VectorImage ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::VariableLengthVector" "VLV" "itkVariableLengthVector.h")
  # Make a list of all of the selected image pixel types and also uchar
  # (for 8-bit image output)
  UNIQUE(wrap_image_types "${WRAP_ITK_COMPLEX_REAL};${WRAP_ITK_SCALAR};UC;D")

  foreach(type ${wrap_image_types})
    ADD_TEMPLATE("${ITKM_${type}}"  "${ITKT_${type}}")
  endforeach()
END_WRAP_TYPE()
set(itk_Wrap_VariableLengthVector ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::Point" "P" "itkPoint.h")
  foreach(d ${ITK_WRAP_IMAGE_DIMS_INCREMENTED})
    ADD_TEMPLATE("${ITKM_F}${d}"  "${ITKT_F},${d}")
    ADD_TEMPLATE("${ITKM_D}${d}"  "${ITKT_D},${d}")
  endforeach()
END_WRAP_TYPE()
set(itk_Wrap_Point ${WRAPPER_TEMPLATES})

# Needed by Modules/Filtering/FastMarching/wrapping/itkLevelSetNode.wrap
WRAP_TYPE("itk::LevelSetNode" "LSN")
  # Only make level set nodes for the selected image pixel types
  foreach(d ${ITK_WRAP_IMAGE_DIMS})
    foreach(type ${WRAP_ITK_SCALAR})
      ADD_TEMPLATE("${ITKM_${type}}${d}"  "${ITKT_${type}},${d}")
    endforeach()
  endforeach()
END_WRAP_TYPE()
set(itk_Wrap_LevelSetNode ${WRAPPER_TEMPLATES})

# Needed by Modules/Filtering/MathematicalMorphology/wrapping/itkFlatStructuringElement.wrap
WRAP_TYPE("itk::FlatStructuringElement" "SE")
  foreach(d ${ITK_WRAP_IMAGE_DIMS})
    ADD_TEMPLATE("${d}"  "${d}")
  endforeach()
END_WRAP_TYPE()
set(itk_Wrap_StructuringElement ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::SpatialObject" "SO")
  foreach(d ${ITK_WRAP_IMAGE_DIMS})
    ADD_TEMPLATE("${d}"  "${d}")
  endforeach()
END_WRAP_TYPE()
set(itk_Wrap_SpatialObject ${WRAPPER_TEMPLATES})

WRAP_TYPE("itk::Statistics::Histogram" "H")
    ADD_TEMPLATE("${ITKM_F}"  "${ITKT_F}")
    ADD_TEMPLATE("${ITKM_D}"  "${ITKT_D}")
END_WRAP_TYPE()
set(itk_Wrap_Histogram ${WRAPPER_TEMPLATES})

# Needed by Modules/Filtering/LabelMap/wrapping/ITKLabelMapBase.wrap
WRAP_TYPE("itk::LabelMap" "LM")
  foreach(d ${ITK_WRAP_IMAGE_DIMS})
    ADD_TEMPLATE("${d}" "itk::StatisticsLabelObject< ${ITKT_UL}, ${d} >")
endforeach()
END_WRAP_TYPE()
set(itk_Wrap_LabelMap ${WRAPPER_TEMPLATES})

#------------------------------------------------------------------------------
