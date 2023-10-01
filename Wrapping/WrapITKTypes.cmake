#------------------------------------------------------------------------------

# set the default include files for the generated wrapper files
# itk::Command has to be available in all ITK wrapped files
set(WRAPPER_DEFAULT_INCLUDE itkCommand.h)

# define some macro to help creation of types vars

macro(WRAP_TYPE class prefix)
  # begin the creation of a type vars
  # call to this macro should be followed by one or several call to ADD_TEMPLATE()
  # and one call to END_WRAP_TYPE to really create the vars
  set(WRAPPER_TEMPLATES "")
  set(itk_Wrap_Prefix "${prefix}")
  set(itk_Wrap_Class "${class}")

  # Add any include's specified in ARGN to WRAPPER_DEFAULT_INCLUDE
  if(NOT
     "${ARGN}"
     STREQUAL
     "")
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
    string(
      REGEX
      REPLACE "([0-9A-Za-z]*)[ ]*#[ ]*(.*)"
              "\\1"
              wrapTpl
              "${wrap}")
    string(
      REGEX
      REPLACE "([0-9A-Za-z]*)[ ]*#[ ]*(.*)"
              "\\2"
              wrapType
              "${wrap}")
    if("${itk_Wrap_Class}" MATCHES "::")
      # there's at least one namespace in the name
      string(
        REGEX
        REPLACE ".*::"
                ""
                base_name
                "${itk_Wrap_Class}")
      string(
        REGEX
        REPLACE "^([^:]*::)?.+"
                "\\1"
                top_namespace
                "${itk_Wrap_Class}")
      string(
        REGEX
        REPLACE "::"
                ""
                top_namespace
                "${top_namespace}") # drop the :: from the namespace
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

wrap_type("itk::Offset" "O" "itkOffset.h")
unique(dims "${ITK_WRAP_IMAGE_DIMS_INCREMENTED};1;2")
foreach(d ${dims})
  add_template("${d}" "${d}")
endforeach()
end_wrap_type()
set(itk_Wrap_Offset ${WRAPPER_TEMPLATES})

wrap_type("itk::Vector" "V" "itkVector.h")
# dim 6 is used by ScaleSkewVersor3DTransform
unique(vector_dims "1;${ITK_WRAP_VECTOR_COMPONENTS_INCREMENTED};6")
unique(vector_types "UC;F;D;${WRAP_ITK_SCALAR}")
foreach(vector_dim ${vector_dims})
  foreach(t ${vector_types})
    add_template("${ITKM_${t}}${vector_dim}" "${ITKT_${t}},${vector_dim}")
  endforeach()
endforeach()
end_wrap_type()
set(itk_Wrap_Vector ${WRAPPER_TEMPLATES})

wrap_type("itk::CovariantVector" "CV" "itkCovariantVector.h")
foreach(vector_dim ${ITK_WRAP_VECTOR_COMPONENTS_INCREMENTED})
  add_template("${ITKM_F}${vector_dim}" "${ITKT_F},${vector_dim}")
  add_template("${ITKM_D}${vector_dim}" "${ITKT_D},${vector_dim}")
endforeach()
end_wrap_type()
set(itk_Wrap_CovariantVector ${WRAPPER_TEMPLATES})

wrap_type("itk::ContinuousIndex" "CI" "itkContinuousIndex.h")
foreach(d ${ITK_WRAP_IMAGE_DIMS_INCREMENTED})
  add_template("${ITKM_F}${d}" "${ITKT_F},${d}")
  add_template("${ITKM_D}${d}" "${ITKT_D},${d}")
endforeach()
end_wrap_type()
set(itk_Wrap_ContinuousIndex ${WRAPPER_TEMPLATES})

wrap_type("itk::Array" "A" "itkArray.h")
add_template("${ITKM_D}" "${ITKT_D}")
add_template("${ITKM_F}" "${ITKT_F}")
add_template("${ITKM_UL}" "${ITKT_UL}")
add_template("${ITKM_ULL}" "${ITKT_ULL}")
add_template("${ITKM_SL}" "${ITKT_SL}")
add_template("${ITKM_SLL}" "${ITKT_SLL}")
add_template("${ITKM_UI}" "${ITKT_UI}")
end_wrap_type()
set(itk_Wrap_Array ${WRAPPER_TEMPLATES})

wrap_type("itk::FixedArray" "FA" "itkFixedArray.h")

set(dims ${ITK_WRAP_IMAGE_DIMS_INCREMENTED})
foreach(d ${ITK_WRAP_IMAGE_DIMS})
  math(EXPR d2 "${d} * 2")
  # for itk::SymmetricSecondRankTensor
  math(EXPR d3 "${d} * (${d} + 1) / 2")
  list(
    APPEND
    dims
    ${d2}
    ${d3})
endforeach()

# FixedArray can be used with dimensions describing
# image dimensions or vector components. Therefore it
# needs to have defined sizes for:
# - ITK_WRAP_IMAGE_DIMS_INCREMENTED
# - ITK_WRAP_VECTOR_COMPONENTS_INCREMENTED
# Dimensions 1;2;3;4;6 should always be wrapped.

unique(array_sizes "${dims};1;2;3;4;6;${ITK_WRAP_VECTOR_COMPONENTS_INCREMENTED}")

# 3-D FixedArrays are required as superclass of rgb pixels
# TODO: Do we need fixed arrays for all of these types? For just the selected
# pixel types plus some few basic cases? Or just for a basic set of types?
foreach(d ${array_sizes})
  add_template("${ITKM_D}${d}" "${ITKT_D},${d}")
  add_template("${ITKM_F}${d}" "${ITKT_F},${d}")
  add_template("${ITKM_UL}${d}" "${ITKT_UL},${d}")
  add_template("${ITKM_ULL}${d}" "${ITKT_ULL},${d}")
  add_template("${ITKM_US}${d}" "${ITKT_US},${d}")
  add_template("${ITKM_UC}${d}" "${ITKT_UC},${d}")
  add_template("${ITKM_UI}${d}" "${ITKT_UI},${d}")
  add_template("${ITKM_SL}${d}" "${ITKT_SL},${d}")
  add_template("${ITKM_SLL}${d}" "${ITKT_SLL},${d}")
  add_template("${ITKM_SS}${d}" "${ITKT_SS},${d}")
  add_template("${ITKM_SC}${d}" "${ITKT_SC},${d}")
  add_template("${ITKM_B}${d}" "${ITKT_B},${d}")
endforeach()

# Wrap FixedArray for BSplineInterpolationWeightFunction:
foreach(d ${ITK_WRAP_IMAGE_DIMS})
  # Wrapping for spline order 3, components = (SplineOrder + 1)^SpaceDimension
  set(comp 1)
  foreach(i RANGE 1 ${d})
    math(EXPR comp "${comp}*4")
  endforeach()
  add_template("${ITKM_D}${comp}" "${ITKT_D},${comp}")
  add_template("${ITKM_UL}${comp}" "${ITKT_UL},${comp}")
endforeach()
end_wrap_type()
set(itk_Wrap_FixedArray ${WRAPPER_TEMPLATES})

wrap_type("itk::RGBPixel" "RGB" "itkRGBPixel.h")

# Required by InterpolateImageFunction
add_template("${ITKM_D}" "${ITKT_D}")

add_template("${ITKM_F}" "${ITKT_F}")

add_template("${ITKM_UC}" "${ITKT_UC}")

# Required by itkTIFFImageIO
add_template("${ITKM_US}" "${ITKT_US}")

end_wrap_type()
set(itk_Wrap_RGBPixel ${WRAPPER_TEMPLATES})

wrap_type("itk::RGBAPixel" "RGBA" "itkRGBAPixel.h")

# Required by InterpolateImageFunction
add_template("${ITKM_D}" "${ITKT_D}")

# Required by spatial objects
add_template("${ITKM_F}" "${ITKT_F}")

add_template("${ITKM_UC}" "${ITKT_UC}")

if(ITK_WRAP_rgba_unsigned_short)
  add_template("${ITKM_US}" "${ITKT_US}")
endif()

end_wrap_type()
set(itk_Wrap_RGBAPixel ${WRAPPER_TEMPLATES})

wrap_type("std::complex" "C" "complex")
add_template("${ITKM_F}" "${ITKT_F}")
add_template("${ITKM_D}" "${ITKT_D}")
end_wrap_type()
set(itk_Wrap_std_complex ${WRAPPER_TEMPLATES})

wrap_type("itk::SymmetricSecondRankTensor" "SSRT" "itkSymmetricSecondRankTensor.h")
# itkDiffusionTensor3D always needs SymmetricSecondRankTensor with dim 2 and 3
unique(image_dims "${ITK_WRAP_IMAGE_DIMS};2;3")
foreach(d ${image_dims})
  add_template("${ITKM_D}${d}" "${ITKT_D}, ${d}")
  add_template("${ITKM_F}${d}" "${ITKT_F}, ${d}")
endforeach()
end_wrap_type()
set(itk_Wrap_SymmetricSecondRankTensor ${WRAPPER_TEMPLATES})

wrap_type("itk::Image" "I" "itkImage.h")
# Make a list of all of the selected image pixel types and also double (for
# BSplineDeformableTransform), uchar (for 8-bit image output), ulong
# (for the watershed and relabel filters), bool for (FlatStructuringElement)
# unsigned int and signed int for IO.

# Wrap from ulong to other integral types, even if ulong isn't wrapped. This
# is needed for the relabel components image filter.
unique(WRAP_ITK_SCALAR_IMAGE_PIXEL_TYPES "${WRAP_ITK_SCALAR};D;UC;SI;UI;UL;ULL;B;${ITKM_IT}")
unique(wrap_image_types "${WRAP_ITK_ALL_TYPES};RGBUC;RGBAUC;VD;${WRAP_ITK_SCALAR_IMAGE_PIXEL_TYPES}")

set(defined_vector_list)
foreach(d ${ITK_WRAP_IMAGE_DIMS})

  foreach(type ${wrap_image_types})

    if("VF;VD;CVF;CVD" MATCHES "(^|;)${type}(;|$)")

      # Vectorial types
      set(orig_type ${type})
      foreach(vec_dim ${ITK_WRAP_VECTOR_COMPONENTS})
        set(type "${orig_type}${vec_dim}")
        add_template("${ITKM_${type}}${d}" "${ITKT_${type}},${d}")
        # Make a list of all defined vector/covariantvector image types.
        list(APPEND defined_vector_list ${ITKM_${type}}${d})
      endforeach()

    else()
      # Scalar types
      add_template("${ITKM_${type}}${d}" "${ITKT_${type}},${d}")
    endif()
  endforeach()

  # FixedArray types required by level set filters
  if(ITK_WRAP_float)
    add_template("${ITKM_FAF${d}}${d}" "${ITKT_FAF${d}},${d}")
  endif()
  if(ITK_WRAP_double)
    add_template("${ITKM_FAD${d}}${d}" "${ITKT_FAD${d}},${d}")
  endif()

  # Offset, used by Danielsson's filter
  add_template("${ITKM_O${d}}${d}" "${ITKT_O${d}},${d}")

  # SymmetricSecondRankTensor types required by level set filters
  add_template("${ITKM_SSRT${ITKM_D}${d}}${d}" "${ITKT_SSRT${ITKM_D}${d}}, ${d}")
  if(ITK_WRAP_float)
    add_template("${ITKM_SSRT${ITKM_F}${d}}${d}" "${ITKT_SSRT${ITKM_F}${d}}, ${d}")
  endif()

endforeach()

# The next templates need to be always present, but should not be
# defined two times; so we check if they are already defined or not
# using the defined_vector_list.

# Vector types required by VelocityFieldTranform classes.
foreach(d ${ITK_WRAP_IMAGE_DIMS})
  increment(d_inc ${d})
  foreach(vector_dim ${ITK_WRAP_VECTOR_COMPONENTS})
    if(NOT
       "${ITKM_VD${vector_dim}}${d_inc}"
       IN_LIST
       defined_vector_list)
      add_template("${ITKM_VD${vector_dim}}${d_inc}" "${ITKT_VD${vector_dim}},${d_inc}")
    endif()
  endforeach()
  # For N4BiasFieldCorrectionImageFilter
  if(NOT
     "${ITKM_VF1}${d}"
     IN_LIST
     defined_vector_list)
    add_template("${ITKM_VF1}${d}" "${ITKT_VF1},${d}")
  endif()
endforeach()

# CovariantVector types required by ImageToImageMetric class
# for the ITKRegistration module.
foreach(d ${ITK_WRAP_IMAGE_DIMS})
  foreach(vector_dim ${ITK_WRAP_VECTOR_COMPONENTS})
    if(NOT
       "${ITKM_CVD${vector_dim}}${d}"
       IN_LIST
       defined_vector_list)
      add_template("${ITKM_CVD${vector_dim}}${d}" "${ITKT_CVD${vector_dim}},${d}")
    endif()
  endforeach()
endforeach()

end_wrap_type()
set(itk_Wrap_Image ${WRAPPER_TEMPLATES})

wrap_type("itk::VectorImage" "VI" "itkVectorImage.h")
# Make a list of all of the selected image pixel types and also uchar
# (for 8-bit image output)
unique(wrap_image_types "${WRAP_ITK_COMPLEX_REAL};${WRAP_ITK_SCALAR};UC;D")

foreach(d ${ITK_WRAP_IMAGE_DIMS})
  foreach(type ${wrap_image_types})
    add_template("${ITKM_${type}}${d}" "${ITKT_${type}},${d}")
  endforeach()
endforeach()
end_wrap_type()
set(itk_Wrap_VectorImage ${WRAPPER_TEMPLATES})

wrap_type("itk::VariableLengthVector" "VLV" "itkVariableLengthVector.h")
# Make a list of all of the selected image pixel types and also uchar
# (for 8-bit image output)
unique(wrap_image_types "${WRAP_ITK_COMPLEX_REAL};${WRAP_ITK_SCALAR};UC;D")

foreach(type ${wrap_image_types})
  add_template("${ITKM_${type}}" "${ITKT_${type}}")
endforeach()
end_wrap_type()
set(itk_Wrap_VariableLengthVector ${WRAPPER_TEMPLATES})

wrap_type("itk::Point" "P" "itkPoint.h")
unique(dims "${ITK_WRAP_IMAGE_DIMS_INCREMENTED};6")
foreach(d ${dims})
  add_template("${ITKM_F}${d}" "${ITKT_F},${d}")
  add_template("${ITKM_D}${d}" "${ITKT_D},${d}")
endforeach()
end_wrap_type()
set(itk_Wrap_Point ${WRAPPER_TEMPLATES})

# Needed by Modules/Filtering/FastMarching/wrapping/itkLevelSetNode.wrap
wrap_type("itk::LevelSetNode" "LSN")
# Only make level set nodes for the selected image pixel types
foreach(d ${ITK_WRAP_IMAGE_DIMS})
  foreach(type ${WRAP_ITK_SCALAR})
    add_template("${ITKM_${type}}${d}" "${ITKT_${type}},${d}")
  endforeach()
endforeach()
end_wrap_type()
set(itk_Wrap_LevelSetNode ${WRAPPER_TEMPLATES})

# Needed by Modules/Filtering/MathematicalMorphology/wrapping/itkFlatStructuringElement.wrap
wrap_type("itk::FlatStructuringElement" "SE")
foreach(d ${ITK_WRAP_IMAGE_DIMS})
  add_template("${d}" "${d}")
endforeach()
end_wrap_type()
set(itk_Wrap_StructuringElement ${WRAPPER_TEMPLATES})

wrap_type("itk::SpatialObject" "SO")
foreach(d ${ITK_WRAP_IMAGE_DIMS})
  add_template("${d}" "${d}")
endforeach()
end_wrap_type()
set(itk_Wrap_SpatialObject ${WRAPPER_TEMPLATES})

wrap_type("itk::Statistics::Histogram" "H")
add_template("${ITKM_F}" "${ITKT_F}")
add_template("${ITKM_D}" "${ITKT_D}")
end_wrap_type()
set(itk_Wrap_Histogram ${WRAPPER_TEMPLATES})

# Needed by Modules/Filtering/LabelMap/wrapping/ITKLabelMapBase.wrap
wrap_type("itk::LabelMap" "LM")
foreach(d ${ITK_WRAP_IMAGE_DIMS})
  add_template("${d}" "itk::StatisticsLabelObject< ${ITKT_UL}, ${d} >")
endforeach()
end_wrap_type()
set(itk_Wrap_LabelMap ${WRAPPER_TEMPLATES})

#------------------------------------------------------------------------------
