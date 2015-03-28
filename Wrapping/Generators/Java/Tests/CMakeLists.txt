
# configure the test driver

# windows doesn't use the same separator than the others
if(WIN32)
  set(sep "\\\;")
else()
  set(sep ":")
endif()

# put all generated files in this list
set(targets "")
# the required libs
set(depends "")

#-----------------
#
#   MACROS
#
#----------------


#----------------
macro(build_classpath var)
  set(${var} ".")
  set(LAST_TEST_DEPS_OK ON)
  foreach(lib ${ARGN})
    string(TOLOWER ${lib} lower_lib)
    set(${var} "${${var}}${sep}${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/org.itk.${lower_lib}.jar")
    list(APPEND depends ${lib}JavaJar)
    if(NOT "${WRAP_ITK_MODULES}" MATCHES "(^|;)${lib}(;|$)")
      set(LAST_TEST_DEPS_OK OFF)
    endif()
  endforeach()
endmacro()


#----------------
macro(build_java_test name cp_var)
  # a convenient macro to build java tests
  #
  build_classpath(${cp_var} ${ARGN})
  if(LAST_TEST_DEPS_OK)
    add_custom_command(
      OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${name}.class
      COMMAND ${Java_JAVAC_EXECUTABLE}
      ARGS -classpath "${${cp_var}}"
      -d "${CMAKE_CURRENT_BINARY_DIR}" ${CMAKE_CURRENT_SOURCE_DIR}/${name}.java
      DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${name}.java
      )
    list(APPEND targets ${CMAKE_CURRENT_BINARY_DIR}/${name}.class)
  else()
    message("Warning: Can't buid java test ${name}.")
  endif()
endmacro()


#----------------
macro(build_java_test_bin name cp_var)
  # a convenient macro to build java tests
  #
  build_classpath(${cp_var} ${ARGN})
  if(LAST_TEST_DEPS_OK)
    add_custom_command(
      OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${name}.class
      COMMAND ${Java_JAVAC_EXECUTABLE}
      ARGS -classpath "${${cp_var}}"
      -d "${CMAKE_CURRENT_BINARY_DIR}" ${CMAKE_CURRENT_BINARY_DIR}/${name}.java
      DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${name}.java
      )
    list(APPEND targets ${CMAKE_CURRENT_BINARY_DIR}/${name}.class)
  else()
    message("Warning: Can't buid java test ${name}.")
  endif()
endmacro()


#----------------
macro(add_java_test name classpath)
  set(command "${Java_JAVA_EXECUTABLE}")

  # add extra command which may be needed on some systems
  if(CMAKE_OSX_ARCHITECTURES)
    list(GET CMAKE_OSX_ARCHITECTURES 0 test_arch)
    set(command arch -${test_arch} ${command})
    if(${test_arch} STREQUAL "i386" OR ${test_arch} STREQUAL "ppc")
      list(APPEND command -d32)
    else()
      if(${test_arch} STREQUAL "x86_64" OR ${test_arch} STREQUAL "ppc64")
        list(APPEND command -d64)
      else()
        message("Warning: unknown architecture ${test_arch}")
      endif()
    endif()
  endif()

  if(LAST_TEST_DEPS_OK)
    itk_add_test(NAME ${name}
      COMMAND "${ITK_TEST_DRIVER}"
      --add-before-libpath "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/$<CONFIGURATION>"
      --add-before-libpath "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}"
      --add-before-libpath "${ITK_DIR}"
      ${command}
      "-Djava.library.path=${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/$<CONFIGURATION>${sep}${CMAKE_LIBRARY_OUTPUT_DIRECTORY}"
      -cp "${classpath}"
      ${ARGN}
      )
  endif()
endmacro()


#----------------
#
# configure the tests
#
#----------------


#----------------
UNIQUE(types "${WRAP_ITK_SCALAR};UC")
# signed char can't be used to store an image with values up to 255
list(REMOVE_ITEM types SC)
foreach(d ${ITK_WRAP_IMAGE_DIMS})
  foreach(t ${types})

    set(test_base_name simplePipeline${t}${d})

    set(ITK_WRAP_JAVA_TEST_IMAGE_DIMENSION ${d})
    set(ITK_WRAP_JAVA_TEST_PIXEL_TYPE ${t})
    configure_file("${CMAKE_CURRENT_SOURCE_DIR}/simplePipeline.java.in"
                  "${CMAKE_CURRENT_BINARY_DIR}/${test_base_name}.java"
                  @ONLY)

    build_java_test_bin(${test_base_name} class_path ITKIOImageBase)

    add_java_test(JavaSimplePipeline${t}${d}
      ${class_path}
      ${test_base_name}
      DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
      ${test_base_name}.nrrd
      --compare ${test_base_name}.nrrd DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
    )

  endforeach()
endforeach()

#----------------
foreach(d ${ITK_WRAP_IMAGE_DIMS})
  if(d EQUAL 2)
    build_java_test(MedianImageFilter class_path ITKIOImageBase ITKSmoothing)
    add_java_test(JavaMedianImageFilter
      ${class_path}
      MedianImageFilter
      DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
      MedianImageFilter.png
      3 3
      MedianImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/MedianImageFilter.png}
    )

    build_java_test(MeanImageFilter class_path ITKIOImageBase ITKSmoothing)
    add_java_test(JavaMeanImageFilter
      ${class_path}
      MeanImageFilter
      DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
      MeanImageFilter.png
      3 3
      MeanImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/MeanImageFilter.png}
    )

    build_java_test(BinaryDilateImageFilter class_path ITKIOImageBase ITKImageFilterBase ITKBinaryMathematicalMorphology)
    add_java_test(JavaBinaryDilateImageFilter
      ${class_path}
      BinaryDilateImageFilter
      DATA{${WrapITK_SOURCE_DIR}/images/2th_cthead1.png}
      BinaryDilateImageFilter.png
      --compare BinaryDilateImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/BinaryDilateImageFilter.png}
    )

    build_java_test(BinaryErodeImageFilter class_path ITKIOImageBase ITKImageFilterBase ITKBinaryMathematicalMorphology)
    add_java_test(JavaBinaryErodeImageFilter
      ${class_path}
      BinaryErodeImageFilter
      DATA{${WrapITK_SOURCE_DIR}/images/2th_cthead1.png}
      BinaryErodeImageFilter.png
      --compare BinaryErodeImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/BinaryErodeImageFilter.png}
    )

    build_java_test(CurvatureAnisotropicDiffusionImageFilter class_path ITKIOImageBase ITKAnisotropicSmoothing ITKImageIntensity)
    add_java_test(JavaCurvatureAnisotropicDiffusionImageFilter
      ${class_path}
      CurvatureAnisotropicDiffusionImageFilter
      DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
      CurvatureAnisotropicDiffusionImageFilter.png
      5 0.125 3
    )

    build_java_test(CurvatureFlowImageFilter class_path ITKIOImageBase ITKCurvatureFlow ITKImageIntensity)
    add_java_test(JavaCurvatureFlowImageFilter
      ${class_path}
      CurvatureFlowImageFilter
      DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
      CurvatureFlowImageFilter.png
      5 0.125 1.0
    )

    build_java_test(GradientAnisotropicDiffusionImageFilter class_path ITKIOImageBase ITKAnisotropicSmoothing ITKImageIntensity)

    build_java_test(SigmoidImageFilter class_path ITKIOImageBase ITKImageIntensity)
    add_java_test(JavaSigmoidImageFilter
      ${class_path}
      SigmoidImageFilter
      DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
      SigmoidImageFilter.png
      10 240 10 170
      --compare SigmoidImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/SigmoidImageFilter.png}
    )

    build_java_test(ThresholdImageFilter class_path ITKIOImageBase ITKThresholding)
    add_java_test(JavaThresholdImageFilter
      ${class_path}
      ThresholdImageFilter
      DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
      ThresholdImageFilter.png
      150 150
      --compare ThresholdImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/ThresholdImageFilter.png}
    )

    build_java_test(BinaryThresholdImageFilter class_path ITKIOImageBase ITKThresholding)
    add_java_test(JavaBinaryThresholdImageFilter
      ${class_path}
      BinaryThresholdImageFilter
      DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
      BinaryThresholdImageFilter.png
      50 150
      0 255
      --compare BinaryThresholdImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/BinaryThresholdImageFilter.png}
    )

    build_java_test(CastImageFilter class_path ITKIOImageBase ITKImageFilterBase)
    add_java_test(JavaCastImageFilter
      ${class_path}
      CastImageFilter
      DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
      CastImageFilter.png
      50 150
      0 255
      --compare CastImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
    )

  endif()
endforeach()

build_java_test(MetaDataDictionary class_path ITKIOImageBase)
add_java_test(JavaMetaDataDictionary
  ${class_path}
  MetaDataDictionary
)

add_custom_target(JavaTests ALL DEPENDS ${targets})
add_dependencies(JavaTests ${depends})
