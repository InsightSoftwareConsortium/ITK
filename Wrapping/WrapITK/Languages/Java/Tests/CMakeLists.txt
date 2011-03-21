
# configure the test driver
find_program(ITK_TEST_DRIVER itkTestDriver)

# windows doesn't use the same separator than the others
if(WIN32)
  set(sep "\\\;")
else(WIN32)
  set(sep ":")
endif(WIN32)

set(JAVA_DRIVER "${ITK_TEST_DRIVER}"
  --add-before-libpath "${ITK_DIR}/bin/\\\${CTEST_CONFIGURATION_TYPE}"
  --add-before-libpath "${ITK_DIR}/bin"
  --add-before-libpath "${ITK_DIR}"
  "${Java_JAVA_EXECUTABLE}"
  -Djava.library.path=${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/\\\${CTEST_CONFIGURATION_TYPE}${sep}${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
)

# put all generated files in this list
set(targets "")
# the required libs
set(depends "")

macro(BUILD_CLASSPATH var)
  set(${var} ".")
  set(LAST_TEST_DEPS_OK ON)
  foreach(lib ${ARGN})
    string(TOLOWER ${lib} lower_lib)
    set(${var} "${${var}}${sep}${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/org.itk.${lower_lib}.jar")
    set(depends ${depends} ${lib}JavaJar)
    if(NOT "${WRAP_ITK_LIBRARIES}" MATCHES "(^|;)${lib}(;|$)")
      set(LAST_TEST_DEPS_OK OFF)
    endif(NOT "${WRAP_ITK_LIBRARIES}" MATCHES "(^|;)${lib}(;|$)")
  endforeach(lib)
endmacro(BUILD_CLASSPATH)


macro(BUILD_JAVA_TEST name cp_var)
  # a convenient macro to build java tests
  #
  BUILD_CLASSPATH(${cp_var} ${ARGN})
  if(LAST_TEST_DEPS_OK)
    add_custom_command(
      OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${name}.class
      COMMAND ${Java_JAVAC_EXECUTABLE}
      ARGS -classpath "${${cp_var}}"
      -d "${CMAKE_CURRENT_BINARY_DIR}" ${CMAKE_CURRENT_SOURCE_DIR}/${name}.java
      DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${name}.java
      )
    set(targets ${targets} ${CMAKE_CURRENT_BINARY_DIR}/${name}.class)
  else(LAST_TEST_DEPS_OK)
    message("Warning: Can't buid java test ${name}.")
  endif(LAST_TEST_DEPS_OK)
endmacro(BUILD_JAVA_TEST)

macro(BUILD_JAVA_TEST_BIN name cp_var)
  # a convenient macro to build java tests
  #
  BUILD_CLASSPATH(${cp_var} ${ARGN})
  if(LAST_TEST_DEPS_OK)
    add_custom_command(
      OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${name}.class
      COMMAND ${Java_JAVAC_EXECUTABLE}
      ARGS -classpath "${${cp_var}}"
      -d "${CMAKE_CURRENT_BINARY_DIR}" ${CMAKE_CURRENT_BINARY_DIR}/${name}.java
      DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${name}.java
      )
    set(targets ${targets} ${CMAKE_CURRENT_BINARY_DIR}/${name}.class)
  else(LAST_TEST_DEPS_OK)
    message("Warning: Can't buid java test ${name}.")
  endif(LAST_TEST_DEPS_OK)
endmacro(BUILD_JAVA_TEST_BIN)

macro(JAVA_ADD_TEST)
  if(LAST_TEST_DEPS_OK)
    add_test(${ARGN})
  endif(LAST_TEST_DEPS_OK)
endmacro(JAVA_ADD_TEST)


# configure the tests




UNIQUE(types "${WRAP_ITK_SCALAR};UC")
# signed char can't be used to store an image with values up to 255
list(REMOVE_ITEM types SC)
foreach(d ${WRAP_ITK_DIMS})
  foreach(t ${types})

    set(test_base_name simplePipeline${t}${d})

    set(WRAP_ITK_JAVA_TEST_IMAGE_DIMENSION ${d})
    set(WRAP_ITK_JAVA_TEST_PIXEL_TYPE ${t})
    configure_file("${CMAKE_CURRENT_SOURCE_DIR}/simplePipeline.java.in"
                  "${CMAKE_CURRENT_BINARY_DIR}/${test_base_name}.java"
                  @ONLY IMMEDIATE)

    BUILD_JAVA_TEST_BIN(${test_base_name} class_path IO)

    JAVA_ADD_TEST(JavaSimplePipeline${t}${d}
      ${JAVA_DRIVER} -classpath ${class_path}
      ${test_base_name}
      ${WrapITK_SOURCE_DIR}/images/cthead1.png
      ${test_base_name}.nrrd
      --compare ${test_base_name}.nrrd ${WrapITK_SOURCE_DIR}/images/cthead1.png
    )

  endforeach(t)
endforeach(d)

BUILD_JAVA_TEST(MedianImageFilter class_path IO Denoising)
JAVA_ADD_TEST(JavaMedianImageFilter
  ${JAVA_DRIVER} -classpath ${class_path}
  MedianImageFilter
  ${WrapITK_SOURCE_DIR}/images/cthead1.png
  MedianImageFilter.png
  3 3
  MedianImageFilter.png ${WrapITK_SOURCE_DIR}/images/MedianImageFilter.png
)

BUILD_JAVA_TEST(MeanImageFilter class_path IO Denoising)
JAVA_ADD_TEST(JavaMeanImageFilter
  ${JAVA_DRIVER} -classpath ${class_path}
  MeanImageFilter
  ${WrapITK_SOURCE_DIR}/images/cthead1.png
  MeanImageFilter.png
  3 3
  MeanImageFilter.png ${WrapITK_SOURCE_DIR}/images/MeanImageFilter.png
)

BUILD_JAVA_TEST(BinaryDilateImageFilter class_path IO SimpleFilters BinaryMorphology)
JAVA_ADD_TEST(JavaBinaryDilateImageFilter
  ${JAVA_DRIVER} -classpath ${class_path}
  BinaryDilateImageFilter
  ${WrapITK_SOURCE_DIR}/images/2th_cthead1.png
  BinaryDilateImageFilter.png
  --compare BinaryDilateImageFilter.png ${WrapITK_SOURCE_DIR}/images/BinaryDilateImageFilter.png
)

BUILD_JAVA_TEST(BinaryErodeImageFilter class_path IO SimpleFilters BinaryMorphology)
JAVA_ADD_TEST(JavaBinaryErodeImageFilter
  ${JAVA_DRIVER} -classpath ${class_path}
  BinaryErodeImageFilter
  ${WrapITK_SOURCE_DIR}/images/2th_cthead1.png
  BinaryErodeImageFilter.png
  --compare BinaryErodeImageFilter.png ${WrapITK_SOURCE_DIR}/images/BinaryErodeImageFilter.png
)

BUILD_JAVA_TEST(BinaryThresholdImageFilter class_path IO BinaryMorphology)
JAVA_ADD_TEST(JavaBinaryThresholdImageFilter
  ${JAVA_DRIVER} -classpath ${class_path}
  BinaryThresholdImageFilter
  ${WrapITK_SOURCE_DIR}/images/cthead1.png
  BinaryThresholdImageFilter.png
  50 150
  0 65535
  --compare BinaryThresholdImageFilter.png ${WrapITK_SOURCE_DIR}/images/BinaryThresholdImageFilter.png
)

BUILD_JAVA_TEST(CastImageFilter class_path IO SimpleFilters)
JAVA_ADD_TEST(JavaCastImageFilter
  ${JAVA_DRIVER} -classpath ${class_path}
  CastImageFilter
  ${WrapITK_SOURCE_DIR}/images/cthead1.png
  CastImageFilter.png
  50 150
  0 65535
  --compare CastImageFilter.png ${WrapITK_SOURCE_DIR}/images/cthead1.png
)

BUILD_JAVA_TEST(CurvatureAnisotropicDiffusionImageFilter class_path IO SimpleFilters Denoising IntensityFilters)
JAVA_ADD_TEST(JavaCurvatureAnisotropicDiffusionImageFilter
  ${JAVA_DRIVER} -classpath ${class_path}
  CurvatureAnisotropicDiffusionImageFilter
  ${WrapITK_SOURCE_DIR}/images/cthead1.png
  CurvatureAnisotropicDiffusionImageFilter.png
  5 0.125 3
)

BUILD_JAVA_TEST(CurvatureFlowImageFilter class_path IO SimpleFilters Denoising IntensityFilters)

BUILD_JAVA_TEST(GradientAnisotropicDiffusionImageFilter class_path IO SimpleFilters Denoising IntensityFilters)
JAVA_ADD_TEST(JavaCurvatureFlowImageFilter
  ${JAVA_DRIVER} -classpath ${class_path}
  CurvatureFlowImageFilter
  ${WrapITK_SOURCE_DIR}/images/cthead1.png
  CurvatureFlowImageFilter.png
  5 0.125 1.0
)

BUILD_JAVA_TEST(SigmoidImageFilter class_path IO  IntensityFilters)
JAVA_ADD_TEST(JavaSigmoidImageFilter
  ${JAVA_DRIVER} -classpath ${class_path}
  SigmoidImageFilter
  ${WrapITK_SOURCE_DIR}/images/cthead1.png
  SigmoidImageFilter.png
  10 240 10 170
  --compare SigmoidImageFilter.png ${WrapITK_SOURCE_DIR}/images/SigmoidImageFilter.png
)

BUILD_JAVA_TEST(ThresholdImageFilter class_path IO SegmentationAndThreshold)
JAVA_ADD_TEST(JavaThresholdImageFilter
  ${JAVA_DRIVER} -classpath ${class_path}
  ThresholdImageFilter
  ${WrapITK_SOURCE_DIR}/images/cthead1.png
  ThresholdImageFilter.png
  150 150
  --compare ThresholdImageFilter.png ${WrapITK_SOURCE_DIR}/images/ThresholdImageFilter.png
)


add_custom_target(JavaTests ALL DEPENDS ${targets})
add_dependencies(JavaTests ${depends})
