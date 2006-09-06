
# configure the test driver
FIND_PROGRAM(IMAGE_COMPARE ImageCompare)
IF(CMAKE_CONFIGURATION_TYPES)
  FOREACH(config ${CMAKE_CONFIGURATION_TYPES})
    SET(CONFIG_WRAP_ITK_TCL_DIR "${WrapITK_BINARY_DIR}/Tcl/${config}")
    SET(CONFIG_WRAP_ITK_TCL_SWIG_DIR "${LIBRARY_OUTPUT_PATH}/${config}")
    SET(CONFIG_WRAP_ITK_ITKWISH_DIR "${EXECUTABLE_OUTPUT_PATH}/${config}")
    CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/tclTestDriver.tcl.in
                 ${CMAKE_CURRENT_BINARY_DIR}/${config}/tclTestDriver.tcl
                 @ONLY IMMEDIATE )
  ENDFOREACH(config)
ELSE(CMAKE_CONFIGURATION_TYPES)
  SET(CONFIG_WRAP_ITK_TCL_DIR "${WrapITK_BINARY_DIR}")
  SET(CONFIG_WRAP_ITK_TCL_SWIG_DIR "${LIBRARY_OUTPUT_PATH}")
  SET(CONFIG_WRAP_ITK_ITKWISH_DIR "${EXECUTABLE_OUTPUT_PATH}")
  CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/tclTestDriver.tcl.in
               ${CMAKE_CURRENT_BINARY_DIR}/tclTestDriver.tcl
               @ONLY IMMEDIATE )
ENDIF(CMAKE_CONFIGURATION_TYPES)



SET(TCL_DRIVER ${TCL_TCLSH} "${CMAKE_CURRENT_BINARY_DIR}/\${CTEST_CONFIGURATION_TYPE}/tclTestDriver.tcl")

ADD_TEST(TclListAll ${TCL_DRIVER} ${CMAKE_CURRENT_SOURCE_DIR}/ListAll.tcl)

# this test is broken I think because of a bug in itk::ParallelSparseFieldLevelSetImageFilter::PrintSelf()
# TODO: run this test, once the bug is fixed
# ADD_TEST(TclPrintAll ${TCL_DRIVER} ${CMAKE_CURRENT_SOURCE_DIR}/PrintAll.tcl)

INTERSECTION(WRAP_2 2 "${WRAP_ITK_DIMS}")
IF(WRAP_unsigned_short AND WRAP_2)
  ADD_TEST(TclBinaryDilateImageFilter
    ${TCL_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/BinaryDilateImageFilter.tcl
    ${WrapITK_SOURCE_DIR}/images/2th_cthead1.png
    BinaryDilateImageFilter.png
    --compare BinaryDilateImageFilter.png ${WrapITK_SOURCE_DIR}/images/BinaryDilateImageFilter.png
  )

  ADD_TEST(TclBinaryErodeImageFilter
    ${TCL_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/BinaryErodeImageFilter.tcl
    ${WrapITK_SOURCE_DIR}/images/2th_cthead1.png
    BinaryErodeImageFilter.png
    --compare BinaryErodeImageFilter.png ${WrapITK_SOURCE_DIR}/images/BinaryErodeImageFilter.png
  )

  ADD_TEST(TclBinaryThresholdImageFilter
    ${TCL_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/BinaryThresholdImageFilter.tcl
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    BinaryThresholdImageFilter.png
    50 150
    0 65535
    --compare BinaryThresholdImageFilter.png ${WrapITK_SOURCE_DIR}/images/BinaryThresholdImageFilter.png
  )

  ADD_TEST(TclCastImageFilter
    ${TCL_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/CastImageFilter.tcl
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    CastImageFilter.png
    --compare CastImageFilter.png ${WrapITK_SOURCE_DIR}/images/CastImageFilter.png
  )

# need to find a dicom image for this test

#   ADD_TEST(TclDicomSliceRead
#     ${TCL_DRIVER}
#     ${CMAKE_CURRENT_SOURCE_DIR}/DicomSliceRead.tcl
#     ${WrapITK_SOURCE_DIR}/images/cthead1.png
#     DicomSliceRead.png
#   )

  ADD_TEST(TclMeanImageFilter
    ${TCL_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/MeanImageFilter.tcl
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    MeanImageFilter.png
    3
  )

  ADD_TEST(TclMedianImageFilter
    ${TCL_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/MedianImageFilter.tcl
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    MedianImageFilter.png
    3
    --compare MeanImageFilter.png ${WrapITK_SOURCE_DIR}/images/MeanImageFilter.png
  )

  ADD_TEST(TclSigmoidImageFilter
    ${TCL_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/SigmoidImageFilter.tcl
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    SigmoidImageFilter.png
    10 240 10 170
    --compare SigmoidImageFilter.png ${WrapITK_SOURCE_DIR}/images/SigmoidImageFilter.png
  )

  ADD_TEST(TclThresholdImageFilter
    ${TCL_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/ThresholdImageFilter.tcl
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    ThresholdImageFilter.png
    150 150
    --compare ThresholdImageFilter.png ${WrapITK_SOURCE_DIR}/images/ThresholdImageFilter.png
  )

ENDIF(WRAP_unsigned_short AND WRAP_2)



IF(WRAP_float AND WRAP_2)
#   ADD_TEST(TclCannyEdgeDetectionImageFilter
#     ${TCL_DRIVER}
#     ${CMAKE_CURRENT_SOURCE_DIR}/CannyEdgeDetectionImageFilter.tcl
#     ${WrapITK_SOURCE_DIR}/images/cthead1.png
#     TclCannyEdgeDetectionImageFilter.png
#   )

  ADD_TEST(TclCurvatureAnisotropicDiffusionImageFilter
    ${TCL_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/CurvatureAnisotropicDiffusionImageFilter.tcl
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    CurvatureAnisotropicDiffusionImageFilter.png
    5 0.125 3
  )

  ADD_TEST(TclCurvatureFlowImageFilter
    ${TCL_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/CurvatureFlowImageFilter.tcl
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    CurvatureFlowImageFilter.png
    5 0.125 1.0
  )

  ADD_TEST(TclGradientAnisotropicDiffusionImageFilter
    ${TCL_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/GradientAnisotropicDiffusionImageFilter.tcl
    ${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png
    GradientAnisotropicDiffusionImageFilter.png
    5 0.125 1.0
    --compare GradientAnisotropicDiffusionImageFilter.png
              ${WrapITK_SOURCE_DIR}/images/GradientAnisotropicDiffusionImageFilter.png
  )
ENDIF(WRAP_float AND WRAP_2)


UNIQUE(types "${WRAP_ITK_SCALAR};UC")
FOREACH(TCL_TEST_IMAGE_DIMENSION ${WRAP_ITK_DIMS})
  FOREACH(TCL_TEST_PIXEL_TYPE ${types})

    SET(TEST_FILE_NAME simplePipeline${TCL_TEST_PIXEL_TYPE}${TCL_TEST_IMAGE_DIMENSION})

    CONFIGURE_FILE("${CMAKE_CURRENT_SOURCE_DIR}/simplePipeline.tcl.in"
                  "${CMAKE_CURRENT_BINARY_DIR}/${TEST_FILE_NAME}.tcl"
                  @ONLY IMMEDIATE)

    ADD_TEST(TclSimplePipeline${TCL_TEST_PIXEL_TYPE}${TCL_TEST_IMAGE_DIMENSION}
      ${TCL_DRIVER}
      ${TEST_FILE_NAME}.tcl
      ${WrapITK_SOURCE_DIR}/images/cthead1.png
      ${TEST_FILE_NAME}.nrrd
      --compare ${TEST_FILE_NAME}.nrrd ${WrapITK_SOURCE_DIR}/images/cthead1.png
    )

  ENDFOREACH(TCL_TEST_PIXEL_TYPE)
ENDFOREACH(TCL_TEST_IMAGE_DIMENSION)

