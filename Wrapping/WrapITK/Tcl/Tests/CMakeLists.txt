
# configure the test driver
find_program(ITK_TEST_DRIVER itkTestDriver)

set(TCL_DRIVER "${ITK_TEST_DRIVER}"
  --add-before-env TCLLIBPATH "${WrapITK_BINARY_DIR}/Tcl/\${CTEST_CONFIGURATION_TYPE}"
  "${EXECUTABLE_OUTPUT_PATH}/\${CTEST_CONFIGURATION_TYPE}/itkwish"
)



add_test(TclListAll ${TCL_DRIVER} ${CMAKE_CURRENT_SOURCE_DIR}/ListAll.tcl)

# this test is broken I think because of a bug in itk::ParallelSparseFieldLevelSetImageFilter::PrintSelf()
# TODO: run this test, once the bug is fixed
# add_test(TclPrintAll ${TCL_DRIVER} ${CMAKE_CURRENT_SOURCE_DIR}/PrintAll.tcl)

INTERSECTION(WRAP_2 2 "${WRAP_ITK_DIMS}")
if(WRAP_unsigned_short AND WRAP_2)
  add_test(TclBinaryDilateImageFilter
    ${TCL_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/BinaryDilateImageFilter.tcl
    ${WrapITK_SOURCE_DIR}/images/2th_cthead1.png
    BinaryDilateImageFilter.png
    --compare BinaryDilateImageFilter.png ${WrapITK_SOURCE_DIR}/images/BinaryDilateImageFilter.png
  )

  add_test(TclBinaryErodeImageFilter
    ${TCL_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/BinaryErodeImageFilter.tcl
    ${WrapITK_SOURCE_DIR}/images/2th_cthead1.png
    BinaryErodeImageFilter.png
    --compare BinaryErodeImageFilter.png ${WrapITK_SOURCE_DIR}/images/BinaryErodeImageFilter.png
  )

  add_test(TclBinaryThresholdImageFilter
    ${TCL_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/BinaryThresholdImageFilter.tcl
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    BinaryThresholdImageFilter.png
    50 150
    0 65535
    --compare BinaryThresholdImageFilter.png ${WrapITK_SOURCE_DIR}/images/BinaryThresholdImageFilter.png
  )

  add_test(TclCastImageFilter
    ${TCL_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/CastImageFilter.tcl
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    CastImageFilter.png
    --compare CastImageFilter.png ${WrapITK_SOURCE_DIR}/images/CastImageFilter.png
  )

# need to find a dicom image for this test

#   add_test(TclDicomSliceRead
#     ${TCL_DRIVER}
#     ${CMAKE_CURRENT_SOURCE_DIR}/DicomSliceRead.tcl
#     ${WrapITK_SOURCE_DIR}/images/cthead1.png
#     DicomSliceRead.png
#   )

  add_test(TclMeanImageFilter
    ${TCL_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/MeanImageFilter.tcl
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    MeanImageFilter.png
    3
  )

  add_test(TclMedianImageFilter
    ${TCL_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/MedianImageFilter.tcl
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    MedianImageFilter.png
    3
    --compare MeanImageFilter.png ${WrapITK_SOURCE_DIR}/images/MeanImageFilter.png
  )

  add_test(TclSigmoidImageFilter
    ${TCL_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/SigmoidImageFilter.tcl
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    SigmoidImageFilter.png
    10 240 10 170
    --compare SigmoidImageFilter.png ${WrapITK_SOURCE_DIR}/images/SigmoidImageFilter.png
  )

  add_test(TclThresholdImageFilter
    ${TCL_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/ThresholdImageFilter.tcl
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    ThresholdImageFilter.png
    150 150
    --compare ThresholdImageFilter.png ${WrapITK_SOURCE_DIR}/images/ThresholdImageFilter.png
  )

endif(WRAP_unsigned_short AND WRAP_2)



if(WRAP_float AND WRAP_2)
#   add_test(TclCannyEdgeDetectionImageFilter
#     ${TCL_DRIVER}
#     ${CMAKE_CURRENT_SOURCE_DIR}/CannyEdgeDetectionImageFilter.tcl
#     ${WrapITK_SOURCE_DIR}/images/cthead1.png
#     TclCannyEdgeDetectionImageFilter.png
#   )

  add_test(TclCurvatureAnisotropicDiffusionImageFilter
    ${TCL_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/CurvatureAnisotropicDiffusionImageFilter.tcl
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    CurvatureAnisotropicDiffusionImageFilter.png
    5 0.125 3
  )

  add_test(TclCurvatureFlowImageFilter
    ${TCL_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/CurvatureFlowImageFilter.tcl
    ${WrapITK_SOURCE_DIR}/images/cthead1.png
    CurvatureFlowImageFilter.png
    5 0.125 1.0
  )

  add_test(TclGradientAnisotropicDiffusionImageFilter
    ${TCL_DRIVER}
    ${CMAKE_CURRENT_SOURCE_DIR}/GradientAnisotropicDiffusionImageFilter.tcl
    ${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png
    GradientAnisotropicDiffusionImageFilter.png
    5 0.125 1.0
    --compare GradientAnisotropicDiffusionImageFilter.png
              ${WrapITK_SOURCE_DIR}/images/GradientAnisotropicDiffusionImageFilter.png
  )
endif(WRAP_float AND WRAP_2)


UNIQUE(types "${WRAP_ITK_SCALAR};UC")
# signed char can't be used to store an image with values up to 255
list(REMOVE_ITEM types SC)
foreach(TCL_TEST_IMAGE_DIMENSION ${WRAP_ITK_DIMS})
  foreach(TCL_TEST_PIXEL_TYPE ${types})

    set(TEST_FILE_NAME simplePipeline${TCL_TEST_PIXEL_TYPE}${TCL_TEST_IMAGE_DIMENSION})

    configure_file("${CMAKE_CURRENT_SOURCE_DIR}/simplePipeline.tcl.in"
                  "${CMAKE_CURRENT_BINARY_DIR}/${TEST_FILE_NAME}.tcl"
                  @ONLY IMMEDIATE)

    add_test(TclSimplePipeline${TCL_TEST_PIXEL_TYPE}${TCL_TEST_IMAGE_DIMENSION}
      ${TCL_DRIVER}
      ${TEST_FILE_NAME}.tcl
      ${WrapITK_SOURCE_DIR}/images/cthead1.png
      ${TEST_FILE_NAME}.nrrd
      --compare ${TEST_FILE_NAME}.nrrd ${WrapITK_SOURCE_DIR}/images/cthead1.png
    )

  endforeach(TCL_TEST_PIXEL_TYPE)
endforeach(TCL_TEST_IMAGE_DIMENSION)

