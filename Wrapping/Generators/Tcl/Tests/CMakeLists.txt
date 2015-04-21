
# configure the test driver
macro(add_tcl_test name)
  set(command "${TCL_TCLSH}")

  # add extra command which may be needed on some systems
  if(CMAKE_OSX_ARCHITECTURES)
    list(GET CMAKE_OSX_ARCHITECTURES 0 test_arch)
    set(command arch -${test_arch} ${command})
  endif()

  itk_add_test(NAME ${name}
    COMMAND "${ITK_TEST_DRIVER}"
    --add-before-env-with-sep TCLLIBPATH "${WrapITK_BINARY_DIR}/Generators/Tcl/$<CONFIGURATION>" " "
    --add-before-env-with-sep TCLLIBPATH "${WrapITK_BINARY_DIR}/Generators/Tcl" " "
    --add-before-libpath "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/$<CONFIGURATION>"
    --add-before-libpath "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}"
    ${command}
    ${ARGN}
    )
endmacro()



add_tcl_test(TclListAll ${CMAKE_CURRENT_SOURCE_DIR}/ListAll.tcl)

# this test is broken I think because of a bug in itk::ParallelSparseFieldLevelSetImageFilter::PrintSelf()
# TODO: run this test, once the bug is fixed
# add_tcl_test(TclPrintAll ${CMAKE_CURRENT_SOURCE_DIR}/PrintAll.tcl)

INTERSECTION(WRAP_2 2 "${ITK_WRAP_IMAGE_DIMS}")
if(ITK_WRAP_unsigned_char AND WRAP_2)
#  add_tcl_test(TclBinaryDilateImageFilter
#    ${CMAKE_CURRENT_SOURCE_DIR}/BinaryDilateImageFilter.tcl
#    DATA{${WrapITK_SOURCE_DIR}/images/2th_cthead1.png}
#    BinaryDilateImageFilter.png
#    --compare BinaryDilateImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/BinaryDilateImageFilter.png}
#  )

  add_tcl_test(TclBinaryErodeImageFilter
    ${CMAKE_CURRENT_SOURCE_DIR}/BinaryErodeImageFilter.tcl
    DATA{${WrapITK_SOURCE_DIR}/images/2th_cthead1.png}
    BinaryErodeImageFilter.png
    --compare BinaryErodeImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/BinaryErodeImageFilter.png}
  )

  add_tcl_test(TclBinaryThresholdImageFilter
    ${CMAKE_CURRENT_SOURCE_DIR}/BinaryThresholdImageFilter.tcl
    DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
    BinaryThresholdImageFilter.png
    50 150
    0 255
    --compare BinaryThresholdImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/BinaryThresholdImageFilter.png}
  )

  add_tcl_test(TclCastImageFilter
    ${CMAKE_CURRENT_SOURCE_DIR}/CastImageFilter.tcl
    DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
    CastImageFilter.png
    --compare CastImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/CastImageFilter.png}
  )

# need to find a dicom image for this test

#   add_tcl_test(TclDicomSliceRead
#     ${CMAKE_CURRENT_SOURCE_DIR}/DicomSliceRead.tcl
#     DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
#     DicomSliceRead.png
#   )

  add_tcl_test(TclMeanImageFilter
    ${CMAKE_CURRENT_SOURCE_DIR}/MeanImageFilter.tcl
    DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
    MeanImageFilter.png
    3
  )

  add_tcl_test(TclMedianImageFilter
    ${CMAKE_CURRENT_SOURCE_DIR}/MedianImageFilter.tcl
    DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
    MedianImageFilter.png
    3
    --compare MedianImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/MedianImageFilter.png}
  )

  add_tcl_test(TclSigmoidImageFilter
    ${CMAKE_CURRENT_SOURCE_DIR}/SigmoidImageFilter.tcl
    DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
    SigmoidImageFilter.png
    10 240 10 170
    --compare SigmoidImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/SigmoidImageFilter.png}
  )

  add_tcl_test(TclThresholdImageFilter
    ${CMAKE_CURRENT_SOURCE_DIR}/ThresholdImageFilter.tcl
    DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
    ThresholdImageFilter.png
    150 150
    --compare ThresholdImageFilter.png DATA{${WrapITK_SOURCE_DIR}/images/ThresholdImageFilter.png}
  )

endif()



if(ITK_WRAP_float AND WRAP_2)
#   add_tcl_test(TclCannyEdgeDetectionImageFilter
#     ${CMAKE_CURRENT_SOURCE_DIR}/CannyEdgeDetectionImageFilter.tcl
#     DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
#     TclCannyEdgeDetectionImageFilter.png
#   )

  add_tcl_test(TclCurvatureAnisotropicDiffusionImageFilter
    ${CMAKE_CURRENT_SOURCE_DIR}/CurvatureAnisotropicDiffusionImageFilter.tcl
    DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
    CurvatureAnisotropicDiffusionImageFilter.png
    5 0.125 3
  )

  add_tcl_test(TclCurvatureFlowImageFilter
    ${CMAKE_CURRENT_SOURCE_DIR}/CurvatureFlowImageFilter.tcl
    DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
    CurvatureFlowImageFilter.png
    5 0.125 1.0
  )

  add_tcl_test(TclGradientAnisotropicDiffusionImageFilter
    ${CMAKE_CURRENT_SOURCE_DIR}/GradientAnisotropicDiffusionImageFilter.tcl
    DATA{${WrapITK_SOURCE_DIR}/images/BrainProtonDensitySlice.png}
    GradientAnisotropicDiffusionImageFilter.png
    5 0.125 1.0
    --compare GradientAnisotropicDiffusionImageFilter.png
              DATA{${WrapITK_SOURCE_DIR}/images/GradientAnisotropicDiffusionImageFilter.png}
  )
endif()


UNIQUE(types "${WRAP_ITK_SCALAR};UC")
# signed char can't be used to store an image with values up to 255
list(REMOVE_ITEM types SC)
foreach(TCL_TEST_IMAGE_DIMENSION ${ITK_WRAP_IMAGE_DIMS})
  foreach(TCL_TEST_PIXEL_TYPE ${types})

    set(TEST_FILE_NAME simplePipeline${TCL_TEST_PIXEL_TYPE}${TCL_TEST_IMAGE_DIMENSION})

    configure_file("${CMAKE_CURRENT_SOURCE_DIR}/simplePipeline.tcl.in"
                  "${CMAKE_CURRENT_BINARY_DIR}/${TEST_FILE_NAME}.tcl"
                  @ONLY)

    add_tcl_test(TclSimplePipeline${TCL_TEST_PIXEL_TYPE}${TCL_TEST_IMAGE_DIMENSION}
      ${TEST_FILE_NAME}.tcl
      DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
      ${TEST_FILE_NAME}.nrrd
      --compare ${TEST_FILE_NAME}.nrrd DATA{${WrapITK_SOURCE_DIR}/images/cthead1.png}
    )

  endforeach()
endforeach()
