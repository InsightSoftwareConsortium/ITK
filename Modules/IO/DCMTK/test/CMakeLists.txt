itk_module_test()
set(
  ITKIODCMTKTests
  itkDCMTKImageIOMultiFrameImageTest.cxx
  itkDCMTKImageIONoPreambleTest.cxx
  itkDCMTKImageIOOrthoDirTest.cxx
  itkDCMTKImageIOSlopeInterceptTest.cxx
  itkDCMTKImageIOSpacingTest.cxx
  itkDCMTKImageIOTest.cxx
  itkDCMTKLoggerTest.cxx
  itkDCMTKMultiFrame4DTest.cxx
  itkDCMTKRGBImageIOTest.cxx
  itkDCMTKSeriesReadImageWrite.cxx
  itkDCMTKSeriesStreamReadImageWrite.cxx
)

createtestdriver(ITKIODCMTK "${ITKIODCMTK-Test_LIBRARIES}" "${ITKIODCMTKTests}")

# itkDCMTKFileReader.h transitively #include's dcmtk/dcmdata/*.h.  ITKIODCMTK
# declares ITKDCMTK as a PRIVATE_DEPENDS (so consumers don't see DCMTK
# headers), but the test driver consumes the public itkDCMTKFileReader.h
# directly and therefore needs the DCMTK include path on its compile line.
# Tracks issue #3820.  Reconstruct the include path the same way
# Modules/ThirdParty/DCMTK/CMakeLists.txt populates ITKDCMTK_INCLUDE_DIRS,
# since that variable is not in scope here.
if(NOT ITK_USE_SYSTEM_DCMTK)
  set(
    _dcmtk_ep_inc
    "${CMAKE_BINARY_DIR}/Modules/ThirdParty/DCMTK/ITKDCMTK_ExtProject"
  )
  set(
    _dcmtk_libs
    dcmdata
    dcmpstat
    dcmsr
    dcmqrdb
    dcmimgle
    dcmimage
    dcmjpeg
    dcmjpls
    dcmnet
    dcmwlm
    dcmrt
    dcmiod
    dcmfg
    dcmseg
    dcmpmap
    ofstd
    oflog
  )
  # IS_DIRECTORY guards were dropped: the ExtProject source/build trees are
  # only materialized at build time, not at CMake configure time, so any
  # IS_DIRECTORY check here returns false on a clean checkout (issue #6189
  # greptile P1). The paths below are deterministic, and CMake is fine with
  # -I flags that name not-yet-existing directories — they just need to
  # exist by the time the test driver is compiled, which is guaranteed by
  # ITKDCMTK_ExtProject being a transitive build dependency of the test
  # driver via the ITKIODCMTK module's PRIVATE_DEPENDS.
  foreach(_lib IN LISTS _dcmtk_libs)
    target_include_directories(
      ITKIODCMTKTestDriver
      PRIVATE
        "${_dcmtk_ep_inc}/${_lib}/include"
    )
  endforeach()
  target_include_directories(
    ITKIODCMTKTestDriver
    PRIVATE
      "${CMAKE_BINARY_DIR}/Modules/ThirdParty/DCMTK/ITKDCMTK_ExtProject-build/config/include"
  )
  unset(_dcmtk_libs)
  unset(_dcmtk_ep_inc)
endif()

itk_add_test(
  NAME itkDCMTKImageIOTest1
  COMMAND
    ITKIODCMTKTestDriver
    itkDCMTKImageIOTest
    DATA{${ITK_DATA_ROOT}/Input/dicom-sc_cs-1.dcm}
    ${ITK_TEST_OUTPUT_DIR}/itkDCMTKImageIOTest.dcm
    ${ITK_TEST_OUTPUT_DIR}/itkDCMTKImageIOTest.png
    ${ITK_TEST_OUTPUT_DIR}/itkDCMTKRescaleImageIOTest.dcm
)

itk_add_test(
  NAME itkDCMTKImageIOTest2
  COMMAND
    ITKIODCMTKTestDriver
    itkDCMTKImageIOTest
    DATA{${ITK_DATA_ROOT}/Input/itkGDCMImageIOTest.dcm}
    ${ITK_TEST_OUTPUT_DIR}/itkDCMTKImageIOTest2.dcm
    ${ITK_TEST_OUTPUT_DIR}/itkDCMTKImageIOTest2.png
    ${ITK_TEST_OUTPUT_DIR}/itkDCMTKRescaleImageIOTest2.dcm
)

itk_add_test(
  NAME itkDCMTKImageIOTest3
  COMMAND
    ITKIODCMTKTestDriver
    itkDCMTKImageIOTest
    DATA{${ITK_DATA_ROOT}/Input/012345.002.050}
    ${ITK_TEST_OUTPUT_DIR}/itkDCMTKImageIOTest3.dcm
    ${ITK_TEST_OUTPUT_DIR}/itkDCMTKImageIOTest3.png
    ${ITK_TEST_OUTPUT_DIR}/itkDCMTKRescaleImageIOTest3.dcm
)

itk_add_test(
  NAME itkDCMTKImageIOTest4
  COMMAND
    ITKIODCMTKTestDriver
    itkDCMTKImageIOTest
    DATA{${ITK_DATA_ROOT}/Input/itkGDCMImageIOTest3.dcm}
    ${ITK_TEST_OUTPUT_DIR}/itkDCMTKImageIOTest4.dcm
    ${ITK_TEST_OUTPUT_DIR}/itkDCMTKImageIOTest4.png
    ${ITK_TEST_OUTPUT_DIR}/itkDCMTKRescaleImageIOTest4.dcm
)

itk_add_test(
  NAME itkDCMTKImageIOTest5
  COMMAND
    ITKIODCMTKTestDriver
    itkDCMTKImageIOTest
    DATA{Input/preamble.dcm}
    ${ITK_TEST_OUTPUT_DIR}/itkDCMTKImageIOTest5.dcm
    ${ITK_TEST_OUTPUT_DIR}/itkDCMTKImageIOTest5.png
    ${ITK_TEST_OUTPUT_DIR}/itkDCMTKRescaleImageIOTest5.dcm
)

itk_add_test(
  NAME itkDCMTKImageIOTest6
  COMMAND
    ITKIODCMTKTestDriver
    itkDCMTKImageIOTest
    DATA{Input/no_preamble.dcm}
    ${ITK_TEST_OUTPUT_DIR}/itkDCMTKImageIOTest6.dcm
    ${ITK_TEST_OUTPUT_DIR}/itkDCMTKImageIOTest6.png
    ${ITK_TEST_OUTPUT_DIR}/itkDCMTKRescaleImageIOTest6.dcm
)

itk_add_test(
  NAME itkDCMTKSeriesReadImageWrite
  COMMAND
    ITKIODCMTKTestDriver
    itkDCMTKSeriesReadImageWrite
    DATA{${ITK_DATA_ROOT}/Input/DicomSeries/,REGEX:Image[0-9]+.dcm}
    ${ITK_TEST_OUTPUT_DIR}/itkDCMTKSeriesReadImageWrite.vtk
    0
    0
    0
)

itk_add_test(
  NAME itkDCMTKDirCosinesTest
  COMMAND
    ITKIODCMTKTestDriver
    itkDCMTKSeriesReadImageWrite
    DATA{${ITK_DATA_ROOT}/Input/DicomDirectionsTest/,REGEX:1020_[0-9]+.dcm}
    ${ITK_TEST_OUTPUT_DIR}/itkDCMTKSeriesReadImageWrite.vtk
    0
    0
    0
)

set_property(
  TEST
    itkDCMTKSeriesReadImageWrite
  APPEND
  PROPERTY
    DEPENDS
      ITKData
)

itk_add_test(
  NAME itkDCMTKSeriesStreamReadImageWrite1
  COMMAND
    ITKIODCMTKTestDriver
    itkDCMTKSeriesStreamReadImageWrite
    DATA{${ITK_DATA_ROOT}/Input/DicomSeries/,REGEX:Image[0-9]+.dcm}
    ${ITK_TEST_OUTPUT_DIR}/itkDCMTKSeriesStreamReadImageWrite1.mhd
    0.859375
    0.85939
    1.60016
    0
)

itk_add_test(
  NAME itkDCMTKSeriesImageOrientationWrite1
  COMMAND
    ITKIODCMTKTestDriver
    --compareCoordinateTolerance
    1e-3
    --compare
    DATA{Baseline/itkDCMTKSeriesImageOrientationWrite1.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkDCMTKSeriesImageOrientationWrite1.mha
    itkDCMTKSeriesReadImageWrite
    DATA{${ITK_DATA_ROOT}/Input/DicomImageOrientationTest/,REGEX:ImageOrientation.[0-9]+.dcm}
    ${ITK_TEST_OUTPUT_DIR}/itkDCMTKSeriesImageOrientationWrite1.mha
    0
    0
    0
)

itk_add_test(
  NAME itkDCMTKImageIOOrthoDirTest
  COMMAND
    ITKIODCMTKTestDriver
    itkDCMTKImageIOOrthoDirTest
    DATA{${ITK_DATA_ROOT}/Input/OrthogonalDirectionsTest.dcm}
)

itk_add_test(
  NAME itkDCMTKMultiFrame4DTest
  COMMAND
    ITKIODCMTKTestDriver
    itkDCMTKMultiFrame4DTest
    DATA{${ITK_DATA_ROOT}/Input/Philips_4D_DICOM.dcm}
    ${ITK_TEST_OUTPUT_DIR}/Philips_4D_DICOM.nrrd
)

itk_add_test(
  NAME itkDCMTKImageIONonSquareTest
  COMMAND
    ITKIODCMTKTestDriver
    --compare
    DATA{Baseline/itkDCMTKNonSquare.nii.gz}
    ${ITK_TEST_OUTPUT_DIR}/itkDCMTKNonSquare.nii.gz
    itkDCMTKImageIOTest
    DATA{${ITK_DATA_ROOT}/Input/itkDCMTKNonSquare.dcm}
    ${ITK_TEST_OUTPUT_DIR}/itkDCMTKNonSquare.nii.gz
    ${ITK_TEST_OUTPUT_DIR}/itkDCMTKNonSquare.nii.gz
    ${ITK_TEST_OUTPUT_DIR}/itkDCMTKNonSquareRescale.dcm
)

itk_add_test(
  NAME itkDCMTKRGBImageIOColorImageTest
  COMMAND
    ITKIODCMTKTestDriver
    --compare
    DATA{Baseline/itkDCMTKColorImage.png}
    ${ITK_TEST_OUTPUT_DIR}/itkDCMTKColorImage.png
    itkDCMTKRGBImageIOTest
    DATA{${ITK_DATA_ROOT}/Input/RGBDicomTest.dcm}
    ${ITK_TEST_OUTPUT_DIR}/itkDCMTKColorImage.png
)

itk_add_test(
  NAME itkDCMTKRGBImageIOColorImageSpatialMetadataTest
  COMMAND
    ITKIODCMTKTestDriver
    --compare
    DATA{Baseline/itkDCMTKColorImageSpatialMetadata.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/itkDCMTKColorImageSpatialMetadata.nrrd
    itkDCMTKRGBImageIOTest
    DATA{Input/visible-male-rgb-slice.dcm}
    ${ITK_TEST_OUTPUT_DIR}/itkDCMTKColorImageSpatialMetadata.nrrd
)

itk_add_test(
  NAME ITKDCMTKLoggerTest
  COMMAND
    ITKIODCMTKTestDriver
    itkDCMTKLoggerTest
)

itk_add_test(
  NAME itkDCMTKImageIOSlopeInterceptTest
  COMMAND
    ITKIODCMTKTestDriver
    itkDCMTKImageIOSlopeInterceptTest
    DATA{Input/slopeInterceptOrig.dcm}
    DATA{Input/slopeIntercept.dcm}
)

# Requires additional logic in DCMTKFileReader::GetSpacing
#itk_add_test(NAME itkDCMTKImageIOMultiFrameImageTest
#  COMMAND ITKIODCMTKTestDriver itkDCMTKImageIOMultiFrameImageTest
#  DATA{Input/MultiFrameDicomTest.dcm}
#  )

itk_add_test(
  NAME itkDCMTKImageIONoPreambleTest
  COMMAND
    ITKIODCMTKTestDriver
    itkDCMTKImageIONoPreambleTest
    DATA{Input/NoPreambleDicomTest.dcm}
)

# Re-use the test driver to ensure that CanReadFile returns true for a file
# with a preamble.
itk_add_test(
  NAME itkDCMTKImageIOPreambleCanReadTest
  COMMAND
    ITKIODCMTKTestDriver
    itkDCMTKImageIONoPreambleTest
    DATA{Input/preamble.dcm}
)

itk_add_test(
  NAME itkDCMTKImageIOSpacingTest
  COMMAND
    ITKIODCMTKTestDriver
    itkDCMTKImageIOSpacingTest
    DATA{Input/MultiFrameMRIZSpacing.dcm}
)
