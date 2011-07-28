itk_module_test()
set(ITKIOBaseTests
itkConvertBufferTest.cxx
itkConvertBufferTest2.cxx
itkImageFileReaderTest1.cxx
itkImageFileWriterTest.cxx
itkIOCommonTest.cxx
itkNumericSeriesFileNamesTest.cxx
itkRegularExpressionSeriesFileNamesTest.cxx
itkArchetypeSeriesFileNamesTest.cxx
itkLargeImageWriteConvertReadTest.cxx
itkLargeImageWriteReadTest.cxx
itkImageFileReaderDimensionsTest.cxx
itkImageFileReaderStreamingTest.cxx
itkImageFileReaderStreamingTest2.cxx
itkImageFileWriterPastingTest1.cxx
itkImageFileWriterPastingTest2.cxx
itkImageFileWriterPastingTest3.cxx
itkImageFileWriterStreamingPastingCompressingTest1.cxx
itkImageFileWriterStreamingTest1.cxx
itkImageFileWriterStreamingTest2.cxx
itkImageFileWriterTest2.cxx
itkImageFileWriterUpdateLargestPossibleRegionTest.cxx
itkImageIOBaseTest.cxx
itkImageIODirection2DTest.cxx
itkImageIODirection3DTest.cxx
itkImageIOFileNameExtensionsTests.cxx
itkImageSeriesReaderDimensionsTest.cxx
itkImageSeriesReaderVectorTest.cxx
itkImageSeriesWriterTest.cxx
itkIOPluginTest.cxx
itkNoiseImageFilterTest.cxx
itkMatrixImageWriteReadTest.cxx
itkReadWriteImageWithDictionaryTest.cxx
itkVectorImageReadWriteTest.cxx
)


CreateTestDriver(ITKIOBase  "${ITKIOBase-Test_LIBRARIES}" "${ITKIOBaseTests}")
itk_add_test(NAME itkArchetypeSeriesFileNamesTest
      COMMAND ITKIOBaseTestDriver
    itkArchetypeSeriesFileNamesTest
    ${ITK_DATA_ROOT}/Input/Archetype/a5b7.PNG
    ${ITK_DATA_ROOT}/Input/Archetype/a7b1.PNG)
itk_add_test(NAME itkArchetypeSeriesFileNamesTest2
      COMMAND ITKIOBaseTestDriver
    itkArchetypeSeriesFileNamesTest
    ${ITK_DATA_ROOT}/Input/Archetype/image.001
    ${ITK_DATA_ROOT}/Input/Archetype/image.010)
itk_add_test(NAME itkConvertBufferTest
      COMMAND ITKIOBaseTestDriver itkConvertBufferTest)
itk_add_test(NAME itkConvertBufferTest2
      COMMAND ITKIOBaseTestDriver itkConvertBufferTest2)
itk_add_test(NAME itkImageFileReaderTest1
      COMMAND ITKIOBaseTestDriver itkImageFileReaderTest1)
itk_add_test(NAME itkImageFileWriterTest
      COMMAND ITKIOBaseTestDriver itkImageFileWriterTest
              ${ITK_TEST_OUTPUT_DIR}/test.png)
itk_add_test(NAME itkIOCommonTest
      COMMAND ITKIOBaseTestDriver itkIOCommonTest)
itk_add_test(NAME itkNumericSeriesFileNamesTest
      COMMAND ITKIOBaseTestDriver itkNumericSeriesFileNamesTest)
itk_add_test(NAME itkRegularExpressionSeriesFileNamesTest
      COMMAND ITKIOBaseTestDriver itkRegularExpressionSeriesFileNamesTest
              ${ITK_DATA_ROOT}/Input)
itk_add_test(NAME itkImageFileReaderDimensionsTest_MHD
      COMMAND ITKIOBaseTestDriver itkImageFileReaderDimensionsTest
              ${ITK_DATA_ROOT}/Input/HeadMRVolume.mha ${ITK_TEST_OUTPUT_DIR} mha)
itk_add_test(NAME itkImageFileReaderDimensionsTest_NRRD
      COMMAND ITKIOBaseTestDriver itkImageFileReaderDimensionsTest
              ${ITK_DATA_ROOT}/Input/vol-ascii.nrrd ${ITK_TEST_OUTPUT_DIR} nrrd)
itk_add_test(NAME itkImageFileReaderStreamingTest_1
      COMMAND ITKIOBaseTestDriver itkImageFileReaderStreamingTest
              ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd 1 0)
itk_add_test(NAME itkImageFileReaderStreamingTest_2
      COMMAND ITKIOBaseTestDriver itkImageFileReaderStreamingTest
              ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd 0 1)
itk_add_test(NAME itkImageFileReaderStreamingTest_3
      COMMAND ITKIOBaseTestDriver itkImageFileReaderStreamingTest
              ${ITK_DATA_ROOT}/Input/vol-ascii.nrrd 0 0)
itk_add_test(NAME itkImageFileReaderStreamingTest2_MHD
      COMMAND ITKIOBaseTestDriver itkImageFileReaderStreamingTest2
              ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd)
itk_add_test(NAME itkImageFileWriterPastingTest1
      COMMAND ITKIOBaseTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd
              ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterPastingTest1_01.mha
    itkImageFileWriterPastingTest1 ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterPastingTest1_01.mha)
itk_add_test(NAME itkImageFileWriterPastingTest2_5
      COMMAND ITKIOBaseTestDriver itkImageFileWriterPastingTest2
              ${ITK_DATA_ROOT}/Input/HeadMRVolume.mha ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterPastingTest2_5.mha)
itk_add_test(NAME itkImageFileWriterPastingTest2_6
      COMMAND ITKIOBaseTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd
              ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterPastingTest2_6.mha
    itkImageFileWriterPastingTest2 ${ITK_DATA_ROOT}/Input/HeadMRVolume.mha ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterPastingTest2_6.mha ${ITK_DATA_ROOT}/Input/HeadMRVolume.mha)
itk_add_test(NAME itkImageFileWriterPastingTest2_7
      COMMAND ITKIOBaseTestDriver itkImageFileWriterPastingTest2
              ${ITK_DATA_ROOT}/Input/HeadMRVolume.mha ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterPastingTest2_7.mha ${ITK_DATA_ROOT}/Input/HeadMRVolumeCompressed.mha)
itk_add_test(NAME itkImageFileWriterPastingTest3
      COMMAND ITKIOBaseTestDriver
    itkImageFileWriterPastingTest3
            ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd
            ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterPastingTest3_01.mha)
itk_add_test(NAME itkImageFileWriterStreamingPastingCompressingTest_MHA
      COMMAND ITKIOBaseTestDriver itkImageFileWriterStreamingPastingCompressingTest1
              ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreamingPastingCompressingTest mha 0 0 0 1 0 0 0 1)
itk_add_test(NAME itkImageFileWriterStreamingPastingCompressingTest_NRRD
      COMMAND ITKIOBaseTestDriver itkImageFileWriterStreamingPastingCompressingTest1
              ${ITK_DATA_ROOT}/Input/vol-ascii.nrrd ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreamingPastingCompressingTest nrrd 0 0 1 1 0 0 1 1)
itk_add_test(NAME itkImageFileWriterStreamingPastingCompressingTest_NHDR
      COMMAND ITKIOBaseTestDriver itkImageFileWriterStreamingPastingCompressingTest1
              ${ITK_DATA_ROOT}/Input/vol-ascii.nrrd ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreamingPastingCompressingTest nhdr 0 0 1 1 0 0 1 1)
itk_add_test(NAME itkImageFileWriterStreamingPastingCompressingTest_VTK
      COMMAND ITKIOBaseTestDriver
    --compare ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd
              ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreamingPastingCompressingTest000.vtk
    itkImageFileWriterStreamingPastingCompressingTest1 ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreamingPastingCompressingTest vtk 0 0 0 0 0 0 0 0)

# JIRA ITK182
# This test has been disabled due to the above bug. It should be
# enabled when it is resolved and to confirm the above test is
# working.
#
#itk_add_test(NAME itkImageFileWriterStreamingPastingCompressingTest_DCM
#      COMMAND ITKIOBaseTestDriver itkImageFileWriterStreamingPastingCompressingTest1
#              ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreamingPastingCompressingTest dcm 0 0 1 1 0 0 1 1)
itk_add_test(NAME itkImageFileWriterStreamingTest1_1
      COMMAND ITKIOBaseTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd
              ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreaming1_1.mha
    itkImageFileWriterStreamingTest1 ${ITK_DATA_ROOT}/Input/HeadMRVolume.mha ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreaming1_1.mha)
itk_add_test(NAME itkImageFileWriterStreamingTest1_2
      COMMAND ITKIOBaseTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd
              ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreaming1_2.mha
    itkImageFileWriterStreamingTest1 ${ITK_DATA_ROOT}/Input/HeadMRVolume.mha ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreaming1_2.mha ${ITK_DATA_ROOT}/Input/HeadMRVolumeCompressed.mha 0)
itk_add_test(NAME itkImageFileWriterStreamingTest1_3
      COMMAND ITKIOBaseTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd
              ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreaming1_3.mha
    itkImageFileWriterStreamingTest1 ${ITK_DATA_ROOT}/Input/HeadMRVolume.mha ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreaming1_3.mha ${ITK_DATA_ROOT}/Input/HeadMRVolume.mha 1)
itk_add_test(NAME itkImageFileWriterStreamingTest2_4
      COMMAND ITKIOBaseTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd
              ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreaming2_4.mha
    itkImageFileWriterStreamingTest2 ${ITK_DATA_ROOT}/Input/HeadMRVolume.mha ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreaming2_4.mha)
itk_add_test(NAME itkImageFileWriterTest2_1
      COMMAND ITKIOBaseTestDriver itkImageFileWriterTest2
              ${ITK_TEST_OUTPUT_DIR}/test.nrrd)
itk_add_test(NAME itkImageFileWriterTest2_2
      COMMAND ITKIOBaseTestDriver itkImageFileWriterTest2
              ${ITK_TEST_OUTPUT_DIR}/test.mha)
itk_add_test(NAME itkImageFileWriterTest2_3
      COMMAND ITKIOBaseTestDriver itkImageFileWriterTest2
              ${ITK_TEST_OUTPUT_DIR}/test.vtk)
itk_add_test(NAME itkImageFileWriterUpdateLargestPossibleRegionTest
      COMMAND ITKIOBaseTestDriver
    --compare ${ITK_DATA_ROOT}/Input/cthead1.png
              ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterUpdateLargestPossibleRegionTest.png
    itkImageFileWriterUpdateLargestPossibleRegionTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterUpdateLargestPossibleRegionTest.png)
itk_add_test(NAME itkImageIOBaseTest
      COMMAND ITKIOBaseTestDriver itkImageIOBaseTest)
itk_add_test(NAME itkImageIODirection2DTest01
      COMMAND ITKIOBaseTestDriver itkImageIODirection2DTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png 1.0 0.0 0.0 1.0 ${ITK_TEST_OUTPUT_DIR}/BrainProtonDensitySliceBorder20.mhd)
itk_add_test(NAME itkImageIODirection2DTest02
      COMMAND ITKIOBaseTestDriver itkImageIODirection2DTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceShifted13x17y.png 1.0 0.0 0.0 1.0 ${ITK_TEST_OUTPUT_DIR}/BrainProtonDensitySliceShifted13x17y.mhd)
itk_add_test(NAME itkImageIODirection2DTest03
      COMMAND ITKIOBaseTestDriver itkImageIODirection2DTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20DirectionPlus30.mhd 0.8660254 -0.5 0.5 0.8660254 ${ITK_TEST_OUTPUT_DIR}/BrainProtonDensitySliceBorder20DirectionPlus30.mhd)
itk_add_test(NAME itkImageIODirection2DTest04
      COMMAND ITKIOBaseTestDriver itkImageIODirection2DTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceShifted13x17yDirectionPlus30.mhd 0.8660254 -0.5 0.5 0.8660254 ${ITK_TEST_OUTPUT_DIR}/BrainProtonDensitySliceShifted13x17yDirectionPlus30.mhd)
itk_add_test(NAME itkImageIODirection2DTest05
      COMMAND ITKIOBaseTestDriver itkImageIODirection2DTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainT1SliceBorder20DirectionPlus30.mhd 0.8660254 -0.5 0.5 0.8660254 ${ITK_TEST_OUTPUT_DIR}/BrainT1SliceBorder20DirectionPlus30.mhd)
itk_add_test(NAME itkImageIODirection2DTest06
      COMMAND ITKIOBaseTestDriver itkImageIODirection2DTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.mhd 1.0 0.0 0.0 1.0 ${ITK_TEST_OUTPUT_DIR}/BrainProtonDensitySliceBorder20.nhdr)
itk_add_test(NAME itkImageIODirection2DTest07
      COMMAND ITKIOBaseTestDriver itkImageIODirection2DTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceShifted13x17y.mhd 1.0 0.0 0.0 1.0 ${ITK_TEST_OUTPUT_DIR}/BrainProtonDensitySliceShifted13x17y.nhdr)
itk_add_test(NAME itkImageIODirection2DTest08
      COMMAND ITKIOBaseTestDriver itkImageIODirection2DTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20DirectionPlus30.mhd 0.8660254 -0.5 0.5 0.8660254 ${ITK_TEST_OUTPUT_DIR}/BrainProtonDensitySliceBorder20DirectionPlus30.nhdr)
itk_add_test(NAME itkImageIODirection2DTest09
      COMMAND ITKIOBaseTestDriver itkImageIODirection2DTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceShifted13x17yDirectionPlus30.mhd 0.8660254 -0.5 0.5 0.8660254 ${ITK_TEST_OUTPUT_DIR}/BrainProtonDensitySliceShifted13x17yDirectionPlus30.nhdr)
itk_add_test(NAME itkImageIODirection2DTest10
      COMMAND ITKIOBaseTestDriver itkImageIODirection2DTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainT1SliceBorder20DirectionPlus30.mhd 0.8660254 -0.5 0.5 0.8660254 ${ITK_TEST_OUTPUT_DIR}/BrainT1SliceBorder20DirectionPlus30.nhdr)
itk_add_test(NAME itkImageIODirection2DTest11
      COMMAND ITKIOBaseTestDriver itkImageIODirection2DTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.nhdr 1.0 0.0 0.0 1.0)
itk_add_test(NAME itkImageIODirection2DTest12
      COMMAND ITKIOBaseTestDriver itkImageIODirection2DTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceShifted13x17y.nhdr 1.0 0.0 0.0 1.0)
itk_add_test(NAME itkImageIODirection2DTest13
      COMMAND ITKIOBaseTestDriver itkImageIODirection2DTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20DirectionPlus30.nhdr 0.8660254 -0.5 0.5 0.8660254)
itk_add_test(NAME itkImageIODirection2DTest14
      COMMAND ITKIOBaseTestDriver itkImageIODirection2DTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceShifted13x17yDirectionPlus30.nhdr 0.8660254 -0.5 0.5 0.8660254)
itk_add_test(NAME itkImageIODirection2DTest15
      COMMAND ITKIOBaseTestDriver itkImageIODirection2DTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainT1SliceBorder20DirectionPlus30.nhdr 0.8660254 -0.5 0.5 0.8660254)
itk_add_test(NAME itkImageIODirection3DTest01
      COMMAND ITKIOBaseTestDriver itkImageIODirection3DTest
              ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirectionIdentity.mhd 1.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 1.0 ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeWithDirectionIdentity.mhd)
itk_add_test(NAME itkImageIODirection3DTest02
      COMMAND ITKIOBaseTestDriver itkImageIODirection3DTest
              ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection.mhd 0.8660254 0.5 0.0 -0.5 0.8660254 0.0 0.0 0.0 1.0 ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeWithDirection.mhd)
itk_add_test(NAME itkImageIODirection3DTest03
      COMMAND ITKIOBaseTestDriver itkImageIODirection3DTest
              ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirectionIdentity.mhd 1.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 1.0 ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeWithDirectionIdentity.nhdr)
itk_add_test(NAME itkImageIODirection3DTest04
      COMMAND ITKIOBaseTestDriver itkImageIODirection3DTest
              ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection.mhd 0.8660254 0.5 0.0 -0.5 0.8660254 0.0 0.0 0.0 1.0 ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeWithDirection.nhdr)
itk_add_test(NAME itkImageIODirection3DTest05
      COMMAND ITKIOBaseTestDriver itkImageIODirection3DTest
              ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirectionIdentity.nhdr 1.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 1.0)
itk_add_test(NAME itkImageIODirection3DTest06
      COMMAND ITKIOBaseTestDriver itkImageIODirection3DTest
              ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection.nhdr 0.8660254 0.5 0.0 -0.5 0.8660254 0.0 0.0 0.0 1.0)
itk_add_test(NAME itkImageIODirection3DTest07
      COMMAND ITKIOBaseTestDriver itkImageIODirection3DTest
              ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection001.mhd 1.0 0.0 0.0 0.0 -1.0 0.0 0.0 0.0 1.0 ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeWithDirection001.nhdr)
itk_add_test(NAME itkImageIODirection3DTest08
      COMMAND ITKIOBaseTestDriver itkImageIODirection3DTest
              ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection002.mhd 1.0 0.0 0.0 0.0 0.0 01.0 0.0 -1.0 0.0 ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeWithDirection002.nhdr)
itk_add_test(NAME itkImageIODirection3DTest09
      COMMAND ITKIOBaseTestDriver itkImageIODirection3DTest
              ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection003.mhd 0.0 -1.0 0.0 0.0 0.0 1.0 1.0 0.0 0.0 ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeWithDirection003.nhdr)
itk_add_test(NAME itkImageIOFileNameExtensionsTests
      COMMAND ITKIOBaseTestDriver itkImageIOFileNameExtensionsTests)
itk_add_test(NAME itkImageSeriesReaderDimensionsTest1
      COMMAND ITKIOBaseTestDriver itkImageSeriesReaderDimensionsTest
              ${ITK_DATA_ROOT}/Input/DicomSeries/Image0075.dcm ${ITK_DATA_ROOT}/Input/DicomSeries/Image0076.dcm ${ITK_DATA_ROOT}/Input/DicomSeries/Image0077.dcm)
itk_add_test(NAME itkImageSeriesReaderDimensionsTest2
      COMMAND ITKIOBaseTestDriver itkImageSeriesReaderDimensionsTest
              ${ITK_DATA_ROOT}/Input/cthead1.tif ${ITK_DATA_ROOT}/Input/cthead1.tif ${ITK_DATA_ROOT}/Input/cthead1.tif)
itk_add_test(NAME itkImageSeriesReaderVectorImageTest1
   COMMAND ITKIOBaseTestDriver itkImageSeriesReaderVectorTest
   ${ITK_DATA_ROOT}/Input/RGBTestImage.tif ${ITK_DATA_ROOT}/Input/RGBTestImage.tif ${ITK_DATA_ROOT}/Input/RGBTestImage.tif )
itk_add_test(NAME itkImageSeriesReaderVectorImageTest2
   COMMAND ITKIOBaseTestDriver itkImageSeriesReaderVectorTest
   ${ITK_DATA_ROOT}/Input/48BitTestImage.tif ${ITK_DATA_ROOT}/Input/48BitTestImage.tif ${ITK_DATA_ROOT}/Input/48BitTestImage.tif )
itk_add_test(NAME itkImageSeriesWriterTest
      COMMAND ITKIOBaseTestDriver itkImageSeriesWriterTest
              ${ITK_DATA_ROOT}/Input/DicomSeries ${ITK_TEST_OUTPUT_DIR} png)

if(ITK_BUILD_SHARED_LIBS)
  itk_add_test(NAME itkIOPluginTest
           COMMAND ITKIOBaseTestDriver itkIOPluginTest
             ${ITK_TEST_OUTPUT_DIR}
             "FileFreeIO::Size=128,256:Spacing=.5,.8:Origin=5,6:Direction=-1,0,0,-1"
             ${ITK_TEST_OUTPUT_DIR}/itkIOPluginTest.png
    )
  ## Create a library to test ITK IO plugins
  set(FileFreeTest_Source itkFileFreeImageIO.cxx itkFileFreeImageIOFactory.cxx)
  add_library(FileFreeTestLib MODULE ${FileFreeTest_Source})
  itk_module_target_label(FileFreeTestLib)
  target_link_libraries(FileFreeTestLib ${ITKIOBase_LIBRARIES})
  set_property(TARGET FileFreeTestLib PROPERTY LIBRARY_OUTPUT_DIRECTORY ${ITK_TEST_OUTPUT_DIR})
  add_dependencies(ITKIOBaseTestDriver FileFreeTestLib)
endif(ITK_BUILD_SHARED_LIBS)
itk_add_test(NAME itkNoiseImageFilterTest
      COMMAND ITKIOBaseTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkNoiseImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/itkNoiseImageFilterTest.png
    itkNoiseImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkNoiseImageFilterTest.png)
itk_add_test(NAME itkMatrixImageWriteReadTest
      COMMAND ITKIOBaseTestDriver itkMatrixImageWriteReadTest
              ${ITK_TEST_OUTPUT_DIR}/testMatrix1.mha)
itk_add_test(NAME itkReadWriteImageWithDictionaryTest
      COMMAND ITKIOBaseTestDriver itkReadWriteImageWithDictionaryTest
              ${ITK_TEST_OUTPUT_DIR}/test.hdr)
itk_add_test(NAME itkReadWriteImageWithDictionaryTest1
      COMMAND ITKIOBaseTestDriver itkReadWriteImageWithDictionaryTest
              ${ITK_TEST_OUTPUT_DIR}/test.mha)
itk_add_test(NAME itkVectorImageReadWriteTest
      COMMAND ITKIOBaseTestDriver itkVectorImageReadWriteTest
              ${ITK_TEST_OUTPUT_DIR}/VectorImageReadWriteTest.mhd)
itk_add_test(NAME itkVectorImageReadWriteTest2
      COMMAND ITKIOBaseTestDriver itkVectorImageReadWriteTest
              ${ITK_TEST_OUTPUT_DIR}/VectorImageReadWriteTest.nrrd)

add_executable(itkUnicodeIOTest itkUnicodeIOTest.cxx)
itk_module_target_label(itkUnicodeIOTest)
itk_add_test(NAME itkUnicodeIOTest COMMAND itkUnicodeIOTest)
