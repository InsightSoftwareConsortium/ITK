itk_module_test()
set(ITKIOImageBaseTests
itkConvertBufferTest.cxx
itkConvertBufferTest2.cxx
itkImageFileReaderTest1.cxx
itkImageFileWriterTest.cxx
itkIOCommonTest.cxx
itkIOCommonTest2.cxx
itkNumericSeriesFileNamesTest.cxx
itkRegularExpressionSeriesFileNamesTest.cxx
itkArchetypeSeriesFileNamesTest.cxx
itkLargeImageWriteConvertReadTest.cxx
itkLargeImageWriteReadTest.cxx
itkImageFileReaderDimensionsTest.cxx
itkImageFileReaderPositiveSpacingTest.cxx
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
itk64bitTest.cxx
)


CreateTestDriver(ITKIOImageBase  "${ITKIOImageBase-Test_LIBRARIES}" "${ITKIOImageBaseTests}")
itk_add_test(NAME itkArchetypeSeriesFileNamesTest
      COMMAND ITKIOImageBaseTestDriver
    itkArchetypeSeriesFileNamesTest
    DATA{${ITK_DATA_ROOT}/Input/Archetype/a5b7.PNG,REGEX:a[0-9]b[0-9]\\.PNG}
    DATA{${ITK_DATA_ROOT}/Input/Archetype/a7b1.PNG})
itk_add_test(NAME itkArchetypeSeriesFileNamesTest2
      COMMAND ITKIOImageBaseTestDriver
    itkArchetypeSeriesFileNamesTest
    DATA{${ITK_DATA_ROOT}/Input/Archetype/image.001,REGEX:image\\.[0-9]+}
    DATA{${ITK_DATA_ROOT}/Input/Archetype/image.010})
itk_add_test(NAME itkConvertBufferTest
      COMMAND ITKIOImageBaseTestDriver itkConvertBufferTest)
itk_add_test(NAME itkConvertBufferTest2
      COMMAND ITKIOImageBaseTestDriver itkConvertBufferTest2)
itk_add_test(NAME itkImageFileReaderTest1
      COMMAND ITKIOImageBaseTestDriver itkImageFileReaderTest1)
itk_add_test(NAME itkImageFileWriterTest
      COMMAND ITKIOImageBaseTestDriver itkImageFileWriterTest
              ${ITK_TEST_OUTPUT_DIR}/test.png)
itk_add_test(NAME itkIOCommonTest
      COMMAND ITKIOImageBaseTestDriver itkIOCommonTest)
itk_add_test(NAME itkIOCommonTest2
      COMMAND ITKIOImageBaseTestDriver itkIOCommonTest2)
itk_add_test(NAME itkNumericSeriesFileNamesTest
      COMMAND ITKIOImageBaseTestDriver itkNumericSeriesFileNamesTest)
itk_add_test(NAME itk64bitTestNRRDtoMHA
      COMMAND ITKIOImageBaseTestDriver --compare DATA{Input/Test64bit.nrrd} ${ITK_TEST_OUTPUT_DIR}/Test64bit.mha
      itk64bitTest DATA{Input/Test64bit.nrrd} ${ITK_TEST_OUTPUT_DIR}/Test64bit.mha)
itk_add_test(NAME itk64bitTestMHAtoNRRD
      COMMAND ITKIOImageBaseTestDriver --compare DATA{Input/Test64bit.mha} ${ITK_TEST_OUTPUT_DIR}/Test64bit.nrrd
      itk64bitTest DATA{Input/Test64bit.mha} ${ITK_TEST_OUTPUT_DIR}/Test64bit.nrrd)
itk_add_test(NAME itk64bitTestNRRDtoMHA2
      COMMAND ITKIOImageBaseTestDriver --compare DATA{Input/Test64bit.nrrd} ${ITK_TEST_OUTPUT_DIR}/Test64bit2.mha
      itk64bitTest DATA{Input/Test64bit.mha} ${ITK_TEST_OUTPUT_DIR}/Test64bit2.mha)
itk_add_test(NAME itk64bitTestNRRDtoNIFTI
      COMMAND ITKIOImageBaseTestDriver --compare DATA{Input/Test64bit.nrrd} ${ITK_TEST_OUTPUT_DIR}/Test64bit.nii
      itk64bitTest DATA{Input/Test64bit.nrrd} ${ITK_TEST_OUTPUT_DIR}/Test64bit.nii)
itk_add_test(NAME itk64bitTestNRRDtoVTK
      COMMAND ITKIOImageBaseTestDriver --ignoreInputInformation --compare DATA{Input/Test64bit.nrrd} ${ITK_TEST_OUTPUT_DIR}/Test64bit.vtk
      itk64bitTest DATA{Input/Test64bit.nrrd} ${ITK_TEST_OUTPUT_DIR}/Test64bit.vtk)

itk_add_test(NAME itkRegularExpressionSeriesFileNamesTest
      COMMAND ITKIOImageBaseTestDriver --redirectOutput ${ITK_TEST_OUTPUT_DIR}/itkRegularExpressionSeriesFileNamesTest.txt
    itkRegularExpressionSeriesFileNamesTest
              ${ITK_DATA_ROOT}/Input)
set_tests_properties(itkRegularExpressionSeriesFileNamesTest PROPERTIES ATTACHED_FILES_ON_FAIL ${ITK_TEST_OUTPUT_DIR}/itkRegularExpressionSeriesFileNamesTest.txt)


itk_add_test(NAME itkImageFileReaderDimensionsTest_MHD
      COMMAND ITKIOImageBaseTestDriver itkImageFileReaderDimensionsTest
              DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mha} ${ITK_TEST_OUTPUT_DIR} mha)
itk_add_test(NAME itkImageFileReaderDimensionsTest_NRRD
      COMMAND ITKIOImageBaseTestDriver itkImageFileReaderDimensionsTest
              DATA{${ITK_DATA_ROOT}/Input/vol-ascii.nrrd} ${ITK_TEST_OUTPUT_DIR} nrrd)
itk_add_test(NAME itkImageFileReaderStreamingTest_1
      COMMAND ITKIOImageBaseTestDriver itkImageFileReaderStreamingTest
              DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd,HeadMRVolume.raw} 1 0)
itk_add_test(NAME itkImageFileReaderStreamingTest_2
      COMMAND ITKIOImageBaseTestDriver itkImageFileReaderStreamingTest
              DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd,HeadMRVolume.raw} 0 1)
itk_add_test(NAME itkImageFileReaderStreamingTest_3
      COMMAND ITKIOImageBaseTestDriver itkImageFileReaderStreamingTest
              DATA{${ITK_DATA_ROOT}/Input/vol-ascii.nrrd} 0 0)
itk_add_test(NAME itkImageFileReaderStreamingTest2_MHD
      COMMAND ITKIOImageBaseTestDriver itkImageFileReaderStreamingTest2
              DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd,HeadMRVolume.raw})
itk_add_test(NAME itkImageFileWriterPastingTest1
      COMMAND ITKIOImageBaseTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd,HeadMRVolume.raw}
              ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterPastingTest1_01.mha
    itkImageFileWriterPastingTest1 DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd,HeadMRVolume.raw} ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterPastingTest1_01.mha)
itk_add_test(NAME itkImageFileWriterPastingTest2_5
      COMMAND ITKIOImageBaseTestDriver itkImageFileWriterPastingTest2
              DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mha} ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterPastingTest2_5.mha)
itk_add_test(NAME itkImageFileWriterPastingTest2_6
      COMMAND ITKIOImageBaseTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd,HeadMRVolume.raw}
              ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterPastingTest2_6.mha
    itkImageFileWriterPastingTest2 DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mha} ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterPastingTest2_6.mha DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mha})
itk_add_test(NAME itkImageFileWriterPastingTest2_7
      COMMAND ITKIOImageBaseTestDriver itkImageFileWriterPastingTest2
              DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mha} ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterPastingTest2_7.mha DATA{${ITK_DATA_ROOT}/Input/HeadMRVolumeCompressed.mha})
itk_add_test(NAME itkImageFileWriterPastingTest3
      COMMAND ITKIOImageBaseTestDriver
    itkImageFileWriterPastingTest3
            DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd,HeadMRVolume.raw}
            ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterPastingTest3_01.mha)
itk_add_test(NAME itkImageFileWriterStreamingPastingCompressingTest_MHA
      COMMAND ITKIOImageBaseTestDriver itkImageFileWriterStreamingPastingCompressingTest1
              DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd,HeadMRVolume.raw} ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreamingPastingCompressingTest mha 0 0 0 1 0 0 0 1)
itk_add_test(NAME itkImageFileWriterStreamingPastingCompressingTest_NRRD
      COMMAND ITKIOImageBaseTestDriver itkImageFileWriterStreamingPastingCompressingTest1
              DATA{${ITK_DATA_ROOT}/Input/vol-ascii.nrrd} ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreamingPastingCompressingTest nrrd 0 0 1 1 0 0 1 1)
itk_add_test(NAME itkImageFileWriterStreamingPastingCompressingTest_NHDR
      COMMAND ITKIOImageBaseTestDriver itkImageFileWriterStreamingPastingCompressingTest1
              DATA{${ITK_DATA_ROOT}/Input/vol-ascii.nrrd} ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreamingPastingCompressingTest nhdr 0 0 1 1 0 0 1 1)
itk_add_test(NAME itkImageFileWriterStreamingPastingCompressingTest_VTK
      COMMAND ITKIOImageBaseTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd,HeadMRVolume.raw}
              ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreamingPastingCompressingTest000.vtk
    itkImageFileWriterStreamingPastingCompressingTest1 DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd,HeadMRVolume.raw} ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreamingPastingCompressingTest vtk 0 0 0 0 0 0 0 0)

# JIRA ITK182
# This test has been disabled due to the above bug. It should be
# enabled when it is resolved and to confirm the above test is
# working.
#
#itk_add_test(NAME itkImageFileWriterStreamingPastingCompressingTest_DCM
#      COMMAND ITKIOImageBaseTestDriver itkImageFileWriterStreamingPastingCompressingTest1
#              DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd,HeadMRVolume.raw} ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreamingPastingCompressingTest dcm 0 0 1 1 0 0 1 1)
itk_add_test(NAME itkImageFileWriterStreamingTest1_1
      COMMAND ITKIOImageBaseTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd,HeadMRVolume.raw}
              ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreaming1_1.mha
    itkImageFileWriterStreamingTest1 DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mha} ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreaming1_1.mha)
itk_add_test(NAME itkImageFileWriterStreamingTest1_2
      COMMAND ITKIOImageBaseTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd,HeadMRVolume.raw}
              ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreaming1_2.mha
    itkImageFileWriterStreamingTest1 DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mha} ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreaming1_2.mha DATA{${ITK_DATA_ROOT}/Input/HeadMRVolumeCompressed.mha} 0)
itk_add_test(NAME itkImageFileWriterStreamingTest1_3
      COMMAND ITKIOImageBaseTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd,HeadMRVolume.raw}
              ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreaming1_3.mha
    itkImageFileWriterStreamingTest1 DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mha} ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreaming1_3.mha DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mha} 1)
itk_add_test(NAME itkImageFileWriterStreamingTest2_4
      COMMAND ITKIOImageBaseTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd,HeadMRVolume.raw}
              ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreaming2_4.mha
    itkImageFileWriterStreamingTest2 DATA{${ITK_DATA_ROOT}/Input/HeadMRVolume.mha} ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreaming2_4.mha)
itk_add_test(NAME itkImageFileWriterTest2_1
      COMMAND ITKIOImageBaseTestDriver itkImageFileWriterTest2
              ${ITK_TEST_OUTPUT_DIR}/test.nrrd)
itk_add_test(NAME itkImageFileWriterTest2_2
      COMMAND ITKIOImageBaseTestDriver itkImageFileWriterTest2
              ${ITK_TEST_OUTPUT_DIR}/test.mha)
itk_add_test(NAME itkImageFileWriterTest2_3
      COMMAND ITKIOImageBaseTestDriver itkImageFileWriterTest2
              ${ITK_TEST_OUTPUT_DIR}/test.vtk)
itk_add_test(NAME itkImageFileWriterUpdateLargestPossibleRegionTest
      COMMAND ITKIOImageBaseTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterUpdateLargestPossibleRegionTest.png
    itkImageFileWriterUpdateLargestPossibleRegionTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterUpdateLargestPossibleRegionTest.png)
itk_add_test(NAME itkImageIOBaseTest
      COMMAND ITKIOImageBaseTestDriver itkImageIOBaseTest)
itk_add_test(NAME itkImageIODirection2DTest01
      COMMAND ITKIOImageBaseTestDriver itkImageIODirection2DTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.png 1.0 0.0 0.0 1.0 ${ITK_TEST_OUTPUT_DIR}/BrainProtonDensitySliceBorder20.mhd)
itk_add_test(NAME itkImageIODirection2DTest02
      COMMAND ITKIOImageBaseTestDriver itkImageIODirection2DTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceShifted13x17y.png 1.0 0.0 0.0 1.0 ${ITK_TEST_OUTPUT_DIR}/BrainProtonDensitySliceShifted13x17y.mhd)
itk_add_test(NAME itkImageIODirection2DTest03
      COMMAND ITKIOImageBaseTestDriver itkImageIODirection2DTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20DirectionPlus30.mhd 0.8660254 -0.5 0.5 0.8660254 ${ITK_TEST_OUTPUT_DIR}/BrainProtonDensitySliceBorder20DirectionPlus30.mhd)
itk_add_test(NAME itkImageIODirection2DTest04
      COMMAND ITKIOImageBaseTestDriver itkImageIODirection2DTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceShifted13x17yDirectionPlus30.mhd 0.8660254 -0.5 0.5 0.8660254 ${ITK_TEST_OUTPUT_DIR}/BrainProtonDensitySliceShifted13x17yDirectionPlus30.mhd)
itk_add_test(NAME itkImageIODirection2DTest05
      COMMAND ITKIOImageBaseTestDriver itkImageIODirection2DTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainT1SliceBorder20DirectionPlus30.mhd 0.8660254 -0.5 0.5 0.8660254 ${ITK_TEST_OUTPUT_DIR}/BrainT1SliceBorder20DirectionPlus30.mhd)
itk_add_test(NAME itkImageIODirection2DTest06
      COMMAND ITKIOImageBaseTestDriver itkImageIODirection2DTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.mhd 1.0 0.0 0.0 1.0 ${ITK_TEST_OUTPUT_DIR}/BrainProtonDensitySliceBorder20.nhdr)
itk_add_test(NAME itkImageIODirection2DTest07
      COMMAND ITKIOImageBaseTestDriver itkImageIODirection2DTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceShifted13x17y.mhd 1.0 0.0 0.0 1.0 ${ITK_TEST_OUTPUT_DIR}/BrainProtonDensitySliceShifted13x17y.nhdr)
itk_add_test(NAME itkImageIODirection2DTest08
      COMMAND ITKIOImageBaseTestDriver itkImageIODirection2DTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20DirectionPlus30.mhd 0.8660254 -0.5 0.5 0.8660254 ${ITK_TEST_OUTPUT_DIR}/BrainProtonDensitySliceBorder20DirectionPlus30.nhdr)
itk_add_test(NAME itkImageIODirection2DTest09
      COMMAND ITKIOImageBaseTestDriver itkImageIODirection2DTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceShifted13x17yDirectionPlus30.mhd 0.8660254 -0.5 0.5 0.8660254 ${ITK_TEST_OUTPUT_DIR}/BrainProtonDensitySliceShifted13x17yDirectionPlus30.nhdr)
itk_add_test(NAME itkImageIODirection2DTest10
      COMMAND ITKIOImageBaseTestDriver itkImageIODirection2DTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainT1SliceBorder20DirectionPlus30.mhd 0.8660254 -0.5 0.5 0.8660254 ${ITK_TEST_OUTPUT_DIR}/BrainT1SliceBorder20DirectionPlus30.nhdr)
itk_add_test(NAME itkImageIODirection2DTest11
      COMMAND ITKIOImageBaseTestDriver itkImageIODirection2DTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20.nhdr 1.0 0.0 0.0 1.0)
itk_add_test(NAME itkImageIODirection2DTest12
      COMMAND ITKIOImageBaseTestDriver itkImageIODirection2DTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceShifted13x17y.nhdr 1.0 0.0 0.0 1.0)
itk_add_test(NAME itkImageIODirection2DTest13
      COMMAND ITKIOImageBaseTestDriver itkImageIODirection2DTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceBorder20DirectionPlus30.nhdr 0.8660254 -0.5 0.5 0.8660254)
itk_add_test(NAME itkImageIODirection2DTest14
      COMMAND ITKIOImageBaseTestDriver itkImageIODirection2DTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainProtonDensitySliceShifted13x17yDirectionPlus30.nhdr 0.8660254 -0.5 0.5 0.8660254)
itk_add_test(NAME itkImageIODirection2DTest15
      COMMAND ITKIOImageBaseTestDriver itkImageIODirection2DTest
              ${ITK_EXAMPLE_DATA_ROOT}/BrainT1SliceBorder20DirectionPlus30.nhdr 0.8660254 -0.5 0.5 0.8660254)
itk_add_test(NAME itkImageIODirection3DTest01
      COMMAND ITKIOImageBaseTestDriver itkImageIODirection3DTest
              DATA{${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirectionIdentity.mhd}
              1.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 1.0 ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeWithDirectionIdentity.mhd)
itk_add_test(NAME itkImageIODirection3DTest02
      COMMAND ITKIOImageBaseTestDriver itkImageIODirection3DTest
              DATA{${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection.mhd}
              0.8660254 0.5 0.0 -0.5 0.8660254 0.0 0.0 0.0 1.0 ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeWithDirection.mhd)
itk_add_test(NAME itkImageIODirection3DTest03
      COMMAND ITKIOImageBaseTestDriver itkImageIODirection3DTest
              DATA{${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirectionIdentity.mhd}
              1.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 1.0 ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeWithDirectionIdentity.nhdr)
itk_add_test(NAME itkImageIODirection3DTest04
      COMMAND ITKIOImageBaseTestDriver itkImageIODirection3DTest
              DATA{${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection.mhd}
              0.8660254 0.5 0.0 -0.5 0.8660254 0.0 0.0 0.0 1.0 ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeWithDirection.nhdr)
itk_add_test(NAME itkImageIODirection3DTest05
      COMMAND ITKIOImageBaseTestDriver itkImageIODirection3DTest
              DATA{${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirectionIdentity.nhdr,HeadMRVolumeWithDirectionIdentity.raw.gz}
              1.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 1.0)
itk_add_test(NAME itkImageIODirection3DTest06
      COMMAND ITKIOImageBaseTestDriver itkImageIODirection3DTest
              DATA{${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection.nhdr,HeadMRVolumeWithDirection.raw.gz}
              0.8660254 0.5 0.0 -0.5 0.8660254 0.0 0.0 0.0 1.0)
itk_add_test(NAME itkImageIODirection3DTest07
      COMMAND ITKIOImageBaseTestDriver itkImageIODirection3DTest
              DATA{${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection001.mhd}
              1.0 0.0 0.0 0.0 -1.0 0.0 0.0 0.0 1.0 ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeWithDirection001.nhdr)
itk_add_test(NAME itkImageIODirection3DTest08
      COMMAND ITKIOImageBaseTestDriver itkImageIODirection3DTest
              DATA{${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection002.mhd}
              1.0 0.0 0.0 0.0 0.0 01.0 0.0 -1.0 0.0 ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeWithDirection002.nhdr)
itk_add_test(NAME itkImageIODirection3DTest09
      COMMAND ITKIOImageBaseTestDriver itkImageIODirection3DTest
              DATA{${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection003.mhd}
              0.0 -1.0 0.0 0.0 0.0 1.0 1.0 0.0 0.0 ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeWithDirection003.nhdr)
itk_add_test(NAME itkImageIOFileNameExtensionsTests
      COMMAND ITKIOImageBaseTestDriver itkImageIOFileNameExtensionsTests)

itk_add_test(NAME itkImageSeriesReaderDimensionsTest1
      COMMAND ITKIOImageBaseTestDriver itkImageSeriesReaderDimensionsTest
              DATA{${ITK_DATA_ROOT}/Input/DicomSeries/Image0075.dcm}
              DATA{${ITK_DATA_ROOT}/Input/DicomSeries/Image0076.dcm}
              DATA{${ITK_DATA_ROOT}/Input/DicomSeries/Image0077.dcm})

set_property(TEST itkImageSeriesReaderDimensionsTest1 APPEND PROPERTY DEPENDS ITK_Data)

itk_add_test(NAME itkImageFileReaderPositiveSpacingTest
      COMMAND ITKIOImageBaseTestDriver itkImageFileReaderPositiveSpacingTest
              DATA{${ITK_DATA_ROOT}/Input/itkImageNegativeSpacing.mha})

itk_add_test(NAME itkImageSeriesReaderDimensionsTest2
      COMMAND ITKIOImageBaseTestDriver itkImageSeriesReaderDimensionsTest
              DATA{${ITK_DATA_ROOT}/Input/cthead1.tif}
              DATA{${ITK_DATA_ROOT}/Input/cthead1.tif} DATA{${ITK_DATA_ROOT}/Input/cthead1.tif})

itk_add_test(NAME itkImageSeriesReaderVectorImageTest1
  COMMAND ITKIOImageBaseTestDriver itkImageSeriesReaderVectorTest
  DATA{${ITK_DATA_ROOT}/Input/RGBTestImage.tif}
  DATA{${ITK_DATA_ROOT}/Input/RGBTestImage.tif}
  DATA{${ITK_DATA_ROOT}/Input/RGBTestImage.tif} )

itk_add_test(NAME itkImageSeriesReaderVectorImageTest2
   COMMAND ITKIOImageBaseTestDriver itkImageSeriesReaderVectorTest
   DATA{${ITK_DATA_ROOT}/Input/48BitTestImage.tif}
   DATA{${ITK_DATA_ROOT}/Input/48BitTestImage.tif} DATA{${ITK_DATA_ROOT}/Input/48BitTestImage.tif} )
itk_add_test(NAME itkImageSeriesWriterTest
      COMMAND ITKIOImageBaseTestDriver itkImageSeriesWriterTest
              DATA{${ITK_DATA_ROOT}/Input/DicomSeries/,REGEX:Image[0-9]+.dcm}
              ${ITK_TEST_OUTPUT_DIR} png)

if(ITK_BUILD_SHARED_LIBS)
  ## Create a library to test ITK IO plugins
  set(FileFreeTest_Source itkFileFreeImageIO.cxx itkFileFreeImageIOFactory.cxx)
  add_library(FileFreeTestLib MODULE ${FileFreeTest_Source})
  itk_module_target_label(FileFreeTestLib)
  target_link_libraries(FileFreeTestLib LINK_PUBLIC ${ITKIOImageBase_LIBRARIES})
  set_property(TARGET FileFreeTestLib PROPERTY LIBRARY_OUTPUT_DIRECTORY ${ITK_TEST_OUTPUT_DIR})
  add_dependencies(ITKIOImageBaseTestDriver FileFreeTestLib)

  itk_add_test(NAME itkIOPluginTest
           COMMAND itkTestDriver  --add-before-env ITK_AUTOLOAD_PATH "$<TARGET_FILE_DIR:FileFreeTestLib>"
              "$<TARGET_FILE:ITKIOImageBaseTestDriver>" itkIOPluginTest
             ${ITK_TEST_OUTPUT_DIR}
             "FileFreeIO::Size=128,256:Spacing=.5,.8:Origin=5,6:Direction=-1,0,0,-1"
             ${ITK_TEST_OUTPUT_DIR}/itkIOPluginTest.png
    )
endif()
itk_add_test(NAME itkNoiseImageFilterTest
      COMMAND ITKIOImageBaseTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/itkNoiseImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/itkNoiseImageFilterTest.png
    itkNoiseImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkNoiseImageFilterTest.png)
itk_add_test(NAME itkMatrixImageWriteReadTest
      COMMAND ITKIOImageBaseTestDriver itkMatrixImageWriteReadTest
              ${ITK_TEST_OUTPUT_DIR}/testMatrix1.mha)
itk_add_test(NAME itkReadWriteImageWithDictionaryTest
      COMMAND ITKIOImageBaseTestDriver itkReadWriteImageWithDictionaryTest
              ${ITK_TEST_OUTPUT_DIR}/test.hdr)
itk_add_test(NAME itkReadWriteImageWithDictionaryTest1
      COMMAND ITKIOImageBaseTestDriver itkReadWriteImageWithDictionaryTest
              ${ITK_TEST_OUTPUT_DIR}/test.mha)
itk_add_test(NAME itkVectorImageReadWriteTest
      COMMAND ITKIOImageBaseTestDriver itkVectorImageReadWriteTest
              ${ITK_TEST_OUTPUT_DIR}/VectorImageReadWriteTest.mhd)
itk_add_test(NAME itkVectorImageReadWriteTest2
      COMMAND ITKIOImageBaseTestDriver itkVectorImageReadWriteTest
              ${ITK_TEST_OUTPUT_DIR}/VectorImageReadWriteTest.nrrd)

add_executable(itkUnicodeIOTest itkUnicodeIOTest.cxx)
itk_module_target_label(itkUnicodeIOTest)
itk_add_test(NAME itkUnicodeIOTest COMMAND itkUnicodeIOTest)
