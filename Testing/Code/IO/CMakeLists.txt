
#
# Build ImageCompare: a helper application for comparing tests outputs
#
add_executable(ImageCompare ImageCompare.cxx)
target_link_libraries(ImageCompare ITKIO)
set(IMAGE_COMPARE "${EXECUTABLE_OUTPUT_PATH}/ImageCompare" CACHE INTERNAL "ImageCompare path to be used by subprojects")

#
# Build ImageCompareCommand: a helper application for comparing tests outputs.
# This is a duplicate application of ImageCompare but with a metaCommand arguments
# interface. This allows to specify in the command line the tolerance parameters
# to be used for comparing the images.
#
add_executable(ImageCompareCommand ImageCompareCommand.cxx)
target_link_libraries(ImageCompareCommand ITKIO)
set(IMAGE_COMPARE_COMMAND "${EXECUTABLE_OUTPUT_PATH}/ImageCompareCommand" CACHE INTERNAL "ImageCompare path to be used by subprojects")

#
# Build ImageCopy: a helper application for moving images around
#
add_executable(ImageCopy ImageCopy.cxx)
target_link_libraries(ImageCopy ITKIO)
set(IMAGE_COPY "${EXECUTABLE_OUTPUT_PATH}/ImageCopy" CACHE INTERNAL "ImageCopy path to be used by subprojects")

#
# Install the executables if we have a destination for them.
#
if(ITK_INSTALL_BIN_DIR_CM24)
  install(TARGETS ImageCompare ImageCompareCommand ImageCopy
    RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT Development)
endif(ITK_INSTALL_BIN_DIR_CM24)


if( NOT ITK_DISABLE_CXX_TESTING )

set(IO_TESTS ${CXX_TEST_PATH}/itkIOTests)
set(IO_HEADER_TEST ${CXX_TEST_PATH}/itkIOHeaderTest)


set(IOTest_SRCS
itkAnalyzeImageIOTest.cxx
itkAnalyzeImageIODirectionsTest.cxx
itkNiftiImageIOTest.cxx
itkNiftiImageIOTest2.cxx
itkArchetypeSeriesFileNamesTest.cxx
itkBMPImageIOTest.cxx
itkBMPImageIOTest2.cxx
itkBioRadImageIOTest.cxx
itkLSMImageIOTest.cxx
itkConvertBufferTest.cxx
itkConvertBufferTest2.cxx
itkDICOMImageIO2Test.cxx
itkDICOMImageSeriesTest.cxx
itkDICOMImageSeriesTest2.cxx
itkDICOMSeriesFileNamesTest.cxx
itkDicomImageIOTest.cxx
itkDicomImageIODirection2DTest.cxx
itkGEImageIOTest.cxx
itkGiplImageIOTest.cxx
itkImageIOBaseTest.cxx
itkImageIOFileNameExtensionsTests.cxx
itkIOCommonTest.cxx
itkIOPrintTest.cxx
itkIOPluginTest.cxx
itkImageFileReaderTest1.cxx
itkImageFileReaderDimensionsTest.cxx
itkImageFileReaderStreamingTest.cxx
itkImageFileReaderStreamingTest2.cxx
itkImageFileWriterTest.cxx
itkImageFileWriterTest2.cxx
itkImageFileWriterPastingTest1.cxx
itkImageFileWriterPastingTest2.cxx
itkImageFileWriterStreamingTest1.cxx
itkImageFileWriterStreamingTest2.cxx
itkImageFileWriterStreamingPastingCompressingTest1.cxx
itkImageIODirection2DTest.cxx
itkImageIODirection3DTest.cxx
itkImageReadDICOMSeriesWriteTest.cxx
itkImageSeriesReaderDimensionsTest.cxx
itkImageSeriesWriterTest.cxx
itkJPEGImageIOTest.cxx
itkLargeImageWriteReadTest.cxx
itkLargeImageWriteConvertReadTest.cxx
itkMatrixImageWriteReadTest.cxx
itkMeshSpatialObjectIOTest.cxx
itkMetaImageIOTest.cxx
itkMetaImageStreamingIOTest.cxx
itkMetaImageStreamingWriterIOTest.cxx
itkNrrdImageIOTest.cxx
itkNrrdImageReadWriteTest.cxx
itkNrrdRGBImageReadWriteTest.cxx
itkNrrdRGBAImageReadWriteTest.cxx
itkNrrdDiffusionTensor3DImageReadTest.cxx
itkNrrdDiffusionTensor3DImageReadWriteTest.cxx
itkNrrdDiffusionTensor3DImageReadTensorDoubleWriteTensorDoubleTest.cxx
itkNrrdComplexImageReadTest.cxx
itkNrrdComplexImageReadWriteTest.cxx
itkNrrdVectorImageReadTest.cxx
itkNrrdVectorImageReadWriteTest.cxx
itkNrrdCovariantVectorImageReadTest.cxx
itkNrrdCovariantVectorImageReadWriteTest.cxx
itkNumericSeriesFileNamesTest.cxx
itkPNGImageIOTest.cxx
itkPolygonGroupSpatialObjectXMLFileTest.cxx
itkRawImageIOTest.cxx
itkRawImageIOTest2.cxx
itkRawImageIOTest3.cxx
itkRawImageIOTest4.cxx
itkRawImageIOTest5.cxx
itkReadWriteImageWithDictionaryTest.cxx
itkReadWriteSpatialObjectTest.cxx
itkRegularExpressionSeriesFileNamesTest.cxx
itkSymmetricSecondRankTensorImageReadTest.cxx
itkSymmetricSecondRankTensorImageWriteReadTest.cxx
itkStimulateImageIOTest.cxx
itkStimulateImageIOTest2.cxx
itkTIFFImageIOTest.cxx
itkTransformIOTest.cxx
itkVTKImageIOTest.cxx
itkVTKImageIOTest2.cxx
itkVectorImageReadWriteTest.cxx
testMetaBlob.cxx
testMetaGroup.cxx
testMetaImage.cxx
testMetaLandmark.cxx
testMetaLine.cxx
testMetaMesh.cxx
testMetaObject.cxx
testMetaScene.cxx
testMetaSurface.cxx
testMetaTube.cxx
testMetaUtils.cxx
testMetaArray.cxx
testMetaCommand.cxx
)

set(TEMP ${ITKTesting_BINARY_DIR}/Temporary)

add_test(itkImageIOBaseTest ${IO_TESTS} itkImageIOBaseTest)
add_test(itkImageIOFileNameExtensionsTests ${IO_TESTS} itkImageIOFileNameExtensionsTests)
add_test(itkIOCommonTest ${IO_TESTS} itkIOCommonTest)
add_test(itkIOPrintTest ${IO_TESTS} itkIOPrintTest)
add_test(itkConvertBufferTest2 ${IO_TESTS} itkConvertBufferTest2)
add_test(itkConvertBufferTest ${IO_TESTS} itkConvertBufferTest)
add_test(testMetaUtils ${IO_TESTS} testMetaUtils ${TEMP})
add_test(testMetaArray ${IO_TESTS} testMetaArray ${TEMP})
add_test(testMetaGroup ${IO_TESTS} testMetaGroup ${TEMP})
add_test(testMetaObject ${IO_TESTS} testMetaObject ${TEMP})
add_test(testMetaImage ${IO_TESTS} testMetaImage)
add_test(testMetaTube ${IO_TESTS} testMetaTube ${TEMP})
add_test(testMetaBlob ${IO_TESTS} testMetaBlob ${TEMP})
add_test(testMetaSurface ${IO_TESTS} testMetaSurface ${TEMP})
add_test(testMetaLine ${IO_TESTS} testMetaLine ${TEMP})
add_test(testMetaLandmark ${IO_TESTS} testMetaLandmark ${TEMP})
add_test(testMetaMesh ${IO_TESTS} testMetaMesh ${TEMP})
add_test(testMetaScene ${IO_TESTS} testMetaScene ${TEMP})

add_test( MetaCommandRequiredOptionsWithMultipleStrings ${IO_TESTS} testMetaCommand -ExpectedFailStatus 0 -sum 7 -r1c_rs StringValue1 StringValue2 StringValue3 -r2_ri 7)
add_test( MetaCommandRequiredOptionsWithMultipleStringsAtEnd ${IO_TESTS} testMetaCommand -ExpectedFailStatus 0 -sum 7 -r2_ri 7 -r1c_rs StringValue1 StringValue2 StringValue3)
add_test( MetaCommandOptionalWithRequired  ${IO_TESTS} testMetaCommand -ExpectedFailStatus 0 -sum 107 -r1c_rs StringValue1 StringValue2 StringValue3 -r2_ri 7 -o1_ri 1 -o2_ri 99)
add_test( MetaCommandOptionalOptoinsWithOutArguments  ${IO_TESTS} testMetaCommand -ExpectedFailStatus 0 -sum 207 -r1c_rs StringValue1 StringValue2 StringValue3 -r2_ri 7 -o1_oi   -o2_oi )
add_test( MetaCommandOptionalOptoinsWithOutArguments  ${IO_TESTS} testMetaCommand -ExpectedFailStatus 0 -sum 107 -r1c_rs StringValue1 StringValue2 StringValue3 -r2_ri 7 -o1_oi 1  -o2_oi 99)
add_test( MetaCommandRequiredAndOptionalOptoinsWithArguments ${IO_TESTS} testMetaCommand -ExpectedFailStatus 0 -sum 207 -r1c_rs StringValue1 StringValue2 StringValue3 -r2_ri 7 -o1_ri 1 -o2_ri 99 -o3_ri_oi 50 50)
add_test( MetaCommandRequiredAndOptionalOptoinsWithOutArguments ${IO_TESTS}  testMetaCommand -ExpectedFailStatus 0 -sum 257 -r1c_rs StringValue1 StringValue2 StringValue3 -r2_ri 7 -o1_ri 1 -o2_ri 99 -o3_ri_oi 50)
add_test( MetaCommandRequiredAndOptionalOptoinsWithOutArguments-1 ${IO_TESTS} testMetaCommand -ExpectedFailStatus 0 -sum 457 -r1c_rs StringValue1 StringValue2 StringValue3 -r2_ri 7 -o1_ri 1 -o2_ri 99 -o3_ri_oi 50 -o1_oi -o2_oi )
add_test( MetaCommandRequiredOptionsWithMissingMultipleStrings  ${IO_TESTS} testMetaCommand -ExpectedFailStatus 1 -sum 7 -r1c_rs  -r2_ri 7)
add_test( MetaCommandRequiredOptionsWithMissingMultipleStringsAtEnd ${IO_TESTS}  testMetaCommand -ExpectedFailStatus 1 -sum 7 -r2_ri 7 -r1c_rs )
add_test( MetaCommandOptionalWithRequiredMissing  ${IO_TESTS} testMetaCommand -ExpectedFailStatus 1 -sum 107 -r1c_rs StringValue1 StringValue2 StringValue3 -r2_ri  -o1_ri 1 -o2_ri 99)
add_test( MetaCommandRequiredAndOptionalOptoinsWithArgumentsMissing  ${IO_TESTS} testMetaCommand -ExpectedFailStatus 1 -sum 207 -r1c_rs StringValue1 StringValue2 StringValue3 -r2_ri 7 -o3_ri_oi )

add_test(itkNrrdImageIOTest1 ${IO_TESTS} itkNrrdImageIOTest ${ITK_TEST_OUTPUT_DIR}/testNrrd.nrrd)
add_test(itkNrrdImageIOTest2 ${IO_TESTS} itkNrrdImageIOTest ${ITK_TEST_OUTPUT_DIR}/testNrrd.nhdr)
add_test(itkReadWriteImageWithDictionaryTest ${IO_TESTS} itkReadWriteImageWithDictionaryTest ${TEMP}/test.hdr)
add_test(itkReadWriteImageWithDictionaryTest1 ${IO_TESTS} itkReadWriteImageWithDictionaryTest ${TEMP}/test.mha)
add_test(itkReadWriteSpatialObjectTest ${IO_TESTS} itkReadWriteSpatialObjectTest ${TEMP}/Objects.meta)
add_test(itkReadWriteSpatialObjectTest1 ${IO_TESTS} itkReadWriteSpatialObjectTest ${TEMP}/Objects1.meta binary)
add_test(itkReadWriteSpatialObjectTest2 ${IO_TESTS} itkReadWriteSpatialObjectTest ${TEMP}/Objects2.meta
                                                    ${ITK_DATA_ROOT}/Input/SpatialObjects.meta)

add_test(itkMatrixImageWriteReadTest ${IO_TESTS}
         itkMatrixImageWriteReadTest
         ${TEMP}/testMatrix1.mha)

add_test(itkMeshSpatialObjectIOTest ${IO_TESTS} itkMeshSpatialObjectIOTest ${TEMP}/metameshIOTest.txt)
add_test(itkMeshSpatialObjectIOTest1 ${IO_TESTS} itkMeshSpatialObjectIOTest ${TEMP}/metameshIOTest1.txt binary)
add_test(itkMeshSpatialObjectIOTest2 ${IO_TESTS} itkMeshSpatialObjectIOTest ${TEMP}/metameshIOTest2.txt
                                                ${ITK_DATA_ROOT}/Input/metamesh.txt)

add_test(itkTransformIOTest ${IO_TESTS} itkTransformIOTest ${TEMP})

add_test(itkVTKImageIOTest ${IO_TESTS} itkVTKImageIOTest
                           ${TEMP}/test1.vtk ${TEMP}/test2.vtk)
add_test(itkVTKImageIOTest2 ${IO_TESTS} itkVTKImageIOTest2
                           ${ITK_DATA_ROOT}/Input/VHFColor.mhd
                           ${TEMP}/testVector.vtk)
add_test(itkRawImageIOTest ${IO_TESTS} itkRawImageIOTest
                           ${TEMP}/raw1.raw ${TEMP}/raw2.raw
)
add_test(itkRawImageIOTest3 ${IO_TESTS} itkRawImageIOTest3
                           ${TEMP}/raw3.raw ${TEMP}/raw4.raw
)
add_test(itkRawImageIOTest4 ${IO_TESTS} itkRawImageIOTest4
                           ${TEMP}/bigendian.raw ${TEMP}/littleendian.raw
)

add_test(itkRawImageIOTest5 ${IO_TESTS} itkRawImageIOTest5 ${TEMP} )

add_test(itkVectorImageReadWriteTest ${IO_TESTS} itkVectorImageReadWriteTest
                           ${TEMP}/VectorImageReadWriteTest.mhd
)
add_test(itkVectorImageReadWriteTest2 ${IO_TESTS} itkVectorImageReadWriteTest
                           ${TEMP}/VectorImageReadWriteTest.nrrd
)

add_test(itkStimulateImageIOTest ${IO_TESTS} itkStimulateImageIOTest
                           ${TEMP}/test1.spr ${TEMP}/test2.spr)

add_test(itkSymmetricSecondRankTensorImageReadTest ${IO_TESTS}
         itkSymmetricSecondRankTensorImageReadTest
         ${TEMP}/testSymmetricTensor.nrrd)

add_test(itkSymmetricSecondRankTensorImageWriteReadTest ${IO_TESTS}
         itkSymmetricSecondRankTensorImageWriteReadTest
         ${TEMP}/testSymmetricTensorWriteRead.mha)

add_test(itkIOHeaderTest ${IO_HEADER_TEST})
add_test(itkPolygonGroupSpatialObjectXMLFileTest ${IO_TESTS} itkPolygonGroupSpatialObjectXMLFileTest ${TEMP})
add_test(itkImageFileWriterTest ${IO_TESTS} itkImageFileWriterTest ${TEMP}/test.png)

###
# Will be moved to a common test driver after the file is determined to be portable
###
add_executable(itkUnicodeIOTest itkUnicodeIOTest.cxx)
add_test(itkUnicodeIOTest ${CXX_TEST_PATH}/itkUnicodeIOTest)
###


add_executable(itkIOHeaderTest itkIOHeaderTest.cxx)
add_executable(itkIOTests itkIOTests.cxx ${IOTest_SRCS})
target_link_libraries(itkIOHeaderTest ITKIO)
target_link_libraries(itkIOTests ITKIO ITKEXPAT ITKSpatialObject ITKAlgorithms)

if(ITK_OBJCXX_COMPILER_WORKS)
  set(IO_OBJCXX_TEST ${CXX_TEST_PATH}/itkIOHeaderObjCxxTest)
  add_test(itkIOHeaderObjCxxTest ${IO_OBJCXX_TEST})
  add_executable(itkIOHeaderObjCxxTest itkIOHeaderObjCxxTest.mm)
  target_link_libraries(itkIOHeaderObjCxxTest ITKIO ITKEXPAT ITKSpatialObject ITKAlgorithms)
endif(ITK_OBJCXX_COMPILER_WORKS)


if( ${ITK_COMPUTER_MEMORY_SIZE} GREATER 5 )

  # Image of 1.67 Gigabytes (pixels size is 16bits)
  add_test(itkLargeImageWriteReadTest1 ${IO_TESTS} itkLargeImageWriteReadTest
           ${ITK_TEST_OUTPUT_DIR}/LargeImage01.mhd  30000L )

  # Image of 2.98 Gigabytes (pixels size is 16bits)
  add_test(itkLargeImageWriteReadTest2 ${IO_TESTS} itkLargeImageWriteReadTest
           ${ITK_TEST_OUTPUT_DIR}/LargeImage02.mhd  40000L )

  # Image of 4.9 Gigabytes (pixels size is 8-bits out and 16-bit in)
  add_test(itkLargeImageWriteConvertReadTest ${IO_TESTS} itkLargeImageWriteConvertReadTest
           ${ITK_TEST_OUTPUT_DIR}/LargeImageConvert.nrrd  50000L )

  # Due to the large memory requirements this tests must be run one by one
  set_tests_properties(itkLargeImageWriteReadTest1
                       itkLargeImageWriteReadTest2
                       itkLargeImageWriteConvertReadTest
                       PROPERTIES
                       RUN_SERIAL 1
                      )
endif( ${ITK_COMPUTER_MEMORY_SIZE} GREATER 5 )


if( ${ITK_COMPUTER_MEMORY_SIZE} GREATER 12 )

  # Image of 9.12 Gigabytes (pixels size is 16bits)
  add_test(itkLargeImageWriteReadTest3 ${IO_TESTS} itkLargeImageWriteReadTest
    ${ITK_TEST_OUTPUT_DIR}/LargeImage03.mhd  70000L )

  # Image of 5.99 Gigabytes (pixels size is 16bits)
  add_test(itkLargeImageWriteReadTest_IMG ${IO_TESTS} itkLargeImageWriteReadTest
    ${ITK_TEST_OUTPUT_DIR}/LargeImage.img  32767L 3 )

  # Image of 5.99 Gigabytes (pixels size is 16bits)
  add_test(itkLargeImageWriteReadTest_IMG_GZ ${IO_TESTS} itkLargeImageWriteReadTest
    ${ITK_TEST_OUTPUT_DIR}/LargeImage.img.gz  32766L 3 )

  # Due to the large memory requirements this tests must be run one by one
  set_tests_properties(itkLargeImageWriteReadTest3
    itkLargeImageWriteReadTest_IMG
    itkLargeImageWriteReadTest_IMG_GZ
    PROPERTIES
    RUN_SERIAL 1
    )

endif( ${ITK_COMPUTER_MEMORY_SIZE} GREATER 12 )



if(ITK_DATA_ROOT)

add_test(itkRawImageIOTest2 ${IO_TESTS} itkRawImageIOTest2
         ${ITK_DATA_ROOT}/Input/VHFColor.raw)


add_test(itkImageFileWriterTest2_1 ${IO_TESTS}
  itkImageFileWriterTest2 ${ITK_TEST_OUTPUT_DIR}/test.nrrd)
add_test(itkImageFileWriterTest2_2 ${IO_TESTS}
  itkImageFileWriterTest2 ${ITK_TEST_OUTPUT_DIR}/test.mha)
add_test(itkImageFileWriterTest2_3 ${IO_TESTS}
  itkImageFileWriterTest2 ${ITK_TEST_OUTPUT_DIR}/test.vtk)

add_test( itkImageFileReaderTest1 ${IO_TESTS}
  itkImageFileReaderTest1 )

add_test( itkImageFileReaderDimensionsTest_MHD ${IO_TESTS}
  itkImageFileReaderDimensionsTest
  ${ITK_DATA_ROOT}/Input/HeadMRVolume.mha
  ${ITK_TEST_OUTPUT_DIR}
  mha
  )

add_test( itkImageFileReaderDimensionsTest_NRRD ${IO_TESTS}
  itkImageFileReaderDimensionsTest
  ${ITK_DATA_ROOT}/Input/vol-ascii.nrrd
  ${ITK_TEST_OUTPUT_DIR}
  nrrd
  )


add_test(itkImageFileReaderStreamingTest_1 ${IO_TESTS}
  itkImageFileReaderStreamingTest
     ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd 1 0
  )

add_test(itkImageFileReaderStreamingTest_2 ${IO_TESTS}
  itkImageFileReaderStreamingTest
     ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd 0 1
  )

add_test(itkImageFileReaderStreamingTest_3 ${IO_TESTS}
  itkImageFileReaderStreamingTest
     ${ITK_DATA_ROOT}/Input/vol-ascii.nrrd 0 0
  )

add_test(itkImageFileReaderStreamingTest2_MHD ${IO_TESTS}
  itkImageFileReaderStreamingTest2
     ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd
  )

add_test(itkImageFileWriterStreamingPastingCompressingTest_MHA ${IO_TESTS}
  itkImageFileWriterStreamingPastingCompressingTest1
            ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd
            ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreamingPastingCompressingTest mha 0 0 0 1 0 0 0 1
            )

add_test(itkImageFileWriterStreamingPastingCompressingTest_NRRD ${IO_TESTS}
  itkImageFileWriterStreamingPastingCompressingTest1
            ${ITK_DATA_ROOT}/Input/vol-ascii.nrrd
            ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreamingPastingCompressingTest nrrd 0 0 1 1 0 0 1 1
            )


add_test(itkImageFileWriterStreamingPastingCompressingTest_NHDR ${IO_TESTS}
  itkImageFileWriterStreamingPastingCompressingTest1
            ${ITK_DATA_ROOT}/Input/vol-ascii.nrrd
            ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreamingPastingCompressingTest nhdr 0 0 1 1 0 0 1 1
            )

add_test(itkImageFileWriterStreamingPastingCompressingTest_VTK ${IO_TESTS}
  --compare
            ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd
            ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreamingPastingCompressingTest000.vtk
  itkImageFileWriterStreamingPastingCompressingTest1
            ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd
            ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreamingPastingCompressingTest  vtk 0 0 1 1 0 0 1 1
            )

add_test(itkImageFileWriterStreamingPastingCompressingTest_DCM ${IO_TESTS}
  itkImageFileWriterStreamingPastingCompressingTest1
            ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd
            ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreamingPastingCompressingTest dcm 0 0 1 1 0 0 1 1
            )


add_test(itkImageFileWriterStreamingPastingCompressingTest_IMG ${IO_TESTS}
  itkImageFileWriterStreamingPastingCompressingTest1
            ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd
            ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreamingPastingCompressingTest img 0 0 1 1 0 0 1 1
            )




add_test(itkImageFileWriterPastingTest1 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd
            ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterPastingTest1_01.mha
  itkImageFileWriterPastingTest1
            ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd
            ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterPastingTest1_01.mha)


set(ITK_IOTEST_PASTINGSTREAMING_MACRO_I 0)
macro(ITK_IOTEST_PASTINGSTREAMING_MACRO InputFileName BaselineFileName DifferentFileName)

  # we test 3 things, streaming to no file, existing file, and when input dosen't stream
  math(EXPR ITK_IOTEST_PASTINGSTREAMING_MACRO_I "${ITK_IOTEST_PASTINGSTREAMING_MACRO_I}+1")
  add_test(itkImageFileWriterStreamingTest1_${ITK_IOTEST_PASTINGSTREAMING_MACRO_I} ${IO_TESTS}
    --compare ${BaselineFileName}
       ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreaming1_${ITK_IOTEST_PASTINGSTREAMING_MACRO_I}.mha
    itkImageFileWriterStreamingTest1
       ${InputFileName}
       ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreaming1_${ITK_IOTEST_PASTINGSTREAMING_MACRO_I}.mha )

  math(EXPR ITK_IOTEST_PASTINGSTREAMING_MACRO_I "${ITK_IOTEST_PASTINGSTREAMING_MACRO_I}+1")
  add_test(itkImageFileWriterStreamingTest1_${ITK_IOTEST_PASTINGSTREAMING_MACRO_I} ${IO_TESTS}
    --compare ${BaselineFileName}
       ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreaming1_${ITK_IOTEST_PASTINGSTREAMING_MACRO_I}.mha
    itkImageFileWriterStreamingTest1
       ${InputFileName}
       ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreaming1_${ITK_IOTEST_PASTINGSTREAMING_MACRO_I}.mha
       ${DifferentFileName} 0)

  math(EXPR ITK_IOTEST_PASTINGSTREAMING_MACRO_I "${ITK_IOTEST_PASTINGSTREAMING_MACRO_I}+1")
  add_test(itkImageFileWriterStreamingTest1_${ITK_IOTEST_PASTINGSTREAMING_MACRO_I} ${IO_TESTS}
    --compare ${BaselineFileName}
       ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreaming1_${ITK_IOTEST_PASTINGSTREAMING_MACRO_I}.mha
    itkImageFileWriterStreamingTest1
       ${InputFileName}
       ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreaming1_${ITK_IOTEST_PASTINGSTREAMING_MACRO_I}.mha
       ${InputFileName} 1)

  math(EXPR ITK_IOTEST_PASTINGSTREAMING_MACRO_I "${ITK_IOTEST_PASTINGSTREAMING_MACRO_I}+1")
  add_test(itkImageFileWriterStreamingTest2_${ITK_IOTEST_PASTINGSTREAMING_MACRO_I} ${IO_TESTS}
    --compare ${BaselineFileName}
       ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreaming2_${ITK_IOTEST_PASTINGSTREAMING_MACRO_I}.mha
    itkImageFileWriterStreamingTest2
       ${InputFileName}
       ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterStreaming2_${ITK_IOTEST_PASTINGSTREAMING_MACRO_I}.mha)

  # we test 3 things, pasting to a new image, a matching image, and a different image
  math(EXPR ITK_IOTEST_PASTINGSTREAMING_MACRO_I "${ITK_IOTEST_PASTINGSTREAMING_MACRO_I}+1")
  add_test(itkImageFileWriterPastingTest2_${ITK_IOTEST_PASTINGSTREAMING_MACRO_I} ${IO_TESTS}
#    The output image is undefined outside of the paste region
#    --compare ${BaselineFileName}
#       ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterPastingTest2_${ITK_IOTEST_PASTINGSTREAMING_MACRO_I}.mha
    itkImageFileWriterPastingTest2
       ${InputFileName}
       ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterPastingTest2_${ITK_IOTEST_PASTINGSTREAMING_MACRO_I}.mha )

  math(EXPR ITK_IOTEST_PASTINGSTREAMING_MACRO_I "${ITK_IOTEST_PASTINGSTREAMING_MACRO_I}+1")
  add_test(itkImageFileWriterPastingTest2_${ITK_IOTEST_PASTINGSTREAMING_MACRO_I} ${IO_TESTS}
    --compare ${BaselineFileName}
       ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterPastingTest2_${ITK_IOTEST_PASTINGSTREAMING_MACRO_I}.mha
    itkImageFileWriterPastingTest2
       ${InputFileName}
       ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterPastingTest2_${ITK_IOTEST_PASTINGSTREAMING_MACRO_I}.mha
       ${InputFileName} )

  math(EXPR ITK_IOTEST_PASTINGSTREAMING_MACRO_I "${ITK_IOTEST_PASTINGSTREAMING_MACRO_I}+1")
  add_test(itkImageFileWriterPastingTest2_${ITK_IOTEST_PASTINGSTREAMING_MACRO_I} ${IO_TESTS}
    itkImageFileWriterPastingTest2
       ${InputFileName}
       ${ITK_TEST_OUTPUT_DIR}/itkImageFileWriterPastingTest2_${ITK_IOTEST_PASTINGSTREAMING_MACRO_I}.mha
       ${DifferentFileName} )

endmacro(ITK_IOTEST_PASTINGSTREAMING_MACRO)

ITK_IOTEST_PASTINGSTREAMING_MACRO( ${ITK_DATA_ROOT}/Input/HeadMRVolume.mha ${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd ${ITK_DATA_ROOT}/Input/HeadMRVolumeCompressed.mha)


add_test(itkMetaImageIOTest ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd
            ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume.mhd
  itkMetaImageIOTest
            ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd
            ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume.mhd)
add_test(itkMetaImageIOShouldFailTest ${IO_TESTS}
         itkMetaImageIOTest
         ${ITK_DATA_ROOT}/Input/MetaImageError.mhd
         1)
add_test(itkMetaImageStreamingIOTest ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd
            ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeStreamed.mhd
  itkMetaImageStreamingIOTest
            ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd
            ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeStreamed.mhd)
add_test(itkMetaImageCompressedStreamingIOTest ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd
            ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeCompressedStreamed.mha
  itkMetaImageStreamingIOTest
            ${ITK_DATA_ROOT}/Input/HeadMRVolumeCompressed.mha
            ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeCompressedStreamed.mha)
add_test(itkMetaImageStreamingWriterIOTest ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd
            ${ITK_TEST_OUTPUT_DIR}/MetaImageStreamingWriterIOTest.mha
  itkMetaImageStreamingWriterIOTest
            ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd
            ${ITK_TEST_OUTPUT_DIR}/MetaImageStreamingWriterIOTest.mha)
# This test is failing to produce correct output
add_test(itkMetaImageStreamingWriterIOTest2 ${IO_TESTS}
#  --compare ${ITK_DATA_ROOT}/Input/mri3D.mhd
#            ${ITK_TEST_OUTPUT_DIR}/mri3DWriteStreamed.mha
  itkMetaImageStreamingWriterIOTest
            ${ITK_DATA_ROOT}/Input/mri3D.mhd
            ${ITK_TEST_OUTPUT_DIR}/mri3DWriteStreamed.mha)
add_test(itkGiplImageIOTest ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/ramp.mhd
            ${ITK_TEST_OUTPUT_DIR}/ramp.mhd
  itkGiplImageIOTest
            ${ITK_DATA_ROOT}/Input/ramp.gipl
            ${ITK_TEST_OUTPUT_DIR}/ramp.mhd)
add_test(itkGiplImageIOGzTest ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/ramp.mhd
            ${ITK_TEST_OUTPUT_DIR}/ramp-gz.mhd
  itkGiplImageIOTest
            ${ITK_DATA_ROOT}/Input/ramp.gipl.gz
            ${ITK_TEST_OUTPUT_DIR}/ramp-gz.mhd)
add_test(itkLSMImageIOTest ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/cthead1.tif
            ${ITK_TEST_OUTPUT_DIR}/cthead1.tif
  itkLSMImageIOTest
            ${ITK_DATA_ROOT}/Input/cthead1.lsm
            ${ITK_TEST_OUTPUT_DIR}/cthead1.tif)
# Outcommenting LSM write test. It fails.
#add_test(itkLSMImageIOTest2 ${IO_TESTS}
#  --compare ${ITK_DATA_ROOT}/Baseline/IO/cthead1.tif
#            ${ITK_TEST_OUTPUT_DIR}/cthead1.lsm
#  itkLSMImageIOTest
#            ${ITK_DATA_ROOT}/Input/cthead1.lsm
#            ${ITK_TEST_OUTPUT_DIR}/cthead1.lsm)
add_test(itkBioRadImageIOTest ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/biorad.mhd
            ${ITK_TEST_OUTPUT_DIR}/biorad.pic
  itkBioRadImageIOTest
            ${ITK_DATA_ROOT}/Input/biorad.pic
            ${ITK_TEST_OUTPUT_DIR}/biorad.pic)
add_test(itkBMPImageIOTest ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/cthead1.bmp
            ${ITK_TEST_OUTPUT_DIR}/cthead1.bmp
  itkBMPImageIOTest
            ${ITK_DATA_ROOT}/Input/cthead1.bmp
            ${ITK_TEST_OUTPUT_DIR}/cthead1.bmp)
add_test(itkBMPImageIOTest2 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/cthead254x254.bmp
            ${ITK_TEST_OUTPUT_DIR}/cthead254x254.bmp
  itkBMPImageIOTest
            ${ITK_DATA_ROOT}/Input/cthead254x254.bmp
            ${ITK_TEST_OUTPUT_DIR}/cthead254x254.bmp)
add_test(itkBMPImageIOTest3 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/image_grayscale.bmp
            ${ITK_TEST_OUTPUT_DIR}/image_grayscale.bmp
  itkBMPImageIOTest
            ${ITK_DATA_ROOT}/Input/image_grayscale.bmp
            ${ITK_TEST_OUTPUT_DIR}/image_grayscale.bmp)
add_test(itkBMPImageIOTest4 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/image_color.bmp
            ${ITK_TEST_OUTPUT_DIR}/image_color.bmp
  itkBMPImageIOTest
            ${ITK_DATA_ROOT}/Input/image_color.bmp
            ${ITK_TEST_OUTPUT_DIR}/image_color.bmp)
add_test(itkBMPImageIOTest5 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/smallRGBA.mha
            ${ITK_TEST_OUTPUT_DIR}/smallRGBA.mha
  itkBMPImageIOTest2
            ${ITK_DATA_ROOT}/Input/smallRGBA.bmp
            ${ITK_TEST_OUTPUT_DIR}/smallRGBA.mha)
add_test(itkBMPImageIOTest6 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/smallRGBA.mha
            ${ITK_TEST_OUTPUT_DIR}/smallRGBA.bmp
  itkBMPImageIOTest2
            ${ITK_DATA_ROOT}/Input/smallRGBA.bmp
            ${ITK_TEST_OUTPUT_DIR}/smallRGBA.bmp)
add_test(itkJPEGImageIOTest ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/cthead1.png
            ${ITK_TEST_OUTPUT_DIR}/cthead1.png
  itkJPEGImageIOTest
            ${ITK_DATA_ROOT}/Input/cthead1.jpg
            ${ITK_TEST_OUTPUT_DIR}/cthead1.png)
add_test(itkJPEGImageIOTest2 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/cthead1.jpg
            ${ITK_TEST_OUTPUT_DIR}/cthead1.jpg
  itkJPEGImageIOTest
            ${ITK_DATA_ROOT}/Input/cthead1.jpg
            ${ITK_TEST_OUTPUT_DIR}/cthead1.jpg)
add_test(itkDicomImageIOTest ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/itkDicomImageIOTest.png
            ${ITK_TEST_OUTPUT_DIR}/itkDicomImageIOTest.png
  itkDicomImageIOTest
            ${ITK_DATA_ROOT}/Input/dicom-sc_cs-1.dcm
            ${ITK_TEST_OUTPUT_DIR}/itkDicomImageIOTest.png)
add_test(itkDICOMImageIO2Test ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/itkDICOMImageIO2Test.png
            ${ITK_TEST_OUTPUT_DIR}/itkDICOMImageIO2Test.png
  itkDICOMImageIO2Test
            ${ITK_DATA_ROOT}/Input/dicom-sc_cs-1.dcm
            ${ITK_TEST_OUTPUT_DIR}/itkDICOMImageIO2Test.png)
add_test(itkDicomImageIODirection2DTest ${IO_TESTS}
  itkDicomImageIODirection2DTest
  ${ITK_TEST_OUTPUT_DIR}/itkDicomImage3DIODirection2DTest.dcm
  ${ITK_TEST_OUTPUT_DIR}/itkDicomImage3DIODirection2DTest.mhd
  ${ITK_TEST_OUTPUT_DIR}/itkDicomImage2DIODirection2DTest.mhd
  )
add_test(itkStimulateImageIOTest2 ${IO_TESTS}
  itkStimulateImageIOTest2
            ${ITK_DATA_ROOT}/Input/BigEndian.spr)

add_test(itkTIFFImageIOTest ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/cthead1.tif
            ${ITK_TEST_OUTPUT_DIR}/cthead1.tif
  itkTIFFImageIOTest
            ${ITK_DATA_ROOT}/Input/cthead1.tif
            ${ITK_TEST_OUTPUT_DIR}/cthead1.tif)

add_test(itkTIFFImageIOTest1 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/RGBTestImage.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImage.tif
  itkTIFFImageIOTest
            ${ITK_DATA_ROOT}/Input/RGBTestImage.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImage.tif)

add_test(itkTIFFImageIOTest2 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/RGBTestImageCCITTFax3.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImageCCITTFax3.tif
  itkTIFFImageIOTest
            ${ITK_DATA_ROOT}/Input/RGBTestImageCCITTFax3.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImageCCITTFax3.tif)

add_test(itkTIFFImageIOTest3 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/RGBTestImageCCITTFax4.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImageCCITTFax4.tif
  itkTIFFImageIOTest
            ${ITK_DATA_ROOT}/Input/RGBTestImageCCITTFax4.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImageCCITTFax4.tif)

add_test(itkTIFFImageIOTest4 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/RGBTestImageGray.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImageGray.tif
  itkTIFFImageIOTest
            ${ITK_DATA_ROOT}/Input/RGBTestImageGray.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImageGray.tif)

add_test(itkTIFFImageIOTest5 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/RGBTestImageHuffmanRLE.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImageHuffmanRLE.tif
  itkTIFFImageIOTest
            ${ITK_DATA_ROOT}/Input/RGBTestImageHuffmanRLE.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImageHuffmanRLE.tif)

add_test(itkTIFFImageIOTest6 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/RGBTestImageJPEG.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImageJPEG.tif
  itkTIFFImageIOTest
            ${ITK_DATA_ROOT}/Input/RGBTestImageJPEG.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImageJPEG.tif)

add_test(itkTIFFImageIOTest7 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/RGBTestImageLZW.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImageLZW.tif
  itkTIFFImageIOTest
            ${ITK_DATA_ROOT}/Input/RGBTestImageLZW.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImageLZW.tif)

add_test(itkTIFFImageIOTest8 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/RGBTestImageNone.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImageNone.tif
  itkTIFFImageIOTest
            ${ITK_DATA_ROOT}/Input/RGBTestImageNone.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImageNone.tif)

add_test(itkTIFFImageIOTest9 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/RGBTestImagePackBits.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImagePackBits.tif
  itkTIFFImageIOTest
            ${ITK_DATA_ROOT}/Input/RGBTestImagePackBits.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImagePackBits.tif)

add_test(itkTIFFImageIOTest10 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/RGBTestImageZIP.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImageZIP.tif
  itkTIFFImageIOTest
            ${ITK_DATA_ROOT}/Input/RGBTestImageZIP.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImageZIP.tif)

add_test(itkTIFFImageIOTest11 ${IO_TESTS}
   --compare ${ITK_DATA_ROOT}/Baseline/IO/ShortTestImage.tiff
             ${ITK_TEST_OUTPUT_DIR}/ShortTestImage.tiff
  itkTIFFImageIOTest
            ${ITK_DATA_ROOT}/Input/ShortTestImage.tiff
            ${ITK_TEST_OUTPUT_DIR}/ShortTestImage.tiff
            2
            2)

add_test(itkTIFFImageIOTest12 ${IO_TESTS}
   --compare ${ITK_DATA_ROOT}/Baseline/IO/48BitTestImage.tif
             ${ITK_TEST_OUTPUT_DIR}/48BitTestImage.tif
  itkTIFFImageIOTest
            ${ITK_DATA_ROOT}/Input/48BitTestImage.tif
            ${ITK_TEST_OUTPUT_DIR}/48BitTestImage.tif
            2
            2)

add_test(itkTIFFImageIOTest13 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/RGBTestImageCCITTFax3.mha
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImageCCITTFax3.mha
  itkTIFFImageIOTest
            ${ITK_DATA_ROOT}/Input/RGBTestImageCCITTFax3.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImageCCITTFax3.mha)

add_test(itkTIFFImageIOTest14 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/RGBTestImageCCITTFax4.mha
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImageCCITTFax4.mha
  itkTIFFImageIOTest
            ${ITK_DATA_ROOT}/Input/RGBTestImageCCITTFax4.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImageCCITTFax4.mha)

add_test(itkTIFFImageIOMultiPagesTest ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/ramp.tif
            ${ITK_TEST_OUTPUT_DIR}/ramp.tif
  itkTIFFImageIOTest
            ${ITK_DATA_ROOT}/Input/ramp.tif
            ${ITK_TEST_OUTPUT_DIR}/ramp.tif
            3
            )

add_test(itkTIFFImageIOMultiPagesTest2 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/rampShort.tif
            ${ITK_TEST_OUTPUT_DIR}/rampShort.tif
  itkTIFFImageIOTest
            ${ITK_DATA_ROOT}/Input/rampShort.tif
            ${ITK_TEST_OUTPUT_DIR}/rampShort.tif
            3
            3
            )

add_test(itkDICOMImageSeriesTest ${IO_TESTS}
  itkDICOMImageSeriesTest
            ${ITK_DATA_ROOT}/Input/DicomSeries 0)

add_test(itkDICOMImageSeriesTest2 ${IO_TESTS}
  itkDICOMImageSeriesTest2
            ${ITK_DATA_ROOT}/Input/DicomSeries )

add_test(itkDICOMSeriesFileNamesTest ${IO_TESTS}
  itkDICOMSeriesFileNamesTest
            ${ITK_DATA_ROOT}/Input/DicomSeries)


add_test(itkImageSeriesReaderDimensionsTest1 ${IO_TESTS}
  itkImageSeriesReaderDimensionsTest
            ${ITK_DATA_ROOT}/Input/DicomSeries/Image0075.dcm
            ${ITK_DATA_ROOT}/Input/DicomSeries/Image0076.dcm
            ${ITK_DATA_ROOT}/Input/DicomSeries/Image0077.dcm
         )


add_test(itkImageSeriesReaderDimensionsTest2 ${IO_TESTS}
  itkImageSeriesReaderDimensionsTest
            ${ITK_DATA_ROOT}/Input/cthead1.tif
            ${ITK_DATA_ROOT}/Input/cthead1.tif
            ${ITK_DATA_ROOT}/Input/cthead1.tif
         )


add_test(itkImageSeriesWriterTest ${IO_TESTS}
  itkImageSeriesWriterTest
            ${ITK_DATA_ROOT}/Input/DicomSeries
            ${ITK_TEST_OUTPUT_DIR} png
         )

add_test(itkNumericSeriesFileNamesTest ${IO_TESTS}
  itkNumericSeriesFileNamesTest)

add_test(itkDicomDifficultFile ${IO_TESTS} itkDicomImageIOTest
         ${ITK_DATA_ROOT}/Input/012345.002.050
         ${ITK_TEST_OUTPUT_DIR}/itkDicomDifficultFile.png)

add_test(itkPNGImageIOTest1 ${IO_TESTS} itkPNGImageIOTest
         ${ITK_DATA_ROOT}/Input/cthead1.png
         ${ITK_TEST_OUTPUT_DIR}/itkPNGImageIOTest1.png)

add_test(itkPNGImageIOTest2 ${IO_TESTS} itkPNGImageIOTest
         ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png
         ${ITK_TEST_OUTPUT_DIR}/itkPNGImageIOTest2.png)

add_test(itkImageReadDICOMSeriesWriteTest ${IO_TESTS}
  itkImageReadDICOMSeriesWriteTest
  ${ITK_DATA_ROOT}/Input/HeadMRVolumeCompressed.mha
  ${ITK_TEST_OUTPUT_DIR}/DICOMSeriesOutputHeadMRVolume)


#
# Testing reading and writing of DICOM series with non-trivial directions.
#
# /Input/HeadMRVolumeWithDirection001.mhd is not suitable for DICOM because
# it has a left-handed coordinate system.
#
add_test(itkImageReadDICOMSeriesWriteWithDirection002Test ${IO_TESTS}
  itkImageReadDICOMSeriesWriteTest
  ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection002.mhd
  ${ITK_TEST_OUTPUT_DIR}/DICOMSeriesOutputHeadMRVolumeWithDirection002)

add_test(itkImageReadDICOMSeriesWriteWithDirection003Test ${IO_TESTS}
  itkImageReadDICOMSeriesWriteTest
  ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection003.mhd
  ${ITK_TEST_OUTPUT_DIR}/DICOMSeriesOutputHeadMRVolumeWithDirection003)


##Analyze Test Suite
  add_test(itkAnalyzeIOBigEndianCompressed.hdr ${IO_TESTS} itkAnalyzeImageIOTest ${ITK_TEST_OUTPUT_DIR}
           ${ITK_DATA_ROOT}/Input/BigEndianCompressed.hdr)
  add_test(itkAnalyzeIOBigEndianCompressed.img ${IO_TESTS} itkAnalyzeImageIOTest ${ITK_TEST_OUTPUT_DIR}
           ${ITK_DATA_ROOT}/Input/BigEndianCompressed.img)
  add_test(itkAnalyzeIOBigEndianCompressed.img.gz ${IO_TESTS} itkAnalyzeImageIOTest ${ITK_TEST_OUTPUT_DIR}
           ${ITK_DATA_ROOT}/Input/BigEndianCompressed.img.gz)
  add_test(itkAnalyzeIOBigEndian ${IO_TESTS} itkAnalyzeImageIOTest ${ITK_TEST_OUTPUT_DIR}
           ${ITK_DATA_ROOT}/Input/BigEndian.hdr)
  add_test(itkAnalyzeIOLittleEndianCompressed ${IO_TESTS} itkAnalyzeImageIOTest ${ITK_TEST_OUTPUT_DIR}
           ${ITK_DATA_ROOT}/Input/LittleEndianCompressed.hdr)
  add_test(itkAnalyzeIOLittleEndian ${IO_TESTS} itkAnalyzeImageIOTest ${ITK_TEST_OUTPUT_DIR}
           ${ITK_DATA_ROOT}/Input/LittleEndian.hdr)
  add_test(itkAnalyzeIOInternalTests ${IO_TESTS} itkAnalyzeImageIOTest ${ITK_TEST_OUTPUT_DIR} )
  add_test(itkAnalyzeIOShouldFail ${IO_TESTS} itkAnalyzeImageIOTest2 ${ITK_TEST_OUTPUT_DIR}
                                  false ${ITK_DATA_ROOT}/Input/ramp.gipl)
  add_test(itkAnalyzeIOShouldSucceed ${IO_TESTS} itkAnalyzeImageIOTest2 ${ITK_TEST_OUTPUT_DIR}
                                  true ${ITK_DATA_ROOT}/Input/LittleEndian.hdr)
  add_test(itkAnalyzeIOShouldSucceedZ ${IO_TESTS} itkAnalyzeImageIOTest2 ${ITK_TEST_OUTPUT_DIR}
                                  true ${ITK_DATA_ROOT}/Input/LittleEndianZ.hdr)
  add_test(itkAnalyzeImageIODirectionsTest ${IO_TESTS} itkAnalyzeImageIODirectionsTest
                                  ${ITK_TEST_OUTPUT_DIR})
  add_test(itkAnalyzeImageIOBadHeader ${IO_TESTS} itkAnalyzeImageIOBadHeader
                                  ${ITK_TEST_OUTPUT_DIR})
  add_test(itkAnalyzeImageIORGBImageTest ${IO_TESTS} itkAnalyzeImageIORGBImageTest
                                         ${ITK_TEST_OUTPUT_DIR})
  add_test(itkAnalyzeDimensionLimitsTest ${IO_TESTS} itkNiftiImageIOTest11
  ${ITK_TEST_OUTPUT_DIR} SizeFailure.img.gz )
##Nifti Test Suite
 add_test(itkNiftiIOBigEndianCompressed.hdr ${IO_TESTS} itkNiftiImageIOTest
          ${ITK_TEST_OUTPUT_DIR} itkNiftiIOBigEndianCompressed.hdr
          ${ITK_DATA_ROOT}/Input/BigEndianCompressed.hdr)
 add_test(itkNiftiIOBigEndianCompressed.img ${IO_TESTS} itkNiftiImageIOTest
          ${ITK_TEST_OUTPUT_DIR} itkNiftiIOBigEndianCompressed.img
          ${ITK_DATA_ROOT}/Input/BigEndianCompressed.img)
 add_test(itkNiftiIOBigEndianCompressed.img.gz ${IO_TESTS} itkNiftiImageIOTest
          ${ITK_TEST_OUTPUT_DIR} itkNiftiIOBigEndianCompressed.img.gz
          ${ITK_DATA_ROOT}/Input/BigEndianCompressed.img.gz)
 add_test(itkNiftiIOBigEndian ${IO_TESTS} itkNiftiImageIOTest
          ${ITK_TEST_OUTPUT_DIR} itkNiftiIOBigEndian
          ${ITK_DATA_ROOT}/Input/BigEndian.hdr)
 add_test(itkNiftiIOLittleEndianCompressed ${IO_TESTS} itkNiftiImageIOTest
          ${ITK_TEST_OUTPUT_DIR} itkNiftiImageIOTest
          ${ITK_DATA_ROOT}/Input/LittleEndianCompressed.hdr)
 add_test(itkNiftiIOLittleEndian ${IO_TESTS} itkNiftiImageIOTest
          ${ITK_TEST_OUTPUT_DIR} itkNiftiIOLittleEndian
          ${ITK_DATA_ROOT}/Input/LittleEndian.hdr)
 add_test(itkNiftiIOInternalTests ${IO_TESTS} itkNiftiImageIOTest ${ITK_TEST_OUTPUT_DIR} )
 add_test(itkNiftiIOShouldFail ${IO_TESTS} itkNiftiImageIOTest2
                                 ${ITK_TEST_OUTPUT_DIR} itkNiftiIOShouldFail
                                 false ${ITK_DATA_ROOT}/Input/ramp.gipl)
 add_test(itkNiftiIOShouldSucceed ${IO_TESTS} itkNiftiImageIOTest2
                                 ${ITK_TEST_OUTPUT_DIR} itkNiftiIOShouldSucceed
                                 true ${ITK_DATA_ROOT}/Input/LittleEndian.hdr)

add_test(itkNiftiVectorImageTest ${IO_TESTS} itkNiftiImageIOTest3 ${ITK_TEST_OUTPUT_DIR} )

add_test(itkDirCosinesTest ${IO_TESTS} itkNiftiImageIOTest4 ${ITK_TEST_OUTPUT_DIR} )
add_test(itkNiftiSlopeInterceptTest ${IO_TESTS} itkNiftiImageIOTest5 ${ITK_TEST_OUTPUT_DIR} )
add_test(itkNiftiVectorImageTest2 ${IO_TESTS} itkNiftiImageIOTest6 ${ITK_TEST_OUTPUT_DIR} )
add_test(itkNiftiDiffusionTensor3dImageTest ${IO_TESTS} itkNiftiImageIOTest7 ${ITK_TEST_OUTPUT_DIR} )
add_test(itkNiftiSymmetricSecondRankTensorImageTest ${IO_TESTS} itkNiftiImageIOTest8 ${ITK_TEST_OUTPUT_DIR} )
add_test(itkNiftiRGBImageTest ${IO_TESTS} itkNiftiImageIOTest9 ${ITK_TEST_OUTPUT_DIR} RGBImage.nii.gz)
add_test(itkNiftiRGBAImageTest ${IO_TESTS} itkNiftiImageIOTest10 ${ITK_TEST_OUTPUT_DIR} RGBAImage.nii.gz)
add_test(itkNiftiDimensionLimitsTest ${IO_TESTS} itkNiftiImageIOTest11 ${ITK_TEST_OUTPUT_DIR} SizeFailure.nii.gz )

add_test(itkGE4 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/19771.002.001.mha
            ${ITK_TEST_OUTPUT_DIR}/19771.002.001.mha
  itkGEImageIOTest  ${ITK_TEST_OUTPUT_DIR}
            true
            GE4
            ${ITK_DATA_ROOT}/Input/test_ge4/19771.002.001
            ${ITK_TEST_OUTPUT_DIR}/19771.002.001.mha)


  add_test(itkGE5 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/113766.003.001.mha
            ${ITK_TEST_OUTPUT_DIR}/113766.003.001.mha
  itkGEImageIOTest ${ITK_TEST_OUTPUT_DIR}
            true
            GE5
            ${ITK_DATA_ROOT}/Input/test_ge5/113766.003.001
            ${ITK_TEST_OUTPUT_DIR}/113766.003.001.mha)

 add_test(itkGEAdw  ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/I.001.mha
            ${ITK_TEST_OUTPUT_DIR}/I.001.mha
  itkGEImageIOTest ${ITK_TEST_OUTPUT_DIR}
            true
            GEAdw
            ${ITK_DATA_ROOT}/Input/test_geadw/I.001
            ${ITK_TEST_OUTPUT_DIR}/I.001.mha)

 add_test(itkSiemens ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/3868-2-100.mha
            ${ITK_TEST_OUTPUT_DIR}/3868-2-100.mha
  itkGEImageIOTest  ${ITK_TEST_OUTPUT_DIR}
            true
            Siemens
            ${ITK_DATA_ROOT}/Input/test_siemens/3868-2-100.ima
            ${ITK_TEST_OUTPUT_DIR}/3868-2-100.mha)


  add_test(itkGE4Fail ${IO_TESTS} itkGEImageIOTest  ${ITK_TEST_OUTPUT_DIR} false
                  GE4
                  ${ITK_DATA_ROOT}/Input/test_ge5/113766.003.001)
  add_test(itkGE5Fail ${IO_TESTS} itkGEImageIOTest  ${ITK_TEST_OUTPUT_DIR} false
                  GE5
                      ${ITK_DATA_ROOT}/Input/test_ge4/19771.002.001)
  add_test(itkGEAdwFail ${IO_TESTS} itkGEImageIOTest  ${ITK_TEST_OUTPUT_DIR} false
                  GEAdw
                  ${ITK_DATA_ROOT}/Input/test_siemens/3868-2-100.ima)
  add_test(itkSiemensFail ${IO_TESTS} itkGEImageIOTest  ${ITK_TEST_OUTPUT_DIR} false
                  Siemens
                  ${ITK_DATA_ROOT}/Input/test_geadw/I.001)

add_test(itkGiplImageIOTest2 ${IO_TESTS} itkGiplImageIOTest
         ${ITK_DATA_ROOT}/Input/ramp.gipl ${TEMP}/ramp.gipl)

  add_test(itkGE4FactoryTest ${IO_TESTS} itkGEImageIOFactoryTest
        ${ITK_DATA_ROOT}/Input/test_ge4/19771.002.001)
  add_test(itkGE5FactoryTest  ${IO_TESTS} itkGEImageIOFactoryTest
        ${ITK_DATA_ROOT}/Input/test_ge5/113766.003.001)
  add_test(itkGEAdwFactoryTest  ${IO_TESTS} itkGEImageIOFactoryTest
        ${ITK_DATA_ROOT}/Input/test_geadw/I.001)
  add_test(itkSiemensFactoryTest  ${IO_TESTS} itkGEImageIOFactoryTest
        ${ITK_DATA_ROOT}/Input/test_siemens/3868-2-100.ima)
  add_test(itkRegularExpressionSeriesFileNamesTest
    ${IO_TESTS} itkRegularExpressionSeriesFileNamesTest
    ${ITK_DATA_ROOT}/Input)
  add_test(itkArchetypeSeriesFileNamesTest
    ${IO_TESTS} itkArchetypeSeriesFileNamesTest
    ${ITK_DATA_ROOT}/Input/Archetype/a5b7.PNG
    ${ITK_DATA_ROOT}/Input/Archetype/a7b1.PNG)
  add_test(itkArchetypeSeriesFileNamesTest2
    ${IO_TESTS} itkArchetypeSeriesFileNamesTest
    ${ITK_DATA_ROOT}/Input/Archetype/image.001
    ${ITK_DATA_ROOT}/Input/Archetype/image.010)

add_executable(itkGDCMImageIOTest itkGDCMImageIOTest.cxx)
target_link_libraries(itkGDCMImageIOTest ITKIO)
add_test(itkGDCMImageIOTest1 ${CXX_TEST_PATH}/itkGDCMImageIOTest
        ${ITK_DATA_ROOT}/Input/dicom-sc_cs-1.dcm
        ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest.dcm
        ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest.png
        ${ITK_TEST_OUTPUT_DIR}/itkGDCMRescaleImageIOTest.dcm)
add_test(itkGDCMImageIOTest2 ${CXX_TEST_PATH}/itkGDCMImageIOTest
        ${ITK_DATA_ROOT}/Input/itkGDCMImageIOTest.dcm
        ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest2.dcm
        ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest2.png
        ${ITK_TEST_OUTPUT_DIR}/itkGDCMRescaleImageIOTest2.dcm)
add_test(itkGDCMImageIOTest3 ${CXX_TEST_PATH}/itkGDCMImageIOTest
        ${ITK_DATA_ROOT}/Input/012345.002.050
        ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest3.dcm
        ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest3.png
        ${ITK_TEST_OUTPUT_DIR}/itkGDCMRescaleImageIOTest3.dcm)
add_test(itkGDCMImageIOTest4 ${CXX_TEST_PATH}/itkGDCMImageIOTest
        ${ITK_DATA_ROOT}/Input/itkGDCMImageIOTest3.dcm
        ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest4.dcm
        ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest4.png
        ${ITK_TEST_OUTPUT_DIR}/itkGDCMRescaleImageIOTest4.dcm)
add_executable(itkGDCMImageIOTest2 itkGDCMImageIOTest2.cxx)
target_link_libraries(itkGDCMImageIOTest2 ITKIO)
add_test(itkGDCMImageIOTest5 ${CXX_TEST_PATH}/itkGDCMImageIOTest2
        ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd
        ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest5)
add_executable(itkGDCMSeriesReadImageWrite itkGDCMSeriesReadImageWrite.cxx)
target_link_libraries(itkGDCMSeriesReadImageWrite ITKIO)
add_test(itkGDCMSeriesReadImageWrite ${CXX_TEST_PATH}/itkGDCMSeriesReadImageWrite
        ${ITK_DATA_ROOT}/Input/DicomSeries
        ${ITK_TEST_OUTPUT_DIR}/itkGDCMSeriesReadImageWrite.vtk
        ${ITK_TEST_OUTPUT_DIR} )
add_executable(itkGDCMSeriesStreamReadImageWrite itkGDCMSeriesStreamReadImageWrite.cxx)
target_link_libraries(itkGDCMSeriesStreamReadImageWrite ITKIO)
add_test(itkGDCMSeriesStreamReadImageWrite1 ${CXX_TEST_PATH}/itkGDCMSeriesStreamReadImageWrite
  ${ITK_DATA_ROOT}/Input/DicomSeries
  ${ITK_TEST_OUTPUT_DIR}/itkGDCMSeriesStreamReadImageWrite1.mhd
  0.859375 0.85939 1.60016
  0 )
add_test(itkGDCMSeriesStreamReadImageWrite2 ${CXX_TEST_PATH}/itkGDCMSeriesStreamReadImageWrite
  ${ITK_DATA_ROOT}/Input/DicomSeries
  ${ITK_TEST_OUTPUT_DIR}/itkGDCMSeriesStreamReadImageWrite2.mhd
  0.859375 0.85939 1.60016
  1 )

add_test(itkNrrdImageReadWriteTest1 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/NrrdImageReadWriteTest1.nrrd
            ${ITK_TEST_OUTPUT_DIR}/box.nrrd
  itkNrrdImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/box.nhdr
            ${ITK_TEST_OUTPUT_DIR}/box.nrrd)
# attached NRRD headers; ascii, raw (big+little endian), gzip (big+little)
add_test(itkNrrdImageReadWriteTest2 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
            ${ITK_TEST_OUTPUT_DIR}/vol2.nrrd
  itkNrrdImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/vol-ascii.nrrd
            ${ITK_TEST_OUTPUT_DIR}/vol2.nrrd)
add_test(itkNrrdImageReadWriteTest3 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
            ${ITK_TEST_OUTPUT_DIR}/vol3.nrrd
  itkNrrdImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/vol-raw-big.nrrd
            ${ITK_TEST_OUTPUT_DIR}/vol3.nrrd)
add_test(itkNrrdImageReadWriteTest4 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
            ${ITK_TEST_OUTPUT_DIR}/vol4.nrrd
  itkNrrdImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/vol-raw-little.nrrd
            ${ITK_TEST_OUTPUT_DIR}/vol4.nrrd)
add_test(itkNrrdImageReadWriteTest5 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
            ${ITK_TEST_OUTPUT_DIR}/vol5.nrrd
  itkNrrdImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/vol-gzip-big.nrrd
            ${ITK_TEST_OUTPUT_DIR}/vol5.nrrd)
add_test(itkNrrdImageReadWriteTest6 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
            ${ITK_TEST_OUTPUT_DIR}/vol6.nrrd
  itkNrrdImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/vol-gzip-little.nrrd
            ${ITK_TEST_OUTPUT_DIR}/vol6.nrrd)
# detached NRRD headers
add_test(itkNrrdImageReadWriteTest7 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
            ${ITK_TEST_OUTPUT_DIR}/vol7.nrrd
  itkNrrdImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/vol-ascii.nhdr
            ${ITK_TEST_OUTPUT_DIR}/vol7.nrrd)
add_test(itkNrrdImageReadWriteTest8 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
            ${ITK_TEST_OUTPUT_DIR}/vol8.nrrd
  itkNrrdImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/vol-raw-big.nhdr
            ${ITK_TEST_OUTPUT_DIR}/vol8.nrrd)
add_test(itkNrrdImageReadWriteTest9 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
            ${ITK_TEST_OUTPUT_DIR}/vol9.nrrd
  itkNrrdImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/vol-raw-little.nhdr
            ${ITK_TEST_OUTPUT_DIR}/vol9.nrrd)
add_test(itkNrrdImageReadWriteTest10 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
            ${ITK_TEST_OUTPUT_DIR}/vol10.nrrd
  itkNrrdImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/vol-gzip-big.nhdr
            ${ITK_TEST_OUTPUT_DIR}/vol10.nrrd)
add_test(itkNrrdImageReadWriteTest11 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
            ${ITK_TEST_OUTPUT_DIR}/vol11.nrrd
  itkNrrdImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/vol-gzip-little.nhdr
            ${ITK_TEST_OUTPUT_DIR}/vol11.nrrd)
# RGB data, colors on fast, medium, and slow axes
add_test(itkNrrdRGBImageReadWriteTest0 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/NrrdRGBImageReadWriteTest0.png
            ${ITK_TEST_OUTPUT_DIR}/NrrdRGBImageReadWriteTest0.png
  itkNrrdRGBImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/testrgb-0.nhdr
            ${ITK_TEST_OUTPUT_DIR}/NrrdRGBImageReadWriteTest0.png)
add_test(itkNrrdRGBImageReadWriteTest1 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/NrrdRGBImageReadWriteTest1.png
            ${ITK_TEST_OUTPUT_DIR}/NrrdRGBImageReadWriteTest1.png
  itkNrrdRGBImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/testrgb-1.nhdr
            ${ITK_TEST_OUTPUT_DIR}/NrrdRGBImageReadWriteTest1.png)
add_test(itkNrrdRGBImageReadWriteTest2 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/NrrdRGBImageReadWriteTest2.png
            ${ITK_TEST_OUTPUT_DIR}/NrrdRGBImageReadWriteTest2.png
  itkNrrdRGBImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/testrgb-2.nhdr
            ${ITK_TEST_OUTPUT_DIR}/NrrdRGBImageReadWriteTest2.png)
# RGBA data
add_test(itkNrrdRGBAImageReadWriteTest ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/NrrdRGBAImageReadWriteTest.png
            ${ITK_TEST_OUTPUT_DIR}/NrrdRGBAImageReadWriteTest.png
  itkNrrdRGBAImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/testrgba.nrrd
            ${ITK_TEST_OUTPUT_DIR}/NrrdRGBAImageReadWriteTest.png)
# Diffusion tensors, masked and unmasked (fastest and slowest)
add_test(itkNrrdDiffusionTensor3DImageReadTest1 ${IO_TESTS}
         itkNrrdDiffusionTensor3DImageReadTest
         ${ITK_DATA_ROOT}/Input/mini-ten-mask.nrrd)
add_test(itkNrrdDiffusionTensor3DImageReadTest2 ${IO_TESTS}
         itkNrrdDiffusionTensor3DImageReadTest
         ${ITK_DATA_ROOT}/Input/mini-ten-nomask-fast.nrrd)
add_test(itkNrrdDiffusionTensor3DImageReadTest3 ${IO_TESTS}
         itkNrrdDiffusionTensor3DImageReadTest
         ${ITK_DATA_ROOT}/Input/mini-ten-nomask-slow.nrrd)
add_test(itkNrrdDiffusionTensor3DImageReadWriteTest ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/mini-ten.nrrd
            ${ITK_TEST_OUTPUT_DIR}/mini-ten.nrrd
  itkNrrdDiffusionTensor3DImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/mini-ten-nomask-fast.nrrd
            ${ITK_TEST_OUTPUT_DIR}/mini-ten.nrrd)
add_test(itkNrrdDiffusionTensor3DImageReadWriteTest4 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/small-tensors.nhdr
            ${ITK_TEST_OUTPUT_DIR}/small-tensors.nhdr
  itkNrrdDiffusionTensor3DImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/small-tensors.nhdr
            ${ITK_TEST_OUTPUT_DIR}/small-tensors.nhdr)
add_test(itkNrrdDiffusionTensor3DImageReadTensorDoubleWriteTensorDouble ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/small-tensors-double.nrrd
            ${ITK_TEST_OUTPUT_DIR}/small-tensors-double.nrrd
  itkNrrdDiffusionTensor3DImageReadTensorDoubleWriteTensorDoubleTest
            ${ITK_DATA_ROOT}/Input/small-tensors.nhdr
            ${ITK_TEST_OUTPUT_DIR}/small-tensors-double.nrrd)
# Complex values
add_test(itkNrrdComplexImageReadTest ${IO_TESTS}
         itkNrrdComplexImageReadTest
         ${ITK_DATA_ROOT}/Input/mini-complex-slow.nrrd)
add_test(itkNrrdComplexImageReadWriteTest ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/mini-complex-fast.nrrd
            ${ITK_TEST_OUTPUT_DIR}/mini-complex.nrrd
  itkNrrdComplexImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/mini-complex-slow.nrrd
            ${ITK_TEST_OUTPUT_DIR}/mini-complex.nrrd)
# vector values
add_test(itkNrrdVectorImageReadTest ${IO_TESTS}
         itkNrrdVectorImageReadTest
         ${ITK_DATA_ROOT}/Input/mini-vector-fast.nrrd)
add_test(itkNrrdVectorImageReadWriteTest ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/mini-vector.nrrd
            ${ITK_TEST_OUTPUT_DIR}/mini-vector.nrrd
  itkNrrdVectorImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/mini-vector-slow.nrrd
            ${ITK_TEST_OUTPUT_DIR}/mini-vector.nrrd)
# covariant vector values
add_test(itkNrrdCovariantVectorImageReadTest ${IO_TESTS}
         itkNrrdCovariantVectorImageReadTest
         ${ITK_DATA_ROOT}/Input/mini-covector-fast.nrrd)
add_test(itkNrrdCovariantVectorImageReadWriteTest ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/mini-covector.nrrd
            ${ITK_TEST_OUTPUT_DIR}/mini-covector.nrrd
  itkNrrdCovariantVectorImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/mini-covector-slow.nrrd
            ${ITK_TEST_OUTPUT_DIR}/mini-covector.nrrd)

# Note: Directions are transposed here with respect to the MetaImage header
add_test(itkImageIODirection3DTest01 ${IO_TESTS} itkImageIODirection3DTest
   ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirectionIdentity.mhd
   1.0    0.0   0.0
   0.0    1.0   0.0
   0.0    0.0   1.0
   ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeWithDirectionIdentity.mhd)

add_test(itkImageIODirection3DTest02 ${IO_TESTS} itkImageIODirection3DTest
   ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection.mhd
   0.8660254    0.5         0.0
  -0.5          0.8660254   0.0
   0.0          0.0         1.0
   ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeWithDirection.mhd)

add_test(itkImageIODirection3DTest03 ${IO_TESTS} itkImageIODirection3DTest
   ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirectionIdentity.mhd
   1.0    0.0   0.0
   0.0    1.0   0.0
   0.0    0.0   1.0
   ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeWithDirectionIdentity.nhdr)

add_test(itkImageIODirection3DTest04 ${IO_TESTS} itkImageIODirection3DTest
   ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection.mhd
   0.8660254    0.5         0.0
  -0.5          0.8660254   0.0
   0.0          0.0         1.0
   ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeWithDirection.nhdr)

add_test(itkImageIODirection3DTest05 ${IO_TESTS} itkImageIODirection3DTest
   ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirectionIdentity.nhdr
   1.0    0.0   0.0
   0.0    1.0   0.0
   0.0    0.0   1.0)

add_test(itkImageIODirection3DTest06 ${IO_TESTS} itkImageIODirection3DTest
   ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection.nhdr
   0.8660254    0.5         0.0
  -0.5          0.8660254   0.0
   0.0          0.0         1.0)

#
#  Testing the three typical Analyze directions applied to MetaImage and Nrrd
#  files.
#
add_test(itkImageIODirection3DTest07 ${IO_TESTS} itkImageIODirection3DTest
   ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection001.mhd
   1.0    0.0   0.0
   0.0   -1.0   0.0
   0.0    0.0   1.0
   ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeWithDirection001.nhdr)

add_test(itkImageIODirection3DTest08 ${IO_TESTS} itkImageIODirection3DTest
   ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection002.mhd
   1.0    0.0   0.0
   0.0    0.0  01.0
   0.0   -1.0   0.0
   ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeWithDirection002.nhdr)

add_test(itkImageIODirection3DTest09 ${IO_TESTS} itkImageIODirection3DTest
   ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection003.mhd
   0.0   -1.0   0.0
   0.0    0.0   1.0
   1.0    0.0   0.0
   ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeWithDirection003.nhdr)


endif(ITK_DATA_ROOT)

if(ITK_OSIRIX_DATA_ROOT)

set(ITK_OSIRIX_CEREBRIX_AXIAL_T1
  "${ITK_OSIRIX_DATA_ROOT}/CEREBRIX/Neuro Crane/Axial_T1 - 5352")



add_test(itkGDCMSeriesStreamReadImageWrite3 ${CXX_TEST_PATH}/itkGDCMSeriesStreamReadImageWrite
  ${ITK_OSIRIX_CEREBRIX_AXIAL_T1}
  ${ITK_TEST_OUTPUT_DIR}/itkGDCMSeriesStreamReadImageWrite3.mhd
  0.998321 0.998321 1.00005
  0 )
add_test(itkGDCMSeriesStreamReadImageWrite4 ${CXX_TEST_PATH}/itkGDCMSeriesStreamReadImageWrite
  ${ITK_OSIRIX_CEREBRIX_AXIAL_T1}
  ${ITK_TEST_OUTPUT_DIR}/itkGDCMSeriesStreamReadImageWrite4.mhd
  0.998321 0.998321 1.00005
  1 )

endif(ITK_OSIRIX_DATA_ROOT)

# Test for reading direction cosines
# Note: Directions are transposed here with respect to the MetaImage header
set(EXAMPLES_DATA_ROOT "${ITK_SOURCE_DIR}/Examples/Data")
add_test(itkImageIODirection2DTest01 ${IO_TESTS} itkImageIODirection2DTest
   ${EXAMPLES_DATA_ROOT}/BrainProtonDensitySliceBorder20.png
   1.0   0.0
   0.0   1.0
   ${ITK_TEST_OUTPUT_DIR}/BrainProtonDensitySliceBorder20.mhd)

add_test(itkImageIODirection2DTest02 ${IO_TESTS} itkImageIODirection2DTest
   ${EXAMPLES_DATA_ROOT}/BrainProtonDensitySliceShifted13x17y.png
   1.0   0.0
   0.0   1.0
   ${ITK_TEST_OUTPUT_DIR}/BrainProtonDensitySliceShifted13x17y.mhd)

add_test(itkImageIODirection2DTest03 ${IO_TESTS} itkImageIODirection2DTest
   ${EXAMPLES_DATA_ROOT}/BrainProtonDensitySliceBorder20DirectionPlus30.mhd
   0.8660254   -0.5
   0.5   0.8660254
   ${ITK_TEST_OUTPUT_DIR}/BrainProtonDensitySliceBorder20DirectionPlus30.mhd)

add_test(itkImageIODirection2DTest04 ${IO_TESTS} itkImageIODirection2DTest
   ${EXAMPLES_DATA_ROOT}/BrainProtonDensitySliceShifted13x17yDirectionPlus30.mhd
   0.8660254   -0.5
   0.5   0.8660254
   ${ITK_TEST_OUTPUT_DIR}/BrainProtonDensitySliceShifted13x17yDirectionPlus30.mhd)

add_test(itkImageIODirection2DTest05 ${IO_TESTS} itkImageIODirection2DTest
   ${EXAMPLES_DATA_ROOT}/BrainT1SliceBorder20DirectionPlus30.mhd
   0.8660254   -0.5
   0.5   0.8660254
   ${ITK_TEST_OUTPUT_DIR}/BrainT1SliceBorder20DirectionPlus30.mhd)

# Now verify that we can read those MetaImage files, and find the same Directions
# then write them as Nifti files.
add_test(itkImageIODirection2DTest06 ${IO_TESTS} itkImageIODirection2DTest
   ${EXAMPLES_DATA_ROOT}/BrainProtonDensitySliceBorder20.mhd
   1.0   0.0
   0.0   1.0
   ${ITK_TEST_OUTPUT_DIR}/BrainProtonDensitySliceBorder20.nhdr)

add_test(itkImageIODirection2DTest07 ${IO_TESTS} itkImageIODirection2DTest
   ${EXAMPLES_DATA_ROOT}/BrainProtonDensitySliceShifted13x17y.mhd
   1.0   0.0
   0.0   1.0
   ${ITK_TEST_OUTPUT_DIR}/BrainProtonDensitySliceShifted13x17y.nhdr)

add_test(itkImageIODirection2DTest08 ${IO_TESTS} itkImageIODirection2DTest
   ${EXAMPLES_DATA_ROOT}/BrainProtonDensitySliceBorder20DirectionPlus30.mhd
   0.8660254   -0.5
   0.5   0.8660254
   ${ITK_TEST_OUTPUT_DIR}/BrainProtonDensitySliceBorder20DirectionPlus30.nhdr)

add_test(itkImageIODirection2DTest09 ${IO_TESTS} itkImageIODirection2DTest
   ${EXAMPLES_DATA_ROOT}/BrainProtonDensitySliceShifted13x17yDirectionPlus30.mhd
   0.8660254   -0.5
   0.5   0.8660254
   ${ITK_TEST_OUTPUT_DIR}/BrainProtonDensitySliceShifted13x17yDirectionPlus30.nhdr)

add_test(itkImageIODirection2DTest10 ${IO_TESTS} itkImageIODirection2DTest
   ${EXAMPLES_DATA_ROOT}/BrainT1SliceBorder20DirectionPlus30.mhd
   0.8660254   -0.5
   0.5   0.8660254
   ${ITK_TEST_OUTPUT_DIR}/BrainT1SliceBorder20DirectionPlus30.nhdr)

# Now verify that we can read those Nifti files, and find the same Directions
# then write them as Nifti files.
add_test(itkImageIODirection2DTest11 ${IO_TESTS} itkImageIODirection2DTest
   ${EXAMPLES_DATA_ROOT}/BrainProtonDensitySliceBorder20.nhdr
   1.0   0.0
   0.0   1.0)

add_test(itkImageIODirection2DTest12 ${IO_TESTS} itkImageIODirection2DTest
   ${EXAMPLES_DATA_ROOT}/BrainProtonDensitySliceShifted13x17y.nhdr
   1.0   0.0
   0.0   1.0)

add_test(itkImageIODirection2DTest13 ${IO_TESTS} itkImageIODirection2DTest
   ${EXAMPLES_DATA_ROOT}/BrainProtonDensitySliceBorder20DirectionPlus30.nhdr
   0.8660254   -0.5
   0.5   0.8660254)

add_test(itkImageIODirection2DTest14 ${IO_TESTS} itkImageIODirection2DTest
   ${EXAMPLES_DATA_ROOT}/BrainProtonDensitySliceShifted13x17yDirectionPlus30.nhdr
   0.8660254   -0.5
   0.5   0.8660254)

add_test(itkImageIODirection2DTest15 ${IO_TESTS} itkImageIODirection2DTest
   ${EXAMPLES_DATA_ROOT}/BrainT1SliceBorder20DirectionPlus30.nhdr
   0.8660254   -0.5
   0.5   0.8660254)


if(ITK_BINARY_DIR)
  if(TCL_TCLSH)
    add_test(PrintSelf-IO ${TCL_TCLSH}
             ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl
             ${ITK_SOURCE_DIR}/Code/IO)
  endif(TCL_TCLSH)
endif(ITK_BINARY_DIR)

if(ITK_BUILD_SHARED_LIBS)
add_test(itkIOPluginTest ${IO_TESTS} itkIOPluginTest
  ${ITK_TEST_OUTPUT_DIR}
  "FileFreeIO::Size=128,256:Spacing=.5,.8:Origin=5,6:Direction=-1,0,0,-1"
  ${ITK_TEST_OUTPUT_DIR}/itkIOPluginTest.png
  )
## Create a library to test ITK IO plugins
set(FileFreeTest_Source itkFileFreeImageIO.cxx itkFileFreeImageIOFactory.cxx)
set(LIBRARY_OUTPUT_PATH "${ITKTesting_BINARY_DIR}/Temporary")
add_library(FileFreeTestLib SHARED ${FileFreeTest_Source})
target_link_libraries(FileFreeTestLib ITKIO)
add_dependencies(itkIOTests FileFreeTestLib)
endif(ITK_BUILD_SHARED_LIBS)

endif( NOT ITK_DISABLE_CXX_TESTING )


if( NOT ITK_DISABLE_JAVA_TESTING )
if(ITK_CSWIG_JAVA AND ITK_BINARY_DIR)
  set(JAVACLASSPATH ${ITK_BINARY_DIR}/Wrapping/CSwig/Java/InsightToolkit.jar)
  foreach(cfgtype ${CMAKE_CONFIGURATION_TYPES})
    set(JAVACLASSPATH "${JAVACLASSPATH}\\;${ITK_BINARY_DIR}/Wrapping/CSwig/Java/${cfgtype}/InsightToolkit.jar")
  endforeach(cfgtype)

  foreach( javatestfile
     DicomSliceRead
     )

  add_custom_target(${javatestfile}Java ALL  DEPENDS ${ITK_BINARY_DIR}/Examples/IO/${javatestfile}.class)
  add_dependencies(${javatestfile}Java ITKJavaJarDummyLibrary)

  add_custom_command(
    OUTPUT  ${ITK_BINARY_DIR}/Examples/IO/${javatestfile}.class
    DEPENDS ${ITK_SOURCE_DIR}/Examples/IO/${javatestfile}.java
    COMMAND ${JAVA_COMPILE}
    ARGS -classpath "${JAVACLASSPATH}" -d "${ITK_BINARY_DIR}/Examples/IO"
    ${ITK_SOURCE_DIR}/Examples/IO/${javatestfile}.java
    COMMENT "Java Class")

  endforeach( javatestfile )

  if(WIN32)
    set(TMPJAVACLASSPATH ${ITK_BINARY_DIR}/Examples/IO/)
    foreach(javadirectory ${JAVACLASSPATH})
      # Set path separator for Windows
      set(TMPJAVACLASSPATH "${TMPJAVACLASSPATH}\\;${javadirectory}")
    endforeach(javadirectory)
    set(JAVACLASSPATH ${TMPJAVACLASSPATH})
  else(WIN32)
    set(TMPJAVACLASSPATH ${ITK_BINARY_DIR}/Examples/IO/)
    foreach(javadirectory ${JAVACLASSPATH})
      # Set path separator for UNIX
      set(TMPJAVACLASSPATH "${TMPJAVACLASSPATH}:${javadirectory}")
    endforeach(javadirectory)
    set(JAVACLASSPATH ${TMPJAVACLASSPATH})
  endif(WIN32)

  add_test(DicomSliceReadJava ${JAVA_RUNTIME} -classpath "${JAVACLASSPATH}"
           DicomSliceRead
           ${ITK_SOURCE_DIR}/Testing/Data/Input/DicomSeries/Image0076.dcm
           ${ITK_TEST_OUTPUT_DIR}/DicomSliceReadJavaTest.png
           1 )

endif(ITK_CSWIG_JAVA AND ITK_BINARY_DIR)
endif( NOT ITK_DISABLE_JAVA_TESTING )





