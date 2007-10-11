
#
# Build ImageCompare: a helper application for comparing tests outputs
#
ADD_EXECUTABLE(ImageCompare ImageCompare.cxx) 
TARGET_LINK_LIBRARIES(ImageCompare ITKIO)
SET(IMAGE_COMPARE "${EXECUTABLE_OUTPUT_PATH}/ImageCompare" CACHE INTERNAL "ImageCompare path to be used by subprojects")

#
# Build ImageCompareCommand: a helper application for comparing tests outputs.
# This is a duplicate application of ImageCompare but with a metaCommand arguments
# interface. This allows to specify in the command line the tolerance parameters 
# to be used for comparing the images.
#
ADD_EXECUTABLE(ImageCompareCommand ImageCompareCommand.cxx) 
TARGET_LINK_LIBRARIES(ImageCompareCommand ITKIO)
SET(IMAGE_COMPARE_COMMAND "${EXECUTABLE_OUTPUT_PATH}/ImageCompareCommand" CACHE INTERNAL "ImageCompare path to be used by subprojects")

#
# Build ImageCopy: a helper application for moving images around
#
ADD_EXECUTABLE(ImageCopy ImageCopy.cxx) 
TARGET_LINK_LIBRARIES(ImageCopy ITKIO)
SET(IMAGE_COPY "${EXECUTABLE_OUTPUT_PATH}/ImageCopy" CACHE INTERNAL "ImageCopy path to be used by subprojects")

#
# Install the executables if we have a destination for them.
#
IF(ITK_INSTALL_BIN_DIR_CM24)
  INSTALL(TARGETS ImageCompare ImageCompareCommand ImageCopy
    RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT Development)
ENDIF(ITK_INSTALL_BIN_DIR_CM24)


IF( NOT ITK_DISABLE_CXX_TESTING )

SET(IO_TESTS ${CXX_TEST_PATH}/itkIOTests)
SET(IO_HEADER_TEST ${CXX_TEST_PATH}/itkIOHeaderTest)


SET(IOTest_SRCS
itkAnalyzeImageIOTest.cxx
itkNiftiImageIOTest.cxx
itkArchetypeSeriesFileNamesTest.cxx
itkBMPImageIOTest.cxx
itkBioRadImageIOTest.cxx
itkLSMImageIOTest.cxx
itkBrains2MaskImageIOTest.cxx
itkConvertBufferTest.cxx
itkConvertBufferTest2.cxx
itkDICOMImageIO2Test.cxx
itkDICOMImageSeriesTest.cxx
itkDICOMSeriesFileNamesTest.cxx
itkDicomImageIOTest.cxx
itkGEImageIOTest.cxx
itkGiplImageIOTest.cxx
itkIOPrintTest.cxx
itkImageFileWriterTest.cxx
itkImageIODirection2DTest.cxx
itkImageIODirection3DTest.cxx
itkImageSeriesWriterTest.cxx
itkJPEGImageIOTest.cxx
itkMeshSpatialObjectIOTest.cxx
itkMetaImageIOTest.cxx
itkMetaImageStreamingIOTest.cxx
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
itkReadWriteSpatialObjectTest.cxx
itkRegularExpressionSeriesFileNamesTest.cxx
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

SET(TEMP ${ITKTesting_BINARY_DIR}/Temporary)

ADD_TEST(itkIOPrintTest ${IO_TESTS} itkIOPrintTest)
ADD_TEST(itkConvertBufferTest2 ${IO_TESTS} itkConvertBufferTest2)
ADD_TEST(itkConvertBufferTest ${IO_TESTS} itkConvertBufferTest)
ADD_TEST(testMetaUtils ${IO_TESTS} testMetaUtils)
ADD_TEST(testMetaArray ${IO_TESTS} testMetaArray)
ADD_TEST(testMetaGroup ${IO_TESTS} testMetaGroup)
ADD_TEST(testMetaObject ${IO_TESTS} testMetaObject)
ADD_TEST(testMetaImage ${IO_TESTS} testMetaImage)
ADD_TEST(testMetaTube ${IO_TESTS} testMetaTube)
ADD_TEST(testMetaBlob ${IO_TESTS} testMetaBlob)
ADD_TEST(testMetaSurface ${IO_TESTS} testMetaSurface)
ADD_TEST(testMetaLine ${IO_TESTS} testMetaLine)
ADD_TEST(testMetaLandmark ${IO_TESTS} testMetaLandmark)
ADD_TEST(testMetaMesh ${IO_TESTS} testMetaMesh )
ADD_TEST(testMetaScene ${IO_TESTS} testMetaScene)

ADD_TEST( MetaCommandRequiredOptionsWithMultipleStrings ${IO_TESTS} testMetaCommand -ExpectedFailStatus 0 -sum 7 -r1c_rs StringValue1 StringValue2 StringValue3 -r2_ri 7)
ADD_TEST( MetaCommandRequiredOptionsWithMultipleStringsAtEnd ${IO_TESTS} testMetaCommand -ExpectedFailStatus 0 -sum 7 -r2_ri 7 -r1c_rs StringValue1 StringValue2 StringValue3)
ADD_TEST( MetaCommandOptionalWithRequired  ${IO_TESTS} testMetaCommand -ExpectedFailStatus 0 -sum 107 -r1c_rs StringValue1 StringValue2 StringValue3 -r2_ri 7 -o1_ri 1 -o2_ri 99)
ADD_TEST( MetaCommandOptionalOptoinsWithOutArguments  ${IO_TESTS} testMetaCommand -ExpectedFailStatus 0 -sum 207 -r1c_rs StringValue1 StringValue2 StringValue3 -r2_ri 7 -o1_oi   -o2_oi )
ADD_TEST( MetaCommandOptionalOptoinsWithOutArguments  ${IO_TESTS} testMetaCommand -ExpectedFailStatus 0 -sum 107 -r1c_rs StringValue1 StringValue2 StringValue3 -r2_ri 7 -o1_oi 1  -o2_oi 99)
ADD_TEST( MetaCommandRequiredAndOptionalOptoinsWithArguments ${IO_TESTS} testMetaCommand -ExpectedFailStatus 0 -sum 207 -r1c_rs StringValue1 StringValue2 StringValue3 -r2_ri 7 -o1_ri 1 -o2_ri 99 -o3_ri_oi 50 50)
ADD_TEST( MetaCommandRequiredAndOptionalOptoinsWithOutArguments ${IO_TESTS}  testMetaCommand -ExpectedFailStatus 0 -sum 257 -r1c_rs StringValue1 StringValue2 StringValue3 -r2_ri 7 -o1_ri 1 -o2_ri 99 -o3_ri_oi 50)
ADD_TEST( MetaCommandRequiredAndOptionalOptoinsWithOutArguments-1 ${IO_TESTS} testMetaCommand -ExpectedFailStatus 0 -sum 457 -r1c_rs StringValue1 StringValue2 StringValue3 -r2_ri 7 -o1_ri 1 -o2_ri 99 -o3_ri_oi 50 -o1_oi -o2_oi )
ADD_TEST( MetaCommandRequiredOptionsWithMissingMultipleStrings  ${IO_TESTS} testMetaCommand -ExpectedFailStatus 1 -sum 7 -r1c_rs  -r2_ri 7)
ADD_TEST( MetaCommandRequiredOptionsWithMissingMultipleStringsAtEnd ${IO_TESTS}  testMetaCommand -ExpectedFailStatus 1 -sum 7 -r2_ri 7 -r1c_rs )
ADD_TEST( MetaCommandOptionalWithRequiredMissing  ${IO_TESTS} testMetaCommand -ExpectedFailStatus 1 -sum 107 -r1c_rs StringValue1 StringValue2 StringValue3 -r2_ri  -o1_ri 1 -o2_ri 99)
ADD_TEST( MetaCommandRequiredAndOptionalOptoinsWithArgumentsMissing  ${IO_TESTS} testMetaCommand -ExpectedFailStatus 1 -sum 207 -r1c_rs StringValue1 StringValue2 StringValue3 -r2_ri 7 -o3_ri_oi )

ADD_TEST(itkNrrdImageIOTest1 ${IO_TESTS} itkNrrdImageIOTest ${ITK_TEST_OUTPUT_DIR}/testNrrd.nrrd)
ADD_TEST(itkNrrdImageIOTest2 ${IO_TESTS} itkNrrdImageIOTest ${ITK_TEST_OUTPUT_DIR}/testNrrd.nhdr)
ADD_TEST(itkReadWriteSpatialObjectTest ${IO_TESTS} itkReadWriteSpatialObjectTest)
ADD_TEST(itkReadWriteSpatialObjectTest1 ${IO_TESTS} itkReadWriteSpatialObjectTest binary)
ADD_TEST(itkReadWriteSpatialObjectTest2 ${IO_TESTS} itkReadWriteSpatialObjectTest 
                                                    ${ITK_DATA_ROOT}/Input/SpatialObjects.meta)
ADD_TEST(itkMeshSpatialObjectIOTest ${IO_TESTS} itkMeshSpatialObjectIOTest)
ADD_TEST(itkMeshSpatialObjectIOTest1 ${IO_TESTS} itkMeshSpatialObjectIOTest binary)
ADD_TEST(itkMeshSpatialObjectIOTest2 ${IO_TESTS} itkMeshSpatialObjectIOTest 
                                                ${ITK_DATA_ROOT}/Input/metamesh.txt)

ADD_TEST(itkTransformIOTest ${IO_TESTS} itkTransformIOTest)

ADD_TEST(itkVTKImageIOTest ${IO_TESTS} itkVTKImageIOTest
                           ${TEMP}/test1.vtk ${TEMP}/test2.vtk)
ADD_TEST(itkVTKImageIOTest2 ${IO_TESTS} itkVTKImageIOTest2
                           ${ITK_DATA_ROOT}/Input/VHFColor.mhd
                           ${TEMP}/testVector.vtk)
ADD_TEST(itkRawImageIOTest ${IO_TESTS} itkRawImageIOTest
                           ${TEMP}/raw1.raw ${TEMP}/raw2.raw
)
ADD_TEST(itkRawImageIOTest3 ${IO_TESTS} itkRawImageIOTest3
                           ${TEMP}/raw1.raw ${TEMP}/raw2.raw
)
ADD_TEST(itkRawImageIOTest4 ${IO_TESTS} itkRawImageIOTest4
                           ${TEMP}/bigendian.raw ${TEMP}/littleendian.raw
)

ADD_TEST(itkRawImageIOTest5 ${IO_TESTS} itkRawImageIOTest5 ${TEMP} )

ADD_TEST(itkVectorImageReadWriteTest ${IO_TESTS} itkVectorImageReadWriteTest
                           ${TEMP}/VectorImageReadWriteTest.mhd
)
ADD_TEST(itkVectorImageReadWriteTest2 ${IO_TESTS} itkVectorImageReadWriteTest
                           ${TEMP}/VectorImageReadWriteTest.nrrd
)

ADD_TEST(itkStimulateImageIOTest ${IO_TESTS} itkStimulateImageIOTest
                           ${TEMP}/test1.spr ${TEMP}/test2.spr)
ADD_TEST(itkIOHeaderTest ${IO_HEADER_TEST})
ADD_TEST(itkPolygonGroupSpatialObjectXMLFileTest ${IO_TESTS} itkPolygonGroupSpatialObjectXMLFileTest ${TEMP})
ADD_TEST(itkImageFileWriterTest ${IO_TESTS} itkImageFileWriterTest ${TEMP}/test.png)
ADD_EXECUTABLE(itkIOHeaderTest itkIOHeaderTest.cxx)
ADD_EXECUTABLE(itkIOTests itkIOTests.cxx ${IOTest_SRCS})
TARGET_LINK_LIBRARIES(itkIOHeaderTest ITKIO)
TARGET_LINK_LIBRARIES(itkIOTests ITKIO ITKEXPAT ITKSpatialObject ITKAlgorithms)

IF(ITK_OBJCXX_COMPILER_WORKS)
  SET(IO_OBJCXX_TEST ${CXX_TEST_PATH}/itkIOHeaderObjCxxTest)
  ADD_TEST(itkIOHeaderObjCxxTest ${IO_OBJCXX_TEST})
  ADD_EXECUTABLE(itkIOHeaderObjCxxTest itkIOHeaderObjCxxTest.mm)
  TARGET_LINK_LIBRARIES(itkIOHeaderObjCxxTest ITKIO ITKEXPAT ITKSpatialObject ITKAlgorithms)
ENDIF(ITK_OBJCXX_COMPILER_WORKS)


IF(ITK_DATA_ROOT)

ADD_TEST(itkRawImageIOTest2 ${IO_TESTS} itkRawImageIOTest2
         ${ITK_DATA_ROOT}/Input/VHFColor.raw)

ADD_TEST(itkMetaImageIOTest ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd
            ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume.mhd
  itkMetaImageIOTest
            ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd
            ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume.mhd)
ADD_TEST(itkMetaImageIOShouldFailTest ${IO_TESTS} 
         itkMetaImageIOTest
         ${ITK_DATA_ROOT}/Input/MetaImageError.mhd
         1)
ADD_TEST(itkMetaImageStreamingIOTest ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd
            ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeStreamed.mhd
  itkMetaImageIOTest
            ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd
            ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeStreamed.mhd)
ADD_TEST(itkMetaImageCompressedStreamingIOTest ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd
            ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeCompressedStreamed.mha
  itkMetaImageIOTest
            ${ITK_DATA_ROOT}/Input/HeadMRVolumeCompressed.mha
            ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeCompressedStreamed.mha)
ADD_TEST(itkGiplImageIOTest ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/ramp.mhd
            ${ITK_TEST_OUTPUT_DIR}/ramp.mhd
  itkGiplImageIOTest 
            ${ITK_DATA_ROOT}/Input/ramp.gipl
            ${ITK_TEST_OUTPUT_DIR}/ramp.mhd)
ADD_TEST(itkGiplImageIOGzTest ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/ramp.mhd
            ${ITK_TEST_OUTPUT_DIR}/ramp.mhd
  itkGiplImageIOTest 
            ${ITK_DATA_ROOT}/Input/ramp.gipl.gz
            ${ITK_TEST_OUTPUT_DIR}/ramp.mhd)
ADD_TEST(itkLSMImageIOTest ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/cthead1.tif
            ${ITK_TEST_OUTPUT_DIR}/cthead1.tif
  itkLSMImageIOTest 
            ${ITK_DATA_ROOT}/Input/cthead1.lsm
            ${ITK_TEST_OUTPUT_DIR}/cthead1.tif)
ADD_TEST(itkBioRadImageIOTest ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/biorad.mhd
            ${ITK_TEST_OUTPUT_DIR}/biorad.pic
  itkBioRadImageIOTest 
            ${ITK_DATA_ROOT}/Input/biorad.pic
            ${ITK_TEST_OUTPUT_DIR}/biorad.pic)
ADD_TEST(itkBMPImageIOTest ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/cthead1.bmp
            ${ITK_TEST_OUTPUT_DIR}/cthead1.bmp
  itkBMPImageIOTest 
            ${ITK_DATA_ROOT}/Input/cthead1.bmp
            ${ITK_TEST_OUTPUT_DIR}/cthead1.bmp)
ADD_TEST(itkBMPImageIOTest2 ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/cthead254x254.bmp
            ${ITK_TEST_OUTPUT_DIR}/cthead254x254.bmp
  itkBMPImageIOTest 
            ${ITK_DATA_ROOT}/Input/cthead254x254.bmp
            ${ITK_TEST_OUTPUT_DIR}/cthead254x254.bmp)
ADD_TEST(itkJPEGImageIOTest ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/cthead1.png
            ${ITK_TEST_OUTPUT_DIR}/cthead1.png
  itkJPEGImageIOTest 
            ${ITK_DATA_ROOT}/Input/cthead1.jpg
            ${ITK_TEST_OUTPUT_DIR}/cthead1.png)
ADD_TEST(itkJPEGImageIOTest2 ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/cthead1.jpg
            ${ITK_TEST_OUTPUT_DIR}/cthead1.jpg
  itkJPEGImageIOTest 
            ${ITK_DATA_ROOT}/Input/cthead1.jpg
            ${ITK_TEST_OUTPUT_DIR}/cthead1.jpg)
ADD_TEST(itkDicomImageIOTest ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/itkDicomImageIOTest.png
            ${ITK_TEST_OUTPUT_DIR}/itkDicomImageIOTest.png
  itkDicomImageIOTest
            ${ITK_DATA_ROOT}/Input/dicom-sc_cs-1.dcm
            ${ITK_TEST_OUTPUT_DIR}/itkDicomImageIOTest.png)
ADD_TEST(itkDICOMImageIO2Test ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/itkDICOMImageIO2Test.png
            ${ITK_TEST_OUTPUT_DIR}/itkDICOMImageIO2Test.png
  itkDICOMImageIO2Test
            ${ITK_DATA_ROOT}/Input/dicom-sc_cs-1.dcm
            ${ITK_TEST_OUTPUT_DIR}/itkDICOMImageIO2Test.png)
ADD_TEST(itkStimulateImageIOTest2 ${IO_TESTS} 
  itkStimulateImageIOTest2
            ${ITK_DATA_ROOT}/Input/BigEndian.spr)

ADD_TEST(itkTIFFImageIOTest ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/cthead1.tif
            ${ITK_TEST_OUTPUT_DIR}/cthead1.tif
  itkTIFFImageIOTest 
            ${ITK_DATA_ROOT}/Input/cthead1.tif
            ${ITK_TEST_OUTPUT_DIR}/cthead1.tif)

ADD_TEST(itkTIFFImageIOTest1 ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/RGBTestImage.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImage.tif
  itkTIFFImageIOTest 
            ${ITK_DATA_ROOT}/Input/RGBTestImage.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImage.tif)

ADD_TEST(itkTIFFImageIOTest2 ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/RGBTestImageCCITTFax3.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImageCCITTFax3.tif
  itkTIFFImageIOTest 
            ${ITK_DATA_ROOT}/Input/RGBTestImageCCITTFax3.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImageCCITTFax3.tif)

ADD_TEST(itkTIFFImageIOTest3 ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/RGBTestImageCCITTFax4.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImageCCITTFax4.tif
  itkTIFFImageIOTest 
            ${ITK_DATA_ROOT}/Input/RGBTestImageCCITTFax4.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImageCCITTFax4.tif)

ADD_TEST(itkTIFFImageIOTest4 ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/RGBTestImageGray.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImageGray.tif
  itkTIFFImageIOTest 
            ${ITK_DATA_ROOT}/Input/RGBTestImageGray.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImageGray.tif)

ADD_TEST(itkTIFFImageIOTest5 ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/RGBTestImageHuffmanRLE.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImageHuffmanRLE.tif
  itkTIFFImageIOTest 
            ${ITK_DATA_ROOT}/Input/RGBTestImageHuffmanRLE.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImageHuffmanRLE.tif)

ADD_TEST(itkTIFFImageIOTest6 ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/RGBTestImageJPEG.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImageJPEG.tif
  itkTIFFImageIOTest 
            ${ITK_DATA_ROOT}/Input/RGBTestImageJPEG.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImageJPEG.tif)

ADD_TEST(itkTIFFImageIOTest7 ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/RGBTestImageLZW.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImageLZW.tif
  itkTIFFImageIOTest 
            ${ITK_DATA_ROOT}/Input/RGBTestImageLZW.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImageLZW.tif)

ADD_TEST(itkTIFFImageIOTest8 ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/RGBTestImageNone.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImageNone.tif
  itkTIFFImageIOTest 
            ${ITK_DATA_ROOT}/Input/RGBTestImageNone.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImageNone.tif)

ADD_TEST(itkTIFFImageIOTest9 ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/RGBTestImagePackBits.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImagePackBits.tif
  itkTIFFImageIOTest 
            ${ITK_DATA_ROOT}/Input/RGBTestImagePackBits.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImagePackBits.tif)

ADD_TEST(itkTIFFImageIOTest10 ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/RGBTestImageZIP.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImageZIP.tif
  itkTIFFImageIOTest 
            ${ITK_DATA_ROOT}/Input/RGBTestImageZIP.tif
            ${ITK_TEST_OUTPUT_DIR}/RGBTestImageZIP.tif)

ADD_TEST(itkTIFFImageIOTest11 ${IO_TESTS}   
   --compare ${ITK_DATA_ROOT}/Baseline/IO/ShortTestImage.mha
             ${ITK_TEST_OUTPUT_DIR}/ShortTestImage.tiff
  itkTIFFImageIOTest 
            ${ITK_DATA_ROOT}/Input/ShortTestImage.tiff
            ${ITK_TEST_OUTPUT_DIR}/ShortTestImage.tiff
            2
            2)

ADD_TEST(itkTIFFImageIOMultiPagesTest ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/ramp.tif
            ${ITK_TEST_OUTPUT_DIR}/ramp.tif
  itkTIFFImageIOTest 
            ${ITK_DATA_ROOT}/Input/ramp.tif
            ${ITK_TEST_OUTPUT_DIR}/ramp.tif
            3
            )

ADD_TEST(itkDICOMImageSeriesTest ${IO_TESTS} 
  itkDICOMImageSeriesTest
            ${ITK_DATA_ROOT}/Input/DicomSeries 0)
ADD_TEST(itkDICOMSeriesFileNamesTest ${IO_TESTS} 
  itkDICOMSeriesFileNamesTest
            ${ITK_DATA_ROOT}/Input/DicomSeries)

ADD_TEST(itkImageSeriesWriterTest ${IO_TESTS} 
  itkImageSeriesWriterTest
            ${ITK_DATA_ROOT}/Input/DicomSeries
            ${ITK_TEST_OUTPUT_DIR} png
         )

ADD_TEST(itkNumericSeriesFileNamesTest ${IO_TESTS} 
  itkNumericSeriesFileNamesTest)

  ADD_TEST(itkDicomDifficultFile ${IO_TESTS} itkDicomImageIOTest
           ${ITK_DATA_ROOT}/Input/012345.002.050
           ${ITK_TEST_OUTPUT_DIR}/itkDicomDifficultFile.png)
  ADD_TEST(itkPNGImageIOTest1 ${IO_TESTS} itkPNGImageIOTest
           ${ITK_DATA_ROOT}/Input/cthead1.png)
  ADD_TEST(itkPNGImageIOTest2 ${IO_TESTS} itkPNGImageIOTest
           ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png)

##Analyze Test Suite
  ADD_TEST(itkAnalyzeImageIOTest ${IO_TESTS}
    itkAnalyzeImageIOTest ${ITK_TEST_OUTPUT_DIR})
  ADD_TEST(itkAnalyzeIOBigEndianCompressed.hdr ${IO_TESTS} itkAnalyzeImageIOTest ${ITK_TEST_OUTPUT_DIR}
           ${ITK_DATA_ROOT}/Input/BigEndianCompressed.hdr)
  ADD_TEST(itkAnalyzeIOBigEndianCompressed.img ${IO_TESTS} itkAnalyzeImageIOTest ${ITK_TEST_OUTPUT_DIR}
           ${ITK_DATA_ROOT}/Input/BigEndianCompressed.img)
  ADD_TEST(itkAnalyzeIOBigEndianCompressed.img.gz ${IO_TESTS} itkAnalyzeImageIOTest ${ITK_TEST_OUTPUT_DIR}
           ${ITK_DATA_ROOT}/Input/BigEndianCompressed.img.gz)
  ADD_TEST(itkAnalyzeIOBigEndian ${IO_TESTS} itkAnalyzeImageIOTest ${ITK_TEST_OUTPUT_DIR}
           ${ITK_DATA_ROOT}/Input/BigEndian.hdr)
  ADD_TEST(itkAnalyzeIOLittleEndianCompressed ${IO_TESTS} itkAnalyzeImageIOTest ${ITK_TEST_OUTPUT_DIR}
           ${ITK_DATA_ROOT}/Input/LittleEndianCompressed.hdr)
  ADD_TEST(itkAnalyzeIOLittleEndian ${IO_TESTS} itkAnalyzeImageIOTest ${ITK_TEST_OUTPUT_DIR}
           ${ITK_DATA_ROOT}/Input/LittleEndian.hdr)
  ADD_TEST(itkAnalyzeIOInternalTests ${IO_TESTS} itkAnalyzeImageIOTest ${ITK_TEST_OUTPUT_DIR} )
  ADD_TEST(itkAnalyzeIOShouldFail ${IO_TESTS} itkAnalyzeImageIOTest2 ${ITK_TEST_OUTPUT_DIR}
                                  false ${ITK_DATA_ROOT}/Input/ramp.gipl)
  ADD_TEST(itkAnalyzeIOShouldSucceed ${IO_TESTS} itkAnalyzeImageIOTest2 ${ITK_TEST_OUTPUT_DIR}
                                  true ${ITK_DATA_ROOT}/Input/LittleEndian.hdr)
##Nifti Test Suite
 ADD_TEST(itkNiftiImageIOTest ${IO_TESTS}
   itkNiftiImageIOTest ${ITK_TEST_OUTPUT_DIR})
 ADD_TEST(itkNiftiIOBigEndianCompressed.hdr ${IO_TESTS} itkNiftiImageIOTest ${ITK_TEST_OUTPUT_DIR}
          ${ITK_DATA_ROOT}/Input/BigEndianCompressed.hdr)
 ADD_TEST(itkNiftiIOBigEndianCompressed.img ${IO_TESTS} itkNiftiImageIOTest ${ITK_TEST_OUTPUT_DIR}
          ${ITK_DATA_ROOT}/Input/BigEndianCompressed.img)
 ADD_TEST(itkNiftiIOBigEndianCompressed.img.gz ${IO_TESTS} itkNiftiImageIOTest ${ITK_TEST_OUTPUT_DIR}
          ${ITK_DATA_ROOT}/Input/BigEndianCompressed.img.gz)
 ADD_TEST(itkNiftiIOBigEndian ${IO_TESTS} itkNiftiImageIOTest ${ITK_TEST_OUTPUT_DIR}
          ${ITK_DATA_ROOT}/Input/BigEndian.hdr)
 ADD_TEST(itkNiftiIOLittleEndianCompressed ${IO_TESTS} itkNiftiImageIOTest ${ITK_TEST_OUTPUT_DIR}
          ${ITK_DATA_ROOT}/Input/LittleEndianCompressed.hdr)
 ADD_TEST(itkNiftiIOLittleEndian ${IO_TESTS} itkNiftiImageIOTest ${ITK_TEST_OUTPUT_DIR}
          ${ITK_DATA_ROOT}/Input/LittleEndian.hdr)
 ADD_TEST(itkNiftiIOInternalTests ${IO_TESTS} itkNiftiImageIOTest ${ITK_TEST_OUTPUT_DIR} )
 ADD_TEST(itkNiftiIOShouldFail ${IO_TESTS} itkNiftiImageIOTest2 ${ITK_TEST_OUTPUT_DIR}
                                 false ${ITK_DATA_ROOT}/Input/ramp.gipl)
 ADD_TEST(itkNiftiIOShouldSucceed ${IO_TESTS} itkNiftiImageIOTest2 ${ITK_TEST_OUTPUT_DIR}
                                 true ${ITK_TEST_OUTPUT_DIR}/NiftiLittleEndian.hdr)

ADD_TEST(itkNiftiVectorImageTest ${IO_TESTS} itkNiftiImageIOTest3 ${ITK_TEST_OUTPUT_DIR} )
ADD_TEST(itkDirCosinesTest ${IO_TESTS} itkNiftiImageIOTest4 ${ITK_TEST_OUTPUT_DIR} )
ADD_TEST(itkNiftiSlopeInterceptTest ${IO_TESTS} itkNiftiImageIOTest5 ${ITK_TEST_OUTPUT_DIR} )

ADD_TEST(itkGE4 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/19771.002.001.mha
            ${ITK_TEST_OUTPUT_DIR}/19771.002.001.mha
  itkGEImageIOTest  ${ITK_TEST_OUTPUT_DIR} 
            true 
            GE4
            ${ITK_DATA_ROOT}/Input/test_ge4/19771.002.001
            ${ITK_TEST_OUTPUT_DIR}/19771.002.001.mha)


  ADD_TEST(itkGE5 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/113766.003.001.mha
            ${ITK_TEST_OUTPUT_DIR}/113766.003.001.mha
  itkGEImageIOTest ${ITK_TEST_OUTPUT_DIR} 
            true 
            GE5
            ${ITK_DATA_ROOT}/Input/test_ge5/113766.003.001
            ${ITK_TEST_OUTPUT_DIR}/113766.003.001.mha)
            
 ADD_TEST(itkGEAdw  ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/I.001.mha
            ${ITK_TEST_OUTPUT_DIR}/I.001.mha
  itkGEImageIOTest ${ITK_TEST_OUTPUT_DIR} 
            true 
            GEAdw 
            ${ITK_DATA_ROOT}/Input/test_geadw/I.001
            ${ITK_TEST_OUTPUT_DIR}/I.001.mha)
            
 ADD_TEST(itkSiemens ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/3868-2-100.mha
            ${ITK_TEST_OUTPUT_DIR}/3868-2-100.mha
  itkGEImageIOTest  ${ITK_TEST_OUTPUT_DIR} 
            true 
            Siemens 
            ${ITK_DATA_ROOT}/Input/test_siemens/3868-2-100.ima
            ${ITK_TEST_OUTPUT_DIR}/3868-2-100.mha)


  ADD_TEST(itkGE4Fail ${IO_TESTS} itkGEImageIOTest  ${ITK_TEST_OUTPUT_DIR} false 
                  GE4 
                  ${ITK_DATA_ROOT}/Input/test_ge5/113766.003.001)
  ADD_TEST(itkGE5Fail ${IO_TESTS} itkGEImageIOTest  ${ITK_TEST_OUTPUT_DIR} false 
                  GE5 
                      ${ITK_DATA_ROOT}/Input/test_ge4/19771.002.001)
  ADD_TEST(itkGEAdwFail ${IO_TESTS} itkGEImageIOTest  ${ITK_TEST_OUTPUT_DIR} false 
                  GEAdw 
                  ${ITK_DATA_ROOT}/Input/test_siemens/3868-2-100.ima)
  ADD_TEST(itkSiemensFail ${IO_TESTS} itkGEImageIOTest  ${ITK_TEST_OUTPUT_DIR} false 
                  Siemens 
                  ${ITK_DATA_ROOT}/Input/test_geadw/I.001)

ADD_TEST(itkGiplImageIOTest2 ${IO_TESTS} itkGiplImageIOTest
         ${ITK_DATA_ROOT}/Input/ramp.gipl ${TEMP}/ramp.gipl)

  ADD_TEST(itkBrains2MaskTest ${IO_TESTS} itkBrains2MaskTest  ${ITK_TEST_OUTPUT_DIR} )

  ADD_TEST(itkGE4FactoryTest ${IO_TESTS} itkGEImageIOFactoryTest 
        ${ITK_DATA_ROOT}/Input/test_ge4/19771.002.001)
  ADD_TEST(itkGE5FactoryTest  ${IO_TESTS} itkGEImageIOFactoryTest
        ${ITK_DATA_ROOT}/Input/test_ge5/113766.003.001)
  ADD_TEST(itkGEAdwFactoryTest  ${IO_TESTS} itkGEImageIOFactoryTest
        ${ITK_DATA_ROOT}/Input/test_geadw/I.001)
  ADD_TEST(itkSiemensFactoryTest  ${IO_TESTS} itkGEImageIOFactoryTest
        ${ITK_DATA_ROOT}/Input/test_siemens/3868-2-100.ima)
  ADD_TEST(itkRegularExpressionSeriesFileNamesTest
    ${IO_TESTS} itkRegularExpressionSeriesFileNamesTest
    ${ITK_DATA_ROOT}/Input)
  ADD_TEST(itkArchetypeSeriesFileNamesTest
    ${IO_TESTS} itkArchetypeSeriesFileNamesTest
    ${ITK_DATA_ROOT}/Input/Archetype/a5b7.PNG
    ${ITK_DATA_ROOT}/Input/Archetype/a7b1.PNG)
  ADD_TEST(itkArchetypeSeriesFileNamesTest2
    ${IO_TESTS} itkArchetypeSeriesFileNamesTest
    ${ITK_DATA_ROOT}/Input/Archetype/image.001
    ${ITK_DATA_ROOT}/Input/Archetype/image.010)

ADD_EXECUTABLE(itkGDCMImageIOTest itkGDCMImageIOTest.cxx)
TARGET_LINK_LIBRARIES(itkGDCMImageIOTest ITKIO)
ADD_TEST(itkGDCMImageIOTest1 ${CXX_TEST_PATH}/itkGDCMImageIOTest
        ${ITK_DATA_ROOT}/Input/dicom-sc_cs-1.dcm
        ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest.dcm
        ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest.png
        ${ITK_TEST_OUTPUT_DIR}/itkGDCMRescaleImageIOTest.dcm)
ADD_TEST(itkGDCMImageIOTest2 ${CXX_TEST_PATH}/itkGDCMImageIOTest
        ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest.dcm
        ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest2.dcm
        ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest2.png
        ${ITK_TEST_OUTPUT_DIR}/itkGDCMRescaleImageIOTest2.dcm)
ADD_TEST(itkGDCMImageIOTest3 ${CXX_TEST_PATH}/itkGDCMImageIOTest
        ${ITK_DATA_ROOT}/Input/012345.002.050
        ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest3.dcm
        ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest3.png
        ${ITK_TEST_OUTPUT_DIR}/itkGDCMRescaleImageIOTest3.dcm)
ADD_TEST(itkGDCMImageIOTest4 ${CXX_TEST_PATH}/itkGDCMImageIOTest
        ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest3.dcm
        ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest4.dcm
        ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest4.png
        ${ITK_TEST_OUTPUT_DIR}/itkGDCMRescaleImageIOTest4.dcm)
ADD_EXECUTABLE(itkGDCMImageIOTest2 itkGDCMImageIOTest2.cxx)
TARGET_LINK_LIBRARIES(itkGDCMImageIOTest2 ITKIO)
ADD_TEST(itkGDCMImageIOTest5 ${CXX_TEST_PATH}/itkGDCMImageIOTest2
        ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd
        ${ITK_TEST_OUTPUT_DIR}/itkGDCMImageIOTest5)
ADD_EXECUTABLE(itkGDCMSeriesReadImageWrite itkGDCMSeriesReadImageWrite.cxx)
TARGET_LINK_LIBRARIES(itkGDCMSeriesReadImageWrite ITKIO)
ADD_TEST(itkGDCMSeriesReadImageWrite ${CXX_TEST_PATH}/itkGDCMSeriesReadImageWrite
        ${ITK_DATA_ROOT}/Input/DicomSeries
        ${ITK_TEST_OUTPUT_DIR}/itkGDCMSeriesReadImageWrite.vtk
        ${ITK_TEST_OUTPUT_DIR} )
ADD_TEST(itkNrrdImageReadWriteTest1 ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/NrrdImageReadWriteTest1.nrrd
            ${ITK_TEST_OUTPUT_DIR}/box.nrrd
  itkNrrdImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/box.nhdr
            ${ITK_TEST_OUTPUT_DIR}/box.nrrd)
# attached NRRD headers; ascii, raw (big+little endian), gzip (big+little)
ADD_TEST(itkNrrdImageReadWriteTest2 ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
            ${ITK_TEST_OUTPUT_DIR}/vol.nrrd
  itkNrrdImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/vol-ascii.nrrd
            ${ITK_TEST_OUTPUT_DIR}/vol.nrrd)
ADD_TEST(itkNrrdImageReadWriteTest3 ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
            ${ITK_TEST_OUTPUT_DIR}/vol.nrrd
  itkNrrdImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/vol-raw-big.nrrd
            ${ITK_TEST_OUTPUT_DIR}/vol.nrrd)
ADD_TEST(itkNrrdImageReadWriteTest4 ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
            ${ITK_TEST_OUTPUT_DIR}/vol.nrrd
  itkNrrdImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/vol-raw-little.nrrd
            ${ITK_TEST_OUTPUT_DIR}/vol.nrrd)
ADD_TEST(itkNrrdImageReadWriteTest5 ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
            ${ITK_TEST_OUTPUT_DIR}/vol.nrrd
  itkNrrdImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/vol-gzip-big.nrrd
            ${ITK_TEST_OUTPUT_DIR}/vol.nrrd)
ADD_TEST(itkNrrdImageReadWriteTest6 ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
            ${ITK_TEST_OUTPUT_DIR}/vol.nrrd
  itkNrrdImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/vol-gzip-little.nrrd
            ${ITK_TEST_OUTPUT_DIR}/vol.nrrd)
# detached NRRD headers
ADD_TEST(itkNrrdImageReadWriteTest7 ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
            ${ITK_TEST_OUTPUT_DIR}/vol.nrrd
  itkNrrdImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/vol-ascii.nhdr
            ${ITK_TEST_OUTPUT_DIR}/vol.nrrd)
ADD_TEST(itkNrrdImageReadWriteTest8 ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
            ${ITK_TEST_OUTPUT_DIR}/vol.nrrd
  itkNrrdImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/vol-raw-big.nhdr
            ${ITK_TEST_OUTPUT_DIR}/vol.nrrd)
ADD_TEST(itkNrrdImageReadWriteTest9 ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
            ${ITK_TEST_OUTPUT_DIR}/vol.nrrd
  itkNrrdImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/vol-raw-little.nhdr
            ${ITK_TEST_OUTPUT_DIR}/vol.nrrd)
ADD_TEST(itkNrrdImageReadWriteTest10 ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
            ${ITK_TEST_OUTPUT_DIR}/vol.nrrd
  itkNrrdImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/vol-gzip-big.nhdr
            ${ITK_TEST_OUTPUT_DIR}/vol.nrrd)
ADD_TEST(itkNrrdImageReadWriteTest11 ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/vol-ascii.nrrd
            ${ITK_TEST_OUTPUT_DIR}/vol.nrrd
  itkNrrdImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/vol-gzip-little.nhdr
            ${ITK_TEST_OUTPUT_DIR}/vol.nrrd)
# RGB data, colors on fast, medium, and slow axes
ADD_TEST(itkNrrdRGBImageReadWriteTest0 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/NrrdRGBImageReadWriteTest0.png
            ${ITK_TEST_OUTPUT_DIR}/NrrdRGBImageReadWriteTest0.png
  itkNrrdRGBImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/testrgb-0.nhdr
            ${ITK_TEST_OUTPUT_DIR}/NrrdRGBImageReadWriteTest0.png)
ADD_TEST(itkNrrdRGBImageReadWriteTest1 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/NrrdRGBImageReadWriteTest1.png
            ${ITK_TEST_OUTPUT_DIR}/NrrdRGBImageReadWriteTest1.png
  itkNrrdRGBImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/testrgb-1.nhdr
            ${ITK_TEST_OUTPUT_DIR}/NrrdRGBImageReadWriteTest1.png)
ADD_TEST(itkNrrdRGBImageReadWriteTest2 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/NrrdRGBImageReadWriteTest2.png
            ${ITK_TEST_OUTPUT_DIR}/NrrdRGBImageReadWriteTest2.png
  itkNrrdRGBImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/testrgb-2.nhdr
            ${ITK_TEST_OUTPUT_DIR}/NrrdRGBImageReadWriteTest2.png)
# RGBA data
ADD_TEST(itkNrrdRGBAImageReadWriteTest ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/NrrdRGBAImageReadWriteTest.png
            ${ITK_TEST_OUTPUT_DIR}/NrrdRGBAImageReadWriteTest.png
  itkNrrdRGBAImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/testrgba.nrrd
            ${ITK_TEST_OUTPUT_DIR}/NrrdRGBAImageReadWriteTest.png)
# Diffusion tensors, masked and unmasked (fastest and slowest)
ADD_TEST(itkNrrdDiffusionTensor3DImageReadTest1 ${IO_TESTS}
         itkNrrdDiffusionTensor3DImageReadTest
         ${ITK_DATA_ROOT}/Input/mini-ten-mask.nrrd)
ADD_TEST(itkNrrdDiffusionTensor3DImageReadTest2 ${IO_TESTS}
         itkNrrdDiffusionTensor3DImageReadTest
         ${ITK_DATA_ROOT}/Input/mini-ten-nomask-fast.nrrd)
ADD_TEST(itkNrrdDiffusionTensor3DImageReadTest3 ${IO_TESTS}
         itkNrrdDiffusionTensor3DImageReadTest
         ${ITK_DATA_ROOT}/Input/mini-ten-nomask-slow.nrrd)
ADD_TEST(itkNrrdDiffusionTensor3DImageReadWriteTest ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/mini-ten.nrrd
            ${ITK_TEST_OUTPUT_DIR}/mini-ten.nrrd
  itkNrrdDiffusionTensor3DImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/mini-ten-nomask-fast.nrrd
            ${ITK_TEST_OUTPUT_DIR}/mini-ten.nrrd)
ADD_TEST(itkNrrdDiffusionTensor3DImageReadWriteTest4 ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/small-tensors.nhdr
            ${ITK_TEST_OUTPUT_DIR}/small-tensors.nhdr
  itkNrrdDiffusionTensor3DImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/small-tensors.nhdr
            ${ITK_TEST_OUTPUT_DIR}/small-tensors.nhdr)
ADD_TEST(itkNrrdDiffusionTensor3DImageReadTensorDoubleWriteTensorDouble ${IO_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/IO/small-tensors-double.nrrd
            ${ITK_TEST_OUTPUT_DIR}/small-tensors-double.nrrd
  itkNrrdDiffusionTensor3DImageReadTensorDoubleWriteTensorDoubleTest
            ${ITK_DATA_ROOT}/Input/small-tensors.nhdr
            ${ITK_TEST_OUTPUT_DIR}/small-tensors-double.nrrd)
# Complex values
ADD_TEST(itkNrrdComplexImageReadTest ${IO_TESTS}
         itkNrrdComplexImageReadTest
         ${ITK_DATA_ROOT}/Input/mini-complex-slow.nrrd)
ADD_TEST(itkNrrdComplexImageReadWriteTest ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/mini-complex-fast.nrrd
            ${ITK_TEST_OUTPUT_DIR}/mini-complex.nrrd
  itkNrrdComplexImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/mini-complex-slow.nrrd
            ${ITK_TEST_OUTPUT_DIR}/mini-complex.nrrd)
# vector values
ADD_TEST(itkNrrdVectorImageReadTest ${IO_TESTS}
         itkNrrdVectorImageReadTest
         ${ITK_DATA_ROOT}/Input/mini-vector-fast.nrrd)
ADD_TEST(itkNrrdVectorImageReadWriteTest ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/mini-vector.nrrd
            ${ITK_TEST_OUTPUT_DIR}/mini-vector.nrrd
  itkNrrdVectorImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/mini-vector-slow.nrrd
            ${ITK_TEST_OUTPUT_DIR}/mini-vector.nrrd)
# covariant vector values
ADD_TEST(itkNrrdCovariantVectorImageReadTest ${IO_TESTS}
         itkNrrdCovariantVectorImageReadTest
         ${ITK_DATA_ROOT}/Input/mini-covector-fast.nrrd)
ADD_TEST(itkNrrdCovariantVectorImageReadWriteTest ${IO_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/mini-covector.nrrd
            ${ITK_TEST_OUTPUT_DIR}/mini-covector.nrrd
  itkNrrdCovariantVectorImageReadWriteTest
            ${ITK_DATA_ROOT}/Input/mini-covector-slow.nrrd
            ${ITK_TEST_OUTPUT_DIR}/mini-covector.nrrd)

# Note: Directions are transposed here with respect to the MetaImage header
ADD_TEST(itkImageIODirection3DTest01 ${IO_TESTS} itkImageIODirection3DTest
   ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirectionIdentity.mhd
   1.0    0.0   0.0 
   0.0    1.0   0.0 
   0.0    0.0   1.0
   ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeWithDirectionIdentity.mhd)

ADD_TEST(itkImageIODirection3DTest02 ${IO_TESTS} itkImageIODirection3DTest
   ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection.mhd  
   0.8660254    0.5         0.0  
  -0.5          0.8660254   0.0 
   0.0          0.0         1.0
   ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeWithDirection.mhd)

ADD_TEST(itkImageIODirection3DTest03 ${IO_TESTS} itkImageIODirection3DTest
   ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeWithDirectionIdentity.mhd
   1.0    0.0   0.0 
   0.0    1.0   0.0 
   0.0    0.0   1.0
   ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeWithDirectionIdentity.nhdr)

ADD_TEST(itkImageIODirection3DTest04 ${IO_TESTS} itkImageIODirection3DTest
   ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeWithDirection.mhd
   0.8660254    0.5         0.0  
  -0.5          0.8660254   0.0 
   0.0          0.0         1.0
   ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeWithDirection.nhdr)

ADD_TEST(itkImageIODirection3DTest05 ${IO_TESTS} itkImageIODirection3DTest
   ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeWithDirectionIdentity.nhdr
   1.0    0.0   0.0 
   0.0    1.0   0.0 
   0.0    0.0   1.0)

ADD_TEST(itkImageIODirection3DTest06 ${IO_TESTS} itkImageIODirection3DTest
   ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeWithDirection.nhdr
   0.8660254    0.5         0.0  
  -0.5          0.8660254   0.0 
   0.0          0.0         1.0)


ENDIF(ITK_DATA_ROOT)


# Test for reading direction cosines
# Note: Directions are transposed here with respect to the MetaImage header
SET(EXAMPLES_DATA_ROOT "${ITK_SOURCE_DIR}/Examples/Data")
ADD_TEST(itkImageIODirection2DTest01 ${IO_TESTS} itkImageIODirection2DTest
   ${EXAMPLES_DATA_ROOT}/BrainProtonDensitySliceBorder20.png  
   1.0   0.0 
   0.0   1.0)

ADD_TEST(itkImageIODirection2DTest02 ${IO_TESTS} itkImageIODirection2DTest
   ${EXAMPLES_DATA_ROOT}/BrainProtonDensitySliceShifted13x17y.png  
   1.0   0.0 
   0.0   1.0)

ADD_TEST(itkImageIODirection2DTest03 ${IO_TESTS} itkImageIODirection2DTest
   ${EXAMPLES_DATA_ROOT}/BrainProtonDensitySliceBorder20DirectionPlus30.mhd
   0.8660254   -0.5  
   0.5   0.8660254)

ADD_TEST(itkImageIODirection2DTest04 ${IO_TESTS} itkImageIODirection2DTest
   ${EXAMPLES_DATA_ROOT}/BrainProtonDensitySliceShifted13x17yDirectionPlus30.mhd
   0.8660254   -0.5  
   0.5   0.8660254)

ADD_TEST(itkImageIODirection2DTest05 ${IO_TESTS} itkImageIODirection2DTest
   ${EXAMPLES_DATA_ROOT}/BrainT1SliceBorder20DirectionPlus30.mhd
   0.8660254   -0.5  
   0.5   0.8660254)


IF(ITK_BINARY_DIR)
  IF(TCL_TCLSH)
    ADD_TEST(PrintSelf-IO ${TCL_TCLSH}
             ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl  
             ${ITK_SOURCE_DIR}/Code/IO)
  ENDIF(TCL_TCLSH)
ENDIF(ITK_BINARY_DIR)

ENDIF( NOT ITK_DISABLE_CXX_TESTING )


IF( NOT ITK_DISABLE_JAVA_TESTING )
IF(ITK_CSWIG_JAVA AND ITK_BINARY_DIR)
  SET(JAVACLASSPATH ${ITK_BINARY_DIR}/Wrapping/CSwig/Java/InsightToolkit.jar)
  FOREACH(cfgtype ${CMAKE_CONFIGURATION_TYPES})
    SET(JAVACLASSPATH "${JAVACLASSPATH}\\;${ITK_BINARY_DIR}/Wrapping/CSwig/Java/${cfgtype}/InsightToolkit.jar")
  ENDFOREACH(cfgtype)

  FOREACH( javatestfile 
     DicomSliceRead
     )

  ADD_CUSTOM_TARGET(${javatestfile}Java ALL  DEPENDS ${ITK_BINARY_DIR}/Examples/IO/${javatestfile}.class)
  ADD_DEPENDENCIES(${javatestfile}Java ITKJavaJarDummyLibrary)

  ADD_CUSTOM_COMMAND(
    OUTPUT  ${ITK_BINARY_DIR}/Examples/IO/${javatestfile}.class
    DEPENDS ${ITK_SOURCE_DIR}/Examples/IO/${javatestfile}.java
    COMMAND ${JAVA_COMPILE}
    ARGS -classpath "${JAVACLASSPATH}" -d "${ITK_BINARY_DIR}/Examples/IO"
    ${ITK_SOURCE_DIR}/Examples/IO/${javatestfile}.java
    COMMENT "Java Class")

  ENDFOREACH( javatestfile )

  IF(WIN32)
    SET(TMPJAVACLASSPATH ${ITK_BINARY_DIR}/Examples/IO/)
    FOREACH(javadirectory ${JAVACLASSPATH})
      # Set path separator for Windows
      SET(TMPJAVACLASSPATH "${TMPJAVACLASSPATH}\\;${javadirectory}")
    ENDFOREACH(javadirectory)
    SET(JAVACLASSPATH ${TMPJAVACLASSPATH})
  ELSE(WIN32)
    SET(TMPJAVACLASSPATH ${ITK_BINARY_DIR}/Examples/IO/)
    FOREACH(javadirectory ${JAVACLASSPATH})
      # Set path separator for UNIX
      SET(TMPJAVACLASSPATH "${TMPJAVACLASSPATH}:${javadirectory}")
    ENDFOREACH(javadirectory)
    SET(JAVACLASSPATH ${TMPJAVACLASSPATH})
  ENDIF(WIN32)
 
  ADD_TEST(DicomSliceReadJava ${JAVA_RUNTIME} -classpath "${JAVACLASSPATH}"
           DicomSliceRead
           ${ITK_SOURCE_DIR}/Testing/Data/Input/DicomSeries/Image0076.dcm
           ${ITK_TEST_OUTPUT_DIR}/DicomSliceReadJavaTest.png  
           1 )  

ENDIF(ITK_CSWIG_JAVA AND ITK_BINARY_DIR)
ENDIF( NOT ITK_DISABLE_JAVA_TESTING )





