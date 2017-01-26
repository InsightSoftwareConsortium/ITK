itk_module_test()
set(ITKIOTIFFTests
itkTIFFImageIOTest.cxx
itkTIFFImageIOTest2.cxx
itkTIFFImageIOCompressionTest.cxx
itkLargeTIFFImageWriteReadTest.cxx
itkTIFFImageIOInfoTest.cxx
itkTIFFImageIOTestPalette.cxx
)

CreateTestDriver(ITKIOTIFF  "${ITKIOTIFF-Test_LIBRARIES}" "${ITKIOTIFFTests}")

itk_add_test(NAME itkTIFFImageIOTest
      COMMAND ITKIOTIFFTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/cthead1.tif}
              ${ITK_TEST_OUTPUT_DIR}/cthead1.tif
    itkTIFFImageIOTest DATA{${ITK_DATA_ROOT}/Input/cthead1.tif} ${ITK_TEST_OUTPUT_DIR}/cthead1.tif)
itk_add_test(NAME itkTIFFImageIOTest1
      COMMAND ITKIOTIFFTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/RGBTestImage.tif}
              ${ITK_TEST_OUTPUT_DIR}/RGBTestImage.tif
    itkTIFFImageIOTest DATA{${ITK_DATA_ROOT}/Input/RGBTestImage.tif} ${ITK_TEST_OUTPUT_DIR}/RGBTestImage.tif)
itk_add_test(NAME itkTIFFImageIOTest2
      COMMAND ITKIOTIFFTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/RGBTestImageCCITTFax3.tif}
              ${ITK_TEST_OUTPUT_DIR}/RGBTestImageCCITTFax3.tif
    itkTIFFImageIOTest DATA{${ITK_DATA_ROOT}/Input/RGBTestImageCCITTFax3.tif} ${ITK_TEST_OUTPUT_DIR}/RGBTestImageCCITTFax3.tif)
itk_add_test(NAME itkTIFFImageIOTest3
      COMMAND ITKIOTIFFTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/RGBTestImageCCITTFax4.tif}
              ${ITK_TEST_OUTPUT_DIR}/RGBTestImageCCITTFax4.tif
    itkTIFFImageIOTest DATA{${ITK_DATA_ROOT}/Input/RGBTestImageCCITTFax4.tif} ${ITK_TEST_OUTPUT_DIR}/RGBTestImageCCITTFax4.tif)
itk_add_test(NAME itkTIFFImageIOTest4
      COMMAND ITKIOTIFFTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/RGBTestImageGray.tif}
              ${ITK_TEST_OUTPUT_DIR}/RGBTestImageGray.tif
    itkTIFFImageIOTest DATA{${ITK_DATA_ROOT}/Input/RGBTestImageGray.tif} ${ITK_TEST_OUTPUT_DIR}/RGBTestImageGray.tif)
itk_add_test(NAME itkTIFFImageIOTest5
      COMMAND ITKIOTIFFTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/RGBTestImageHuffmanRLE.tif}
              ${ITK_TEST_OUTPUT_DIR}/RGBTestImageHuffmanRLE.tif
    itkTIFFImageIOTest DATA{${ITK_DATA_ROOT}/Input/RGBTestImageHuffmanRLE.tif} ${ITK_TEST_OUTPUT_DIR}/RGBTestImageHuffmanRLE.tif)
itk_add_test(NAME itkTIFFImageIOTest6
      COMMAND ITKIOTIFFTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/RGBTestImageJPEG.tif}
              ${ITK_TEST_OUTPUT_DIR}/RGBTestImageJPEG.tif
    itkTIFFImageIOTest DATA{${ITK_DATA_ROOT}/Input/RGBTestImageJPEG.tif} ${ITK_TEST_OUTPUT_DIR}/RGBTestImageJPEG.tif)
itk_add_test(NAME itkTIFFImageIOTest7
      COMMAND ITKIOTIFFTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/RGBTestImageLZW.tif}
              ${ITK_TEST_OUTPUT_DIR}/RGBTestImageLZW.tif
    itkTIFFImageIOTest DATA{${ITK_DATA_ROOT}/Input/RGBTestImageLZW.tif} ${ITK_TEST_OUTPUT_DIR}/RGBTestImageLZW.tif)
itk_add_test(NAME itkTIFFImageIOTest8
      COMMAND ITKIOTIFFTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/RGBTestImageNone.tif}
              ${ITK_TEST_OUTPUT_DIR}/RGBTestImageNone.tif
    itkTIFFImageIOTest DATA{${ITK_DATA_ROOT}/Input/RGBTestImageNone.tif} ${ITK_TEST_OUTPUT_DIR}/RGBTestImageNone.tif)
itk_add_test(NAME itkTIFFImageIOTest9
      COMMAND ITKIOTIFFTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/RGBTestImagePackBits.tif}
              ${ITK_TEST_OUTPUT_DIR}/RGBTestImagePackBits.tif
    itkTIFFImageIOTest DATA{${ITK_DATA_ROOT}/Input/RGBTestImagePackBits.tif} ${ITK_TEST_OUTPUT_DIR}/RGBTestImagePackBits.tif)
itk_add_test(NAME itkTIFFImageIOTest10
      COMMAND ITKIOTIFFTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/RGBTestImageZIP.tif}
              ${ITK_TEST_OUTPUT_DIR}/RGBTestImageZIP.tif
    itkTIFFImageIOTest DATA{${ITK_DATA_ROOT}/Input/RGBTestImageZIP.tif} ${ITK_TEST_OUTPUT_DIR}/RGBTestImageZIP.tif)
itk_add_test(NAME itkTIFFImageIOTest11
      COMMAND ITKIOTIFFTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/ShortTestImage.tiff}
              ${ITK_TEST_OUTPUT_DIR}/ShortTestImage.tiff
    itkTIFFImageIOTest DATA{${ITK_DATA_ROOT}/Input/ShortTestImage.tiff} ${ITK_TEST_OUTPUT_DIR}/ShortTestImage.tiff 2 2)
itk_add_test(NAME itkTIFFImageIOTest12
      COMMAND ITKIOTIFFTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/48BitTestImage.tif}
              ${ITK_TEST_OUTPUT_DIR}/48BitTestImage.tif
    itkTIFFImageIOTest DATA{${ITK_DATA_ROOT}/Input/48BitTestImage.tif} ${ITK_TEST_OUTPUT_DIR}/48BitTestImage.tif 2 2)
itk_add_test(NAME itkTIFFImageIOTest13
      COMMAND ITKIOTIFFTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/RGBTestImageCCITTFax3.mha}
              ${ITK_TEST_OUTPUT_DIR}/RGBTestImageCCITTFax3.mha
    itkTIFFImageIOTest DATA{${ITK_DATA_ROOT}/Input/RGBTestImageCCITTFax3.tif} ${ITK_TEST_OUTPUT_DIR}/RGBTestImageCCITTFax3.mha)
itk_add_test(NAME itkTIFFImageIOTest14
      COMMAND ITKIOTIFFTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/RGBTestImageCCITTFax4.mha}
              ${ITK_TEST_OUTPUT_DIR}/RGBTestImageCCITTFax4.mha
    itkTIFFImageIOTest DATA{${ITK_DATA_ROOT}/Input/RGBTestImageCCITTFax4.tif} ${ITK_TEST_OUTPUT_DIR}/RGBTestImageCCITTFax4.mha)
itk_add_test(NAME itkTIFFImageIOTest15
      COMMAND ITKIOTIFFTestDriver
   --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkTIFFImageIOTest15.png
              fd894cf735a09380c3492f46b39a0ab9
    itkTIFFImageIOTest DATA{Input/jello.tif} ${ITK_TEST_OUTPUT_DIR}/itkTIFFImageIOTest15.png 2 2)

itk_add_test(NAME itkTIFFImageIOMultiPagesTest
      COMMAND ITKIOTIFFTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/ramp.tif}
              ${ITK_TEST_OUTPUT_DIR}/ramp.tif
    itkTIFFImageIOTest DATA{${ITK_DATA_ROOT}/Input/ramp.tif} ${ITK_TEST_OUTPUT_DIR}/ramp.tif 3)
itk_add_test(NAME itkTIFFImageIOMultiPagesTest2
      COMMAND ITKIOTIFFTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/rampShort.tif}
              ${ITK_TEST_OUTPUT_DIR}/rampShort.tif
    itkTIFFImageIOTest DATA{${ITK_DATA_ROOT}/Input/rampShort.tif} ${ITK_TEST_OUTPUT_DIR}/rampShort.tif 3 3)
itk_add_test(NAME itkTIFFImageIOMultResTest1
   COMMAND ITKIOTIFFTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/cthead1.tif}
              ${ITK_TEST_OUTPUT_DIR}/itkTIFFImageIOMultResTest1.tif
    itkTIFFImageIOTest DATA{${ITK_DATA_ROOT}/Input/cthead1-multires.tif} ${ITK_TEST_OUTPUT_DIR}/itkTIFFImageIOMultResTest1.tif)
itk_add_test(NAME itkTIFFImageIOMultResTest2
   COMMAND ITKIOTIFFTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/cthead1.tif}
              ${ITK_TEST_OUTPUT_DIR}/itkTIFFImageIOMultResTest2.tif
    itkTIFFImageIOTest DATA{${ITK_DATA_ROOT}/Input/cthead1-multires.tif} ${ITK_TEST_OUTPUT_DIR}/itkTIFFImageIOMultResTest2.tif 3)
itk_add_test(NAME itkTIFFImageIOSpacing
   COMMAND ITKIOTIFFTestDriver
    itkTIFFImageIOTest2 ${ITK_TEST_OUTPUT_DIR}/itkTIFFImageIOSpacing.tif)
itk_add_test(NAME itkTIFFImageIOFloatTest
      COMMAND ITKIOTIFFTestDriver
    --compare DATA{Baseline/rampFloat.tif}
              ${ITK_TEST_OUTPUT_DIR}/rampFloat.tif
    itkTIFFImageIOTest DATA{Baseline/rampFloat.tif} ${ITK_TEST_OUTPUT_DIR}/rampFloat.tif 3 4)
  itk_add_test(NAME itkTIFFImageIORGBATest
      COMMAND ITKIOTIFFTestDriver
      --compare DATA{Baseline/itkTIFFImageIORGBATestBaseline.tif}
              ${ITK_TEST_OUTPUT_DIR}/itkTIFFImageIORGBATest.tif
    itkTIFFImageIOTest DATA{Input/RGBAHead.tif} ${ITK_TEST_OUTPUT_DIR}/itkTIFFImageIORGBATest.tif 3 2)

itk_add_test(NAME itkTIFFImageIOOrientation1
      COMMAND ITKIOTIFFTestDriver
      --compare  DATA{${ITK_DATA_ROOT}/Baseline/IO/cthead1.tif}
              ${ITK_TEST_OUTPUT_DIR}/itkTIFFImageIOOrientation1.tif
    itkTIFFImageIOTest DATA{Input/cthead_oriet_tl.tif} ${ITK_TEST_OUTPUT_DIR}/itkTIFFImageIOOrientation1.tif 2 1)
itk_add_test(NAME itkTIFFImageIOOrientation2
      COMMAND ITKIOTIFFTestDriver
      --compare  DATA{${ITK_DATA_ROOT}/Baseline/IO/cthead1.tif}
              ${ITK_TEST_OUTPUT_DIR}/itkTIFFImageIOOrientation2.tif
    itkTIFFImageIOTest DATA{Input/cthead_oriet_bl.tif} ${ITK_TEST_OUTPUT_DIR}/itkTIFFImageIOOrientation2.tif 2 1)
itk_add_test(NAME itkTIFFImageIOOrientation3
      COMMAND ITKIOTIFFTestDriver
      --compare  DATA{${ITK_DATA_ROOT}/Baseline/IO/cthead1.tif}
              ${ITK_TEST_OUTPUT_DIR}/itkTIFFImageIOOrientation3.tif
    itkTIFFImageIOTest DATA{Input/cthead_oriet_br.tif} ${ITK_TEST_OUTPUT_DIR}/itkTIFFImageIOOrientation3.tif 2 1)
itk_add_test(NAME itkTIFFImageIOOrientation4
      COMMAND ITKIOTIFFTestDriver
      --compare  DATA{${ITK_DATA_ROOT}/Baseline/IO/cthead1.tif}
              ${ITK_TEST_OUTPUT_DIR}/itkTIFFImageIOOrientation4.tif
    itkTIFFImageIOTest DATA{Input/cthead_oriet_tr.tif} ${ITK_TEST_OUTPUT_DIR}/itkTIFFImageIOOrientation4.tif 2 1)
itk_add_test(NAME itkTIFFImageIOPlannarConfig1
      COMMAND ITKIOTIFFTestDriver
      --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkTIFFImageIOPlannarConfig1.tif
              e3db5c8b5ddfb17dc708330c66a22659
    itkTIFFImageIOTest DATA{Input/oxford.tif} ${ITK_TEST_OUTPUT_DIR}/itkTIFFImageIOPlannarConfig1.tif 2 1)
itk_add_test(NAME itkTIFFImageIOPlannarConfig2
      COMMAND ITKIOTIFFTestDriver
      --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkTIFFImageIOPlannarConfig2.tif
              91975481ba66709c8f3ccb538af5cd2a
    itkTIFFImageIOTest DATA{Input/ps-separated.tif} ${ITK_TEST_OUTPUT_DIR}/itkTIFFImageIOPlannarConfig2.tif 2 1)

itk_add_test(NAME itkTIFFImageIOInfoTest1
      COMMAND ITKIOTIFFTestDriver
      itkTIFFImageIOInfoTest DATA{Input/single-channel.ome.tif})
set_tests_properties( itkTIFFImageIOInfoTest1
    PROPERTIES PASS_REGULAR_EXPRESSION "OME-XML metadata block")
itk_add_test(NAME itkTIFFImageIOInfoTest2
      COMMAND ITKIOTIFFTestDriver
      itkTIFFImageIOInfoTest DATA{Input/ps-separated.tif})
set_tests_properties( itkTIFFImageIOInfoTest2
    PROPERTIES PASS_REGULAR_EXPRESSION "2014:09:24 14:16:01")


######################
# Test Compression

set (Compression "NoCompression;LZW;JPEG;PackBits")
set (CompressionFiles "cthead1;RGBTestImageJPEG")
foreach(f ${CompressionFiles})
  foreach(c ${Compression})
    set (file TestTIFFCompression_${f}_${c}.tif})
    itk_add_test(NAME itkTIFFImageIOCompression_${f}_${c}
          COMMAND ITKIOTIFFTestDriver
          --compare DATA{Baseline/TestTIFFCompression_${f}_${c}.tif}
              ${ITK_TEST_OUTPUT_DIR}/TestTIFFCompression_${f}_${c}.tif
        itkTIFFImageIOCompressionTest DATA{${ITK_DATA_ROOT}/Input/${f}.tif} ${ITK_TEST_OUTPUT_DIR}/TestTIFFCompression_${f}_${c}.tif ${c})
      endforeach()
endforeach()

set (Compression "JPEG")
set (CompressionFiles "cthead1")
set (Quality "1;50;100")
foreach(f ${CompressionFiles})
  foreach(q ${Quality})
    set (file TestTIFFCompression_${f}_JPEG_${q}.tif})
    itk_add_test(NAME itkTIFFImageIOCompression_${f}_JPEG_${q}
          COMMAND ITKIOTIFFTestDriver
          --compare DATA{Baseline/TestTIFFCompression_${f}_JPEG_${q}.tif}
              ${ITK_TEST_OUTPUT_DIR}/TestTIFFCompression_${f}_JPEG_${q}.tif
        itkTIFFImageIOCompressionTest DATA{${ITK_DATA_ROOT}/Input/${f}.tif} ${ITK_TEST_OUTPUT_DIR}/TestTIFFCompression_${f}_JPEG_${q}.tif JPEG ${q})
      endforeach()
endforeach()

######################
if( "${ITK_COMPUTER_MEMORY_SIZE}" GREATER 5 )

  # Image of 1.67 Gigabytes (pixels size is 16bits)
  itk_add_test(NAME itkLargeTIFFImageWriteReadTest1
      COMMAND ITKIOTIFFTestDriver
    itkLargeTIFFImageWriteReadTest ${ITK_TEST_OUTPUT_DIR}/LargeImage01.tif  30000L )

  # Image of 2.98 Gigabytes (pixels size is 16bits)
  itk_add_test(NAME itkLargeTIFFImageWriteReadTest2
      COMMAND ITKIOTIFFTestDriver
    itkLargeTIFFImageWriteReadTest ${ITK_TEST_OUTPUT_DIR}/LargeImage02.tif  40000L )

  # Image of 4.9 Gigabytes (pixels size is 8-bits out and 16-bit in)
  itk_add_test(NAME itkLargeTIFFImageWriteReadTest3
      COMMAND ITKIOTIFFTestDriver
    itkLargeTIFFImageWriteReadTest ${ITK_TEST_OUTPUT_DIR}/LargeImage03.tif  50000L )

  # Due to the large memory requirements this tests must be run one by one
  set_tests_properties(itkLargeTIFFImageWriteReadTest1
                       itkLargeTIFFImageWriteReadTest2
                       itkLargeTIFFImageWriteReadTest3
                       PROPERTIES
                       LABELS BigIO
                       RESOURCE_LOCK MEMORY_SIZE
                      )
  set_property(TEST itkLargeTIFFImageWriteReadTest1 APPEND PROPERTY LABELS RUNS_LONG)
  set_property(TEST itkLargeTIFFImageWriteReadTest2 APPEND PROPERTY LABELS RUNS_LONG)
  set_property(TEST itkLargeTIFFImageWriteReadTest3 APPEND PROPERTY LABELS RUNS_LONG)
endif()


if( "${ITK_COMPUTER_MEMORY_SIZE}" GREATER 12 )

  # Image of 9.12 Gigabytes (pixels size is 16bits)
  itk_add_test(NAME itkLargeTIFFImageWriteReadTest4
      COMMAND ITKIOTIFFTestDriver
    itkLargeTIFFImageWriteReadTest ${ITK_TEST_OUTPUT_DIR}/LargeImage04.tif  70000L )

  # Due to the large memory requirements this tests must lock the memory size resource
  set_tests_properties(itkLargeTIFFImageWriteReadTest4
    PROPERTIES
    LABELS BigIO
    RESOURCE_LOCK MEMORY_SIZE
    )
  set_property(TEST itkLargeTIFFImageWriteReadTest4 APPEND PROPERTY LABELS RUNS_LONG)

endif()


itk_add_test(NAME itkTIFFImageIOTestPaletteExpanded
      COMMAND ITKIOTIFFTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkTIFFImageIOTestPaletteExpanded.tif
              1e1a89a70b7cb472f55c450909df7b77
    itkTIFFImageIOTestPalette DATA{Input/HeliconiusNumataPalette.tif} ${ITK_TEST_OUTPUT_DIR}/itkTIFFImageIOTestPaletteExpanded.tif 1 0)
itk_add_test(NAME itkTIFFImageIOTestPaletteNotExpanded
      COMMAND ITKIOTIFFTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkTIFFImageIOTestPaletteNotExpanded.tif
              4a4133ec26e5c83a5cbd9188067b1633
    itkTIFFImageIOTestPalette DATA{Input/HeliconiusNumataPalette.tif} ${ITK_TEST_OUTPUT_DIR}/itkTIFFImageIOTestPaletteNotExpanded.tif 0 1)
itk_add_test(NAME itkTIFFImageIOTestPaletteNotExpandedGrey
      COMMAND ITKIOTIFFTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkTIFFImageIOTestPaletteNotExpandedGrey.tif
              4a4133ec26e5c83a5cbd9188067b1633
    itkTIFFImageIOTestPalette DATA{Input/HeliconiusNumataPalette.tif} ${ITK_TEST_OUTPUT_DIR}/itkTIFFImageIOTestPaletteNotExpandedGrey.tif 0 0)
