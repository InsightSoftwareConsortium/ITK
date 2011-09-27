itk_module_test()
set(ITKIOVideoTests
  itkVideoFileReaderWriterTest.cxx
  itkFileListVideoIOTest.cxx
  itkFileListVideoIOFactoryTest.cxx
)

CreateTestDriver(ITKIOVideo "${ITKIOVideo-Test_LIBRARIES}" "${ITKIOVideoTests}")

# VideoFileRederWriterTest:
itk_add_test(
  NAME VideoFileReaderWriterTest
  COMMAND ITKIOVideoTestDriver
    itkVideoFileReaderWriterTest
      DATA{Input/frame0.jpg}
      DATA{Input/frame1.jpg}
      DATA{Input/frame2.jpg}
      DATA{Input/frame3.jpg}
      DATA{Input/frame4.jpg}
      "${ITK_TEST_OUTPUT_DIR}/frame0.png,${ITK_TEST_OUTPUT_DIR}/frame1.png,${ITK_TEST_OUTPUT_DIR}/frame2.png,${ITK_TEST_OUTPUT_DIR}/frame3.png,${ITK_TEST_OUTPUT_DIR}/frame4.png"
    )


# FileListVideoIO:
itk_add_test(
  NAME FileListVideoIOTest
  COMMAND ITKIOVideoTestDriver
    itkFileListVideoIOTest
      DATA{Input/frame0.jpg}
      DATA{Input/frame1.jpg}
      DATA{Input/frame2.jpg}
      DATA{Input/frame3.jpg}
      DATA{Input/frame4.jpg}
      ${ITKVideo_Data_DIR}/Input/TestParameters.txt
      "${ITK_TEST_OUTPUT_DIR}/frame0.png,${ITK_TEST_OUTPUT_DIR}/frame1.png,${ITK_TEST_OUTPUT_DIR}/frame2.png,${ITK_TEST_OUTPUT_DIR}/frame3.png,${ITK_TEST_OUTPUT_DIR}/frame4.png"
      ${ITK_TEST_OUTPUT_DIR}/FileListVideoIOTest_CameraOut.mha
      320 240 5 1)

# FileListVideoIOFactory:
itk_add_test(
  NAME FileListVideoIOFactoryTest
  COMMAND ITKIOVideoTestDriver
    itkFileListVideoIOFactoryTest
    DATA{Input/frame0.jpg}
    DATA{Input/frame1.jpg}
    DATA{Input/frame2.jpg}
    DATA{Input/frame3.jpg}
    DATA{Input/frame4.jpg}
    "${ITK_TEST_OUTPUT_DIR}/frame0.png,${ITK_TEST_OUTPUT_DIR}/frame1.png,${ITK_TEST_OUTPUT_DIR}/frame2.png,${ITK_TEST_OUTPUT_DIR}/frame3.png,${ITK_TEST_OUTPUT_DIR}/frame4.png"
    0)
