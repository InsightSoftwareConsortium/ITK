itk_module_test()
set(ITKVideoIOTests
  itkVideoFileReaderWriterTest.cxx
  itkFileListVideoIOTest.cxx
  itkFileListVideoIOFactoryTest.cxx
)

CreateTestDriver(ITKVideoIO
  "${ITKVideoIO-Test_LIBRARIES}"
  "${ITKVideoIOTests}"
)

# VideoFileRederWriterTest:
itk_add_test(
  NAME VideoFileReaderWriterTest
  COMMAND ITKVideoIOTestDriver
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
  COMMAND ITKVideoIOTestDriver
    itkFileListVideoIOTest
      DATA{Input/frame0.jpg}
      DATA{Input/frame1.jpg}
      DATA{Input/frame2.jpg}
      DATA{Input/frame3.jpg}
      DATA{Input/frame4.jpg}
      ${ITKVideo_Data_DIR}/Input/TestParameters.txt
      "${ITK_TEST_OUTPUT_DIR}/filelistvideoio_frame0.png,${ITK_TEST_OUTPUT_DIR}/filelistvideoio_frame1.png,${ITK_TEST_OUTPUT_DIR}/filelistvideoio_frame2.png,${ITK_TEST_OUTPUT_DIR}/filelistvideoio_frame3.png,${ITK_TEST_OUTPUT_DIR}/filelistvideoio_frame4.png"
      ${ITK_TEST_OUTPUT_DIR}/FileListVideoIOTest_CameraOut.mha
      320 240 5 1)

# FileListVideoIOFactory:
itk_add_test(
  NAME FileListVideoIOFactoryTest
  COMMAND ITKVideoIOTestDriver
    itkFileListVideoIOFactoryTest
    DATA{Input/frame0.jpg}
    DATA{Input/frame1.jpg}
    DATA{Input/frame2.jpg}
    DATA{Input/frame3.jpg}
    DATA{Input/frame4.jpg}
    "${ITK_TEST_OUTPUT_DIR}/filelistfactory_frame0.png,${ITK_TEST_OUTPUT_DIR}/filelistfactory_frame1.png,${ITK_TEST_OUTPUT_DIR}/filelistfactory_frame2.png,${ITK_TEST_OUTPUT_DIR}/filelistfactory_frame3.png,${ITK_TEST_OUTPUT_DIR}/filelistfactory_frame4.png"
    0)
