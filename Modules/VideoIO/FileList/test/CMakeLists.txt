itk_module_test()
set(Video-IO-FileListTests
  itkFileListVideoIOTest.cxx
  itkFileListVideoIOFactoryTest.cxx
)
CreateTestDriver(Video-IO-FileList  "${Video-IO-FileList-Test_LIBRARIES}" "${Video-IO-FileListTests}")

# For Midas-stored testing data
include(${Video-Common_SOURCE_DIR}/MIDAS.cmake)

# FileListVideoIO:
midas_add_test(NAME FileListVideoIOTest
         COMMAND Video-IO-FileListTestDriver itkFileListVideoIOTest
         # Video Input:
         "MIDAS{frame0.jpg.md5},MIDAS{frame1.jpg.md5},MIDAS{frame2.jpg.md5},MIDAS{frame3.jpg.md5},MIDAS{frame4.jpg.md5}"
         # Non-Video Input:
         ${ITKVideo_Data_DIR}/Input/TestParameters.txt
         # Output:
         "${ITK_TEST_OUTPUT_DIR}/frame0.png,${ITK_TEST_OUTPUT_DIR}/frame1.png,${ITK_TEST_OUTPUT_DIR}/frame2.png,${ITK_TEST_OUTPUT_DIR}/frame3.png,${ITK_TEST_OUTPUT_DIR}/frame4.png"
         # Webcam Output:
         ${ITK_TEST_OUTPUT_DIR}/FileListVideoIOTest_CameraOut.mha
         # Height Width NumFrames FpS
         320 240 5 1
         )

# FileListVideoIOFactory:
midas_add_test(NAME FileListVideoIOFactoryTest
         COMMAND Video-IO-FileListTestDriver itkFileListVideoIOFactoryTest
         # Video Input:
         "MIDAS{frame0.jpg.md5},MIDAS{frame1.jpg.md5},MIDAS{frame2.jpg.md5},MIDAS{frame3.jpg.md5},MIDAS{frame4.jpg.md5}"
         # Output:
         "${ITK_TEST_OUTPUT_DIR}/frame0.png,${ITK_TEST_OUTPUT_DIR}/frame1.png,${ITK_TEST_OUTPUT_DIR}/frame2.png,${ITK_TEST_OUTPUT_DIR}/frame3.png,${ITK_TEST_OUTPUT_DIR}/frame4.png"
         # Webcam Number:
         0
         )
