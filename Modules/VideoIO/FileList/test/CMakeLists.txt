itk_module_test()
set(Video-IO-FileListTests
  itkFileListVideoIOTest.cxx
  itkFileListVideoIOFactoryTest.cxx
)
CreateTestDriver(Video-IO-FileList  "${Video-IO-FileList-Test_LIBRARIES}" "${Video-IO-FileListTests}")

#DEBUG
set(ITKVideo_Data_DIR /home/gabe/Projects/itk4/Video/ITKVideo/Video/Data)


# FileListVideoIO:
add_test(NAME FileListVideoIOTest
         COMMAND Video-IO-FileListTestDriver itkFileListVideoIOTest
         # Video Input:
         "${ITKVideo_Data_DIR}/Input/FileList/frame0.jpg\;${ITKVideo_Data_DIR}/Input/FileList/frame1.jpg\;${ITKVideo_Data_DIR}/Input/FileList/frame2.jpg\;${ITKVideo_Data_DIR}/Input/FileList/frame3.jpg\;${ITKVideo_Data_DIR}/Input/FileList/frame4.jpg"
         # Non-Video Input:
         ${ITKVideo_Data_DIR}/Input/TestParameters.txt
         # Output:
         "${ITK_TEST_OUTPUT_DIR}/frame0.png\;${ITK_TEST_OUTPUT_DIR}/frame1.png\;${ITK_TEST_OUTPUT_DIR}/frame2.png\;${ITK_TEST_OUTPUT_DIR}/frame3.png\;${ITK_TEST_OUTPUT_DIR}/frame4.png"
         # Webcam Output:
         ${ITK_TEST_OUTPUT_DIR}/FileListVideoIOTest_CameraOut.mha
         # Height Width NumFrames FpS
         320 240 1396 14.985
         )

# FileListVideoIOFactory:
add_test(NAME FileListVideoIOFactoryTest
         COMMAND Video-IO-FileListTestDriver itkFileListVideoIOFactoryTest
         # Video Input:
         "${ITKVideo_Data_DIR}/Input/FileList/frame0.jpg\;${ITKVideo_Data_DIR}/Input/FileList/frame1.jpg\;${ITKVideo_Data_DIR}/Input/FileList/frame2.jpg\;${ITKVideo_Data_DIR}/Input/FileList/frame3.jpg\;${ITKVideo_Data_DIR}/Input/FileList/frame4.jpg"
         # Output:
         "${ITK_TEST_OUTPUT_DIR}/frame0.png\;${ITK_TEST_OUTPUT_DIR}/frame1.png\;${ITK_TEST_OUTPUT_DIR}/frame2.png\;${ITK_TEST_OUTPUT_DIR}/frame3.png\;${ITK_TEST_OUTPUT_DIR}/frame4.png"
         # Webcam Number:
         0
         )

