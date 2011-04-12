itk_module_test()
set(Video-IO-BaseTests
  itkVideoFileReaderTest.cxx
  itkVideoFileWriterTest.cxx
)
CreateTestDriver(Video-IO-Base  "${Video-IO-Base-Test_LIBRARIES}" "${Video-IO-BaseTests}")

#DEBUG
set(ITKVideo_Data_DIR /home/gabe/Projects/itk4/Video/ITKVideo/Video/Data)


# VideoFileReader:
add_test(NAME VideoFileReaderTest
         COMMAND Video-IO-BaseTestDriver itkVideoFileReaderTest
         # Input
         "${ITKVideo_Data_DIR}/Input/FileList/frame0.jpg\;${ITKVideo_Data_DIR}/Input/FileList/frame1.jpg\;${ITKVideo_Data_DIR}/Input/FileList/frame2.jpg\;${ITKVideo_Data_DIR}/Input/FileList/frame3.jpg\;${ITKVideo_Data_DIR}/Input/FileList/frame4.jpg"
         # Output
         "${ITK_TEST_OUTPUT_DIR}/frame0.png\;${ITK_TEST_OUTPUT_DIR}/frame1.png\;${ITK_TEST_OUTPUT_DIR}/frame2.png\;${ITK_TEST_OUTPUT_DIR}/frame3.png\;${ITK_TEST_OUTPUT_DIR}/frame4.png"
         )

# VideoFileWriter:
add_test(NAME VideoFileWriterTest
         COMMAND Video-IO-BaseTestDriver itkVideoFileWriterTest
         # Input
         "${ITKVideo_Data_DIR}/Input/FileList/frame0.jpg\;${ITKVideo_Data_DIR}/Input/FileList/frame1.jpg\;${ITKVideo_Data_DIR}/Input/FileList/frame2.jpg\;${ITKVideo_Data_DIR}/Input/FileList/frame3.jpg\;${ITKVideo_Data_DIR}/Input/FileList/frame4.jpg"
         # Output
         "${ITK_TEST_OUTPUT_DIR}/frame0.png\;${ITK_TEST_OUTPUT_DIR}/frame1.png\;${ITK_TEST_OUTPUT_DIR}/frame2.png\;${ITK_TEST_OUTPUT_DIR}/frame3.png\;${ITK_TEST_OUTPUT_DIR}/frame4.png"
         )
