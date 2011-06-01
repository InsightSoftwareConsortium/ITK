itk_module_test()
set(Video-Filters-CommonTests
  itkFrameAverageVideoFilterTest.cxx
  itkDecimateFramesVideoFilterTest.cxx
  itkImageFilterToVideoFilterWrapperTest.cxx
)

CreateTestDriver(Video-Filters-Common  "${Video-Filters-Common-Test_LIBRARIES}" "${Video-Filters-CommonTests}")

# FrameAverageVideoFilter
add_video_test(NAME FrameAverageVideoFilterTest
              COMMAND Video-Filters-CommonTestDriver itkFrameAverageVideoFilterTest
              # Input
              "MIDAS{frame0.jpg.md5},MIDAS{frame1.jpg.md5},MIDAS{frame2.jpg.md5},MIDAS{frame3.jpg.md5},MIDAS{frame4.jpg.md5}"
              # Output
              "${ITK_TEST_OUTPUT_DIR}/avg_frame0.png,${ITK_TEST_OUTPUT_DIR}/avg_frame1.png,${ITK_TEST_OUTPUT_DIR}/avg_frame2.png"
              )

# DecimateFramesVideoFilter
add_video_test(NAME DecimateFramesVideoFilterTest
              COMMAND Video-Filters-CommonTestDriver itkDecimateFramesVideoFilterTest
              # Input
              "MIDAS{frame0.jpg.md5},MIDAS{frame1.jpg.md5},MIDAS{frame2.jpg.md5},MIDAS{frame3.jpg.md5},MIDAS{frame4.jpg.md5}"
              # Output
              "${ITK_TEST_OUTPUT_DIR}/dec_frame0.png,${ITK_TEST_OUTPUT_DIR}/dec_frame1.png"
              )

# ImageFilterToVideoFilterWrapperTest
add_video_test(NAME ImageFilterToVideoFilterWrapperTest
              COMMAND Video-Filters-CommonTestDriver itkImageFilterToVideoFilterWrapperTest
              # Input
              "MIDAS{frame0.jpg.md5},MIDAS{frame1.jpg.md5},MIDAS{frame2.jpg.md5},MIDAS{frame3.jpg.md5},MIDAS{frame4.jpg.md5}"
              # Output
              "${ITK_TEST_OUTPUT_DIR}/wrapper_frame0.png,${ITK_TEST_OUTPUT_DIR}/wrapper_frame1.png,${ITK_TEST_OUTPUT_DIR}/wrapper_frame2.png,${ITK_TEST_OUTPUT_DIR}/wrapper_frame3.png,${ITK_TEST_OUTPUT_DIR}/wrapper_frame4.png"
              )
