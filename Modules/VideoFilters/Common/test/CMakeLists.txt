itk_module_test()
set(Video-Filters-CommonTests
  itkFrameAverageVideoFilterTest.cxx
)

CreateTestDriver(Video-Filters-Common  "${Video-Filters-Common-Test_LIBRARIES}" "${Video-Filters-CommonTests}")

# For Midas-stored testing data
include(${Video-Common_SOURCE_DIR}/MIDAS.cmake)

# FrameAverageVideoFilter
midas_add_test(NAME FrameAverageVideoFilterTest
        COMMAND Video-Filters-CommonTestDriver itkFrameAverageVideoFilterTest
        # Input
        "MIDAS{frame0.jpg.md5},MIDAS{frame1.jpg.md5},MIDAS{frame2.jpg.md5},MIDAS{frame3.jpg.md5},MIDAS{frame4.jpg.md5}"
        # Output
        "${ITK_TEST_OUTPUT_DIR}/avg_frame0.png,${ITK_TEST_OUTPUT_DIR}/avg_frame1.png,${ITK_TEST_OUTPUT_DIR}/avg_frame2.png"
        )
