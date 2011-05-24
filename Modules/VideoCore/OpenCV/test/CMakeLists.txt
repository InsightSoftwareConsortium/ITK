itk_module_test()
set(Video-Core-OpenCVTests
  itkOpenCVVideoCaptureTest.cxx
)
find_package ( OpenCV REQUIRED )
CreateTestDriver(Video-Core-OpenCV  "${Video-Core-OpenCV-Test_LIBRARIES}" "${Video-Core-OpenCVTests}")

include_directories(${Video-IO-OpenCV_INCLUDE_DIRS})

# OpenCVVideoCapture:
midas_add_test(NAME OpenCVVideoCaptureTest
         COMMAND Video-Core-OpenCVTestDriver itkOpenCVVideoCaptureTest
         # Video Input:
         MIDAS{inde-circulation.avi.md5}
         # Scalar Output:
         ${ITK_TEST_OUTPUT_DIR}/OpenCVVideoCaptureTest_ScalarOut.avi
         # RGB Output:
         ${ITK_TEST_OUTPUT_DIR}/OpenCVVideoCaptureTest_RGBOut.avi
         # Width Height
         320 240
         )
