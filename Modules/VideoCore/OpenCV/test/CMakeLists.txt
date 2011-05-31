itk_module_test()
set(Video-Core-OpenCVTests
  itkOpenCVVideoCaptureTest.cxx
  itkOpenCVImageBridgeTest.cxx
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

# OpenCVImageBridge:
#   NOTE: For some reason, ITK reads jpg files slightly differently than
#   OpenCV, so we need both jpg and mha versions to guanrantee consistency
midas_add_test(NAME OpenCVImageBridgeTest
         COMMAND Video-Core-OpenCVTestDriver itkOpenCVImageBridgeTest
         # Input Grayscale Image jpg
         MIDAS{frame0_gray.jpg.md5}
         # Input RGB Image jpg:
         MIDAS{frame0.jpg.md5}
         # Input RGB Image mha:
         MIDAS{frame0.mha.md5}
         )
