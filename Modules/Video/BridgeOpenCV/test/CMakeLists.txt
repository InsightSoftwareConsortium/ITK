itk_module_test()
set(ITKVideoBridgeOpenCVTests
  itkOpenCVVideoCaptureTest.cxx
  itkOpenCVImageBridgeGrayScaleTest.cxx
  itkOpenCVImageBridgeRGBTest.cxx
  itkOpenCVVideoIOTest.cxx
  itkOpenCVVideoIOFactoryTest.cxx
  itkOpenCVBasicTypeBridgeTest.cxx
)

include_directories(${ITKVideoBridgeOpenCV_INCLUDE_DIRS})

CreateTestDriver(ITKVideoBridgeOpenCV
  "${ITKVideoBridgeOpenCV-Test_LIBRARIES};${OpenCV_LIBS}"
  "${ITKVideoBridgeOpenCVTests}")

# OpenCVVideoCapture:
itk_add_test(NAME
  OpenCVVideoCaptureTest
  COMMAND ITKVideoBridgeOpenCVTestDriver
    itkOpenCVVideoCaptureTest
      # Video Input:
      DATA{Input/inde-circulation_short.avi}
      # Scalar Output:
      ${ITK_TEST_OUTPUT_DIR}/OpenCVVideoCaptureTest_ScalarOut.avi
      # RGB Output:
      ${ITK_TEST_OUTPUT_DIR}/OpenCVVideoCaptureTest_RGBOut.avi
      # Width Height
      320 240
    )

# OpenCVImageBridge:
#   NOTE: For some reason, ITK reads jpg files slightly differently than
#   OpenCV, so we need both jpg and mha versions to guarantee consistency
itk_add_test(
  NAME OpenCVImageBridgeGrayScaleTest
  COMMAND ITKVideoBridgeOpenCVTestDriver
    itkOpenCVImageBridgeGrayScaleTest
      # Input Grayscale Image jpg
      DATA{Input/frame0_gray.jpg}
      # Input Grayscale Image jpg 2, with odd pixel dimension
      DATA{Input/peppers_513x512.jpg}
      # Input Grayscale Image png, with two-byte pixels
      DATA{Input/IM11.png}
    )

itk_add_test(
  NAME OpenCVImageBridgeRGBTest
  COMMAND ITKVideoBridgeOpenCVTestDriver
    itkOpenCVImageBridgeRGBTest
      # Input RGB Image jpg:
      DATA{Input/frame0.jpg}
      # Input RGB Image mha:
      DATA{Input/frame0.mha}
      # Input RGB Image 513x512
      DATA{Input/peppers_513x512_rgb.png}
    )
# OpenCVVideoIO:
itk_add_test(
  NAME OpenCVVideoIOTest
  COMMAND ITKVideoBridgeOpenCVTestDriver
    itkOpenCVVideoIOTest
      # Video Input:
      DATA{Input/inde-circulation_short.avi}
      # Non-Video Input:
      foo.txt
      # Output:
      ${ITK_TEST_OUTPUT_DIR}/OpenCVVideoIOTest_Out.avi
      # Webcam Output:
      ${ITK_TEST_OUTPUT_DIR}/OpenCVVideoIOTest_CameraOut.mha
      # Width Height NumFrames FpS
      320 240 30 14.985
    )

# OpenCVVideoIOFactory:
itk_add_test(
  NAME OpenCVVideoIOFactoryTest
  COMMAND ITKVideoBridgeOpenCVTestDriver
    itkOpenCVVideoIOFactoryTest
      # Video Input:
      DATA{Input/inde-circulation_short.avi}
      # Output:
      ${ITK_TEST_OUTPUT_DIR}/OpenCVVideoIOTest_Out.avi
      # Webcam Number:
      0
    )

# OpenCVBasicTypeBridgeTest
itk_add_test(
  NAME OpenCVBasicTypeBridgeTest
  COMMAND ITKVideoBridgeOpenCVTestDriver
    itkOpenCVBasicTypeBridgeTest
)
