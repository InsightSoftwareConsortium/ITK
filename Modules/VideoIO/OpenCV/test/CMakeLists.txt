itk_module_test()
set(Video-IO-OpenCVTests
  itkOpenCVVideoIOTest.cxx
  itkOpenCVVideoIOFactoryTest.cxx
)
find_package ( OpenCV REQUIRED )
CreateTestDriver(Video-IO-OpenCV  "${Video-IO-OpenCV-Test_LIBRARIES}" "${Video-IO-OpenCVTests}")

# OpenCVVideoIO:
midas_add_test(NAME OpenCVVideoIOTest
         COMMAND Video-IO-OpenCVTestDriver itkOpenCVVideoIOTest
         # Video Input:
         MIDAS{inde-circulation.avi.md5}
         # Non-Video Input:
         ${ITKVideo_Data_DIR}/Input/TestParameters.txt
         # Output:
         ${ITK_TEST_OUTPUT_DIR}/OpenCVVideoIOTest_Out.avi
         # Webcam Output:
         ${ITK_TEST_OUTPUT_DIR}/OpenCVVideoIOTest_CameraOut.mha
         # Height Width NumFrames FpS
         320 240 1396 14.985
         )

# OpenCVVideoIOFactory:
midas_add_test(NAME OpenCVVideoIOFactoryTest
         COMMAND Video-IO-OpenCVTestDriver itkOpenCVVideoIOFactoryTest
         # Video Input:
         MIDAS{inde-circulation.avi.md5}
         # Output:
         ${ITK_TEST_OUTPUT_DIR}/OpenCVVideoIOTest_Out.avi
         # Webcam Number:
         0
         )
