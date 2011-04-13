itk_module_test()
set(Video-IO-VXLTests
  itkVXLVideoIOTest.cxx
  itkVXLVideoIOFactoryTest.cxx
)

# include VXL headers
find_package ( VXL REQUIRED )
include( ${VXL_CMAKE_DIR}/UseVXL.cmake )

CreateTestDriver(Video-IO-VXL "${Video-IO-VXL-Test_LIBRARIES}" "${Video-IO-VXLTests}")

# VXLVideoIO:
midas_add_test(NAME VXLVideoIOTest
         COMMAND Video-IO-VXLTestDriver itkVXLVideoIOTest
         # Video Input:
         MIDAS{inde-circulation.avi.md5}
         # Non-Video Input:
         ${ITKVideo_Data_DIR}/Input/TestParameters.txt
         # Output:
         ${ITK_TEST_OUTPUT_DIR}/VXLVideoIOTest_Out.avi
         # Webcam Output:
         ${ITK_TEST_OUTPUT_DIR}/VXLVideoIOTest_CameraOut.mha
         # Height Width NumFrames FpS
         320 240 1396 14.985
         )

# VXLVideoIOFactory:
midas_add_test(NAME VXLVideoIOFactoryTest
         COMMAND Video-IO-VXLTestDriver itkVXLVideoIOFactoryTest
         # Video Input:
         MIDAS{inde-circulation.avi.md5}
         # Output:
         ${ITK_TEST_OUTPUT_DIR}/VXLVideoIOTest_Out.avi
         # Webcam Number:
         0
         )

