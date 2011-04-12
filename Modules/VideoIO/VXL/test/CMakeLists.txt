itk_module_test()
set(Video-IO-VXLTests
  itkVXLVideoIOTest.cxx
  itkVXLVideoIOFactoryTest.cxx
)

# include VXL headers
find_package ( VXL REQUIRED )
include( ${VXL_CMAKE_DIR}/UseVXL.cmake )

CreateTestDriver(Video-IO-VXL "${Video-IO-VXL-Test_LIBRARIES}" "${Video-IO-VXLTests}")

#DEBUG
set(ITKVideo_Data_DIR /home/gabe/Projects/itk4/Video/ITKVideo/Video/Data)


# VXLVideoIO:
add_test(NAME VXLVideoIOTest
         COMMAND Video-IO-VXLTestDriver itkVXLVideoIOTest
         # Video Input:
         ${ITKVideo_Data_DIR}/Input/inde-circulation.avi
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
add_test(NAME VXLVideoIOFactoryTest
         COMMAND Video-IO-VXLTestDriver itkVXLVideoIOFactoryTest
         # Video Input:
         ${ITKVideo_Data_DIR}/Input/inde-circulation.avi
         # Output:
         ${ITK_TEST_OUTPUT_DIR}/VXLVideoIOTest_Out.avi
         # Webcam Number:
         0
         )

