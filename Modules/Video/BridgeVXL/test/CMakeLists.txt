itk_module_test()
set(ITKVideoBridgeVXLTests
itkVXLVideoIOFactoryTest.cxx
itkVXLVideoIOTest.cxx
vidl_itk_istreamTest.cxx
)

include( ${VXL_CMAKE_DIR}/UseVXL.cmake )
CreateTestDriver(ITKVideoBridgeVXL
                "${ITKVideoBridgeVXL-Test_LIBRARIES};vul;vidl"
                "${ITKVideoBridgeVXLTests}" )


#Disable this test for now ( to avoid polluting the dashboard)
# due to potential VXL ffmpeg (vidl_ffmpeg_ostream) related memoery leaks.
#When deleting the ostream after writing into a video file, seg faults will happen

#itk_add_test(NAME VXLVideoIOTest
#                 COMMAND ITKVideoBridgeVXLTestDriver itkVXLVideoIOTest
#                 # Video Input:
#                 DATA{Input/inde-circulation.avi}
#                 # NonVideo Input:
#                 ${ITKVideo_Data_DIR}/Input/TestParameters.txt
#                 # Output:
#                 ${ITK_TEST_OUTPUT_DIR}/VXLVideoIOTest_Out.avi
#                 # Webcam Output:
#                 ${ITK_TEST_OUTPUT_DIR}/VXLVideoIOTest_CameraOut.mha
#                 # Width Height NumFrames FpS
#                 320 240 1396 14.985
#                 )

itk_add_test(NAME VXLVideoIOFactoryTest
                 COMMAND ITKVideoBridgeVXLTestDriver itkVXLVideoIOFactoryTest
                 # Video Input:
                 DATA{Input/inde-circulation.avi}
                 # Output:
                 ${ITK_TEST_OUTPUT_DIR}/VXLVideoIOTest_Out.avi
                 # Webcam Number:
                 0
                 )
itk_add_test(NAME vidl_itk_istreamTest
                 COMMAND ITKVideoBridgeVXLTestDriver vidl_itk_istreamTest
                 # Video Input:
                 DATA{Input/inde-circulation_short.avi}
                 # Output:
                 ${ITK_TEST_OUTPUT_DIR}/vidl_itk_istreamTest_Out.avi
                 # Width Height
                 320 240
                 )
