itk_module_test()
set(Video-Filters-OpenCVTests
  itkFaceDetectionFilterTest.cxx
)
find_package ( OpenCV REQUIRED )
CreateTestDriver(Video-Filters-OpenCV  "${Video-Filters-OpenCV-Test_LIBRARIES}" "${Video-Filters-OpenCVTests}")

#Face detection :
add_test(NAME ITKVideoTestFaceDetection
         COMMAND Video-Filters-OpenCVTestDriver itkFaceDetectionFilterTest
         #Input
         ${ITKVideo_Data_DIR}/faces3.png
         #Main Output
         ${CMAKE_CURRENT_BINARY_DIR}/Testing/Temporary/FaceDetectionFilterResult.png 
         #Faces whithout extension :
         ${CMAKE_CURRENT_BINARY_DIR}/Testing/Temporary/Face_ 
         #Trainer
         ${ITKVideo_Data_DIR}/haarcascade_frontalface_alt2.xml)

