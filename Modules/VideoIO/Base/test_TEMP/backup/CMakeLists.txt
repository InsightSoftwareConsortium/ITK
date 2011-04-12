
CreateTestDriver(Video-IO-Base  "${Video-IO-Base-Test_LIBRARIES}" "${Video-IO-BaseTests}")


#Face detection :
add_test(NAME ITKVideoTestMotion
         COMMAND Video-IO-BaseTestDriver itkMotionTrackerTest
         #Input :
         ${ITKVideo_Data_DIR}/inde-circulation.avi
         #Output :
         ${CMAKE_CURRENT_BINARY_DIR}/Testing/Temporary/Motion_tracked_inde-circulation.avi
         #Reader use OpenCV :
         1
         #Writer use OpenCV :
         1
         )


# Ignore everything else for now
if (FALSE)

set(itkVideoFilePipelineTest_SRCS ${ITKVideo_IO_DIR}/itkVideoFilePipelineTest.cxx )
set(itkVMotionTracker_SRCS ${ITKVideo_IO_DIR}/itkMotionTrackerTest.cxx )
set(itkVTransformDICOMToVideo_SRCS ${ITKVideo_IO_DIR}/itkTransformDICOMToVideo.cxx )
set(itkVideoFileReaderExample_SRCS ${ITKVideo_IO_DIR}/itkVideoFileReaderExample.cxx )
set(itkVideoViewerExample_SRCS ${ITKVideo_IO_DIR}/itkVideoViewerExample.cxx )

add_executable(itkVideoFilePipelineTest ${itkVideoFilePipelineTest_SRCS})
target_link_libraries(itkVideoFilePipelineTest ${ITKVideo_Lib})

add_executable(itkMotionTrackerTest ${itkVMotionTracker_SRCS})
target_link_libraries(itkMotionTrackerTest ${ITKVideo_Lib}) 

add_executable(itkTransformDICOMToVideo ${itkVTransformDICOMToVideo_SRCS})
target_link_libraries(itkTransformDICOMToVideo ${ITKVideo_Lib})

add_executable(itkVideoFileReaderExample ${itkVideoFileReaderExample_SRCS})
target_link_libraries(itkVideoFileReaderExample ${ITKVideo_Lib})

add_executable(itkVideoViewerExample ${itkVideoViewerExample_SRCS})
target_link_libraries(itkVideoViewerExample ${ITKVideo_Lib})   

#Motion tracker :
 add_test (ITKVideoTestMotion itkMotionTrackerTest 
 #Input :
  ${ITKVideo_Data_DIR}/inde-circulation.avi
  #Output :
  ${CMAKE_CURRENT_BINARY_DIR}/Testing/Temporary/Motion_tracked_inde-circulation.avi
 #Reader use OpenCV :
  1
  #Writer use OpenCV :
  1 
  )
 
#Reader : 
 #with opencv
 add_test(ITKVideoTestReaderOpenCV itkVideoFileReaderExample
    #Input :
    ${ITKVideo_Data_DIR}/inde-circulation.avi 
    #Output whithout extension :
   ${CMAKE_CURRENT_BINARY_DIR}/Testing/Temporary/OpenCVFrame_Number_ 
    #Reader Use openCV :
  1)
  #with VXL
 add_test(ITKVideoTestReaderVXL itkVideoFileReaderExample 
    #Input :
    ${ITKVideo_Data_DIR}/inde-circulation.avi 
    #Output whithout extension :
    ${CMAKE_CURRENT_BINARY_DIR}/Testing/Temporary/VXLFrame_Number_ 
   #Reader Use OpenCV
    0)
 
#Pipeline :

  #with reader && writer opencv
add_test(ITKVideoTestPipelineReadOpenCVWriteOpenCV itkVideoFilePipelineTest
    #Input :
    ${ITKVideo_Data_DIR}/inde-circulation.avi
   #Output :
    ${CMAKE_CURRENT_BINARY_DIR}/Testing/Temporary/inde_circulation_ReadOpenCV_WriteOpenCV.avi 
    #Reader Use OpenCV
    1
    #Writer Use OpenCV
    1)  
    
  #with opencv reader and VXL writer
add_test(ITKVideoTestPipelineReadOpenCVWriteVXL itkVideoFilePipelineTest 
    #Input :
    ${ITKVideo_Data_DIR}/inde-circulation.avi 
    #Output :
    ${CMAKE_CURRENT_BINARY_DIR}/Testing/Temporary/inde_circulation_ReadOpenCV_WriteVXL.avi 
    #Reader Use OpenCV
    1
    #Writer Use OpenCV
    0)
    
  #with VXL reader and OpenCV writer
add_test(ITKVideoTestPipelineReadVXLWriteVXL itkVideoFilePipelineTest 
    #Input
    ${ITKVideo_Data_DIR}/inde-circulation.avi 
    #Output
    ${CMAKE_CURRENT_BINARY_DIR}/Testing/Temporary/inde_circulation_ReadVXL_WriteVXL.avi 
    #Reader Use OpenCV
    0 
   #Writer Use OpenCV
    0)
    
  #with VXL reader and writer
add_test(ITKVideoTestPipelineReadVXLWriteOpenCV itkVideoFilePipelineTest
    #Input
    ${ITKVideo_Data_DIR}/inde-circulation.avi 
    #Output
    ${CMAKE_CURRENT_BINARY_DIR}/Testing/Temporary/inde_circulation_ReadVXL_WriteOpenCV.avi
    #Reader Use OpenCV
    0
    #Writer Use OpenCV
    1)  

#Writer :
  #with openCV
add_test(ITKVideoTestWriterOpenCV itkTransformDICOMToVideo
    #Input whithout extension
    ${ITKVideo_Data_DIR}/CTHeadAxialDicom/CTHead
    #Output
    ${CMAKE_CURRENT_BINARY_DIR}/Testing/Temporary/DicomImageAsAVideoWithOpenCV.avi
    #Writer Use OpenCV
    1
    #First Image
    1
    #Last Image
    93)
  #with VXL
add_test(ITKVideoTestWriterVXL itkTransformDICOMToVideo
    #Input whithout extension
    ${ITKVideo_Data_DIR}/CTHeadAxialDicom/CTHead
    #Output
    ${CMAKE_CURRENT_BINARY_DIR}/Testing/Temporary/DicomImageAsAVideoWithVXL.avi 
    #Writer Use OpenCV
    0
    #First Image
    1
    #Second Image
    93)


endif (FALSE)
