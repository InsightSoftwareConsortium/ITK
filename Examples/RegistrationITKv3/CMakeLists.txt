project(ITKv3ImageRegistration)

add_executable(ITKv3ImageRegistration3 ImageRegistration3.cxx )
target_link_libraries(ITKv3ImageRegistration3 ${ITK_LIBRARIES})

add_executable(ITKv3ImageRegistration5 ImageRegistration5.cxx )
target_link_libraries(ITKv3ImageRegistration5 ${ITK_LIBRARIES})

add_executable(ITKv3ImageRegistration6 ImageRegistration6.cxx )
target_link_libraries(ITKv3ImageRegistration6 ${ITK_LIBRARIES})

add_executable(ITKv3ImageRegistration8 ImageRegistration8.cxx )
target_link_libraries(ITKv3ImageRegistration8 ${ITK_LIBRARIES})

add_executable(ITKv3ImageRegistration9 ImageRegistration9.cxx )
target_link_libraries(ITKv3ImageRegistration9 ${ITK_LIBRARIES})

add_executable(ITKv3ImageRegistration10 ImageRegistration10.cxx )
target_link_libraries(ITKv3ImageRegistration10 ${ITK_LIBRARIES})

add_executable(ITKv3DeformableRegistration4 DeformableRegistration4.cxx )
target_link_libraries(ITKv3DeformableRegistration4 ${ITK_LIBRARIES})

add_executable(ITKv3ImageRegistration4 ImageRegistration4.cxx )
target_link_libraries(ITKv3ImageRegistration4 ${ITK_LIBRARIES})

add_executable(ITKv3ImageRegistration7 ImageRegistration7.cxx )
target_link_libraries(ITKv3ImageRegistration7 ${ITK_LIBRARIES})

add_executable(ITKv3ImageRegistration11 ImageRegistration11.cxx )
target_link_libraries(ITKv3ImageRegistration11 ${ITK_LIBRARIES})

add_executable(ITKv3ImageRegistration12 ImageRegistration12.cxx )
target_link_libraries(ITKv3ImageRegistration12 ${ITK_LIBRARIES})

add_executable(ITKv3ImageRegistration13 ImageRegistration13.cxx )
target_link_libraries(ITKv3ImageRegistration13 ${ITK_LIBRARIES})

add_executable(ITKv3ImageRegistration14 ImageRegistration14.cxx )
target_link_libraries(ITKv3ImageRegistration14 ${ITK_LIBRARIES})

add_executable(ITKv3ImageRegistration15 ImageRegistration15.cxx )
target_link_libraries(ITKv3ImageRegistration15 ${ITK_LIBRARIES})

add_executable(ITKv3ImageRegistration16 ImageRegistration16.cxx )
target_link_libraries(ITKv3ImageRegistration16 ${ITK_LIBRARIES})

add_executable(ITKv3ImageRegistration18 ImageRegistration18.cxx )
target_link_libraries(ITKv3ImageRegistration18 ${ITK_LIBRARIES})

add_executable(ITKv3ImageRegistration19 ImageRegistration19.cxx )
target_link_libraries(ITKv3ImageRegistration19 ${ITK_LIBRARIES})

add_executable(ITKv3ImageRegistration20 ImageRegistration20.cxx )
target_link_libraries(ITKv3ImageRegistration20 ${ITK_LIBRARIES})

add_executable(ITKv3DeformableRegistration6 DeformableRegistration6.cxx )
target_link_libraries(ITKv3DeformableRegistration6 ${ITK_LIBRARIES})

add_executable(ITKv3DeformableRegistration7 DeformableRegistration7.cxx )
target_link_libraries(ITKv3DeformableRegistration7 ${ITK_LIBRARIES})

add_executable(ITKv3DeformableRegistration8 DeformableRegistration8.cxx )
target_link_libraries(ITKv3DeformableRegistration8 ${ITK_LIBRARIES})

if( ITK_USE_FFTWD )
  add_executable(ITKv3DeformableRegistration9 DeformableRegistration9.cxx )
  target_link_libraries(ITKv3DeformableRegistration9 ${ITK_LIBRARIES})

  add_executable(ITKv3DeformableRegistration10 DeformableRegistration10.cxx )
  target_link_libraries(ITKv3DeformableRegistration10 ${ITK_LIBRARIES})
endif()

add_executable(ITKv3MultiResImageRegistration1 MultiResImageRegistration1.cxx )
target_link_libraries(ITKv3MultiResImageRegistration1 ${ITK_LIBRARIES})

add_executable(ITKv3MultiResImageRegistration2 MultiResImageRegistration2.cxx )
target_link_libraries(ITKv3MultiResImageRegistration2 ${ITK_LIBRARIES})

add_executable(ITKv3MultiResImageRegistration3 MultiResImageRegistration3.cxx )
target_link_libraries(ITKv3MultiResImageRegistration3 ${ITK_LIBRARIES})

add_executable(ITKv3DeformableRegistration1 DeformableRegistration1.cxx )
target_link_libraries(ITKv3DeformableRegistration1 ${ITK_LIBRARIES})

add_executable(ITKv3DeformableRegistration11 DeformableRegistration11.cxx )
target_link_libraries(ITKv3DeformableRegistration11 ${ITK_LIBRARIES})

add_executable(ITKv3DeformableRegistration2 DeformableRegistration2.cxx )
target_link_libraries(ITKv3DeformableRegistration2 ${ITK_LIBRARIES})

add_executable(ITKv3DeformableRegistration3 DeformableRegistration3.cxx )
target_link_libraries(ITKv3DeformableRegistration3 ${ITK_LIBRARIES})

add_executable(ITKv3DeformableRegistration5 DeformableRegistration5.cxx )
target_link_libraries(ITKv3DeformableRegistration5 ${ITK_LIBRARIES})

add_executable(ITKv3DeformableRegistration12 DeformableRegistration12.cxx )
target_link_libraries(ITKv3DeformableRegistration12 ${ITK_LIBRARIES})

add_executable(ITKv3DeformableRegistration13 DeformableRegistration13.cxx )
target_link_libraries(ITKv3DeformableRegistration13 ${ITK_LIBRARIES})

add_executable(ITKv3DeformableRegistration14 DeformableRegistration14.cxx )
target_link_libraries(ITKv3DeformableRegistration14 ${ITK_LIBRARIES})

add_executable(ITKv3ChangeInformationImageFilter ChangeInformationImageFilter.cxx )
target_link_libraries(ITKv3ChangeInformationImageFilter ${ITK_LIBRARIES})

add_executable(ITKv3DeformableRegistration15 DeformableRegistration15.cxx )
target_link_libraries(ITKv3DeformableRegistration15 ${ITK_LIBRARIES})

add_executable(ITKv3DeformableRegistration16 DeformableRegistration16.cxx )
target_link_libraries(ITKv3DeformableRegistration16 ${ITK_LIBRARIES})

add_executable(ITKv3DeformableRegistration17 DeformableRegistration17.cxx )
target_link_libraries(ITKv3DeformableRegistration17 ${ITK_LIBRARIES})

add_executable(ITKv3DisplacementFieldInitialization DisplacementFieldInitialization.cxx )
target_link_libraries(ITKv3DisplacementFieldInitialization ${ITK_LIBRARIES})

add_executable(ITKv3LandmarkWarping2 LandmarkWarping2.cxx )
target_link_libraries(ITKv3LandmarkWarping2 ${ITK_LIBRARIES})

add_executable(ITKv3BSplineWarping1 BSplineWarping1.cxx )
target_link_libraries(ITKv3BSplineWarping1 ${ITK_LIBRARIES})

add_executable(ITKv3BSplineWarping2 BSplineWarping2.cxx )
target_link_libraries(ITKv3BSplineWarping2 ${ITK_LIBRARIES})

add_executable(ITKv3ThinPlateSplineWarp ThinPlateSplineWarp.cxx )
target_link_libraries(ITKv3ThinPlateSplineWarp ${ITK_LIBRARIES})

add_executable(ITKv3ModelToImageRegistration1 ModelToImageRegistration1.cxx )
target_link_libraries(ITKv3ModelToImageRegistration1 ${ITK_LIBRARIES})

add_executable(ITKv3ModelToImageRegistration2 ModelToImageRegistration2.cxx )
target_link_libraries(ITKv3ModelToImageRegistration2 ${ITK_LIBRARIES})

add_executable(ITKv3ImageRegistration1 ImageRegistration1.cxx )
target_link_libraries(ITKv3ImageRegistration1 ${ITK_LIBRARIES})

add_executable(ITKv3ImageRegistration2 ImageRegistration2.cxx )
target_link_libraries(ITKv3ImageRegistration2 ${ITK_LIBRARIES})

add_executable(ITKv3ImageRegistration17 ImageRegistration17.cxx )
target_link_libraries(ITKv3ImageRegistration17 ${ITK_LIBRARIES})

add_executable(ITKv3ImageRegistrationHistogramPlotter ImageRegistrationHistogramPlotter.cxx )
target_link_libraries(ITKv3ImageRegistrationHistogramPlotter ${ITK_LIBRARIES})

add_executable(ITKv3MeanSquaresImageMetric1  MeanSquaresImageMetric1.cxx )
target_link_libraries(ITKv3MeanSquaresImageMetric1 ${ITK_LIBRARIES})

add_executable(ITKv3DeformationFieldJacobian DeformationFieldJacobian.cxx )
target_link_libraries(ITKv3DeformationFieldJacobian ${ITK_LIBRARIES})

add_executable(ITKv3IterativeClosestPoint1   IterativeClosestPoint1.cxx )
target_link_libraries(ITKv3IterativeClosestPoint1 ${ITK_LIBRARIES})

add_executable(ITKv3IterativeClosestPoint2   IterativeClosestPoint2.cxx )
target_link_libraries(ITKv3IterativeClosestPoint2 ${ITK_LIBRARIES})

add_executable(ITKv3IterativeClosestPoint3   IterativeClosestPoint3.cxx )
target_link_libraries(ITKv3IterativeClosestPoint3 ${ITK_LIBRARIES})

if(BUILD_TESTING)
  add_subdirectory(test)
endif()
