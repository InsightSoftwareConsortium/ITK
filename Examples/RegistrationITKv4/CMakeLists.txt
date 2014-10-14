project(ITKv4ImageRegistration)

add_executable(ImageRegistration3 ImageRegistration3.cxx )
target_link_libraries(ImageRegistration3 ${ITK_LIBRARIES})

add_executable(ImageRegistration5 ImageRegistration5.cxx )
target_link_libraries(ImageRegistration5 ${ITK_LIBRARIES})

add_executable(ImageRegistration6 ImageRegistration6.cxx )
target_link_libraries(ImageRegistration6 ${ITK_LIBRARIES})

add_executable(ImageRegistration8 ImageRegistration8.cxx )
target_link_libraries(ImageRegistration8 ${ITK_LIBRARIES})

add_executable(ImageRegistration9 ImageRegistration9.cxx )
target_link_libraries(ImageRegistration9 ${ITK_LIBRARIES})

add_executable(ImageRegistration10 ImageRegistration10.cxx )
target_link_libraries(ImageRegistration10 ${ITK_LIBRARIES})

add_executable(DeformableRegistration4 DeformableRegistration4.cxx )
target_link_libraries(DeformableRegistration4 ${ITK_LIBRARIES})

add_executable(ImageRegistration4 ImageRegistration4.cxx )
target_link_libraries(ImageRegistration4 ${ITK_LIBRARIES})

add_executable(ImageRegistration7 ImageRegistration7.cxx )
target_link_libraries(ImageRegistration7 ${ITK_LIBRARIES})

add_executable(ImageRegistration11 ImageRegistration11.cxx )
target_link_libraries(ImageRegistration11 ${ITK_LIBRARIES})

add_executable(ImageRegistration12 ImageRegistration12.cxx )
target_link_libraries(ImageRegistration12 ${ITK_LIBRARIES})

add_executable(ImageRegistration13 ImageRegistration13.cxx )
target_link_libraries(ImageRegistration13 ${ITK_LIBRARIES})

add_executable(ImageRegistration14 ImageRegistration14.cxx )
target_link_libraries(ImageRegistration14 ${ITK_LIBRARIES})

add_executable(ImageRegistration15 ImageRegistration15.cxx )
target_link_libraries(ImageRegistration15 ${ITK_LIBRARIES})

add_executable(ImageRegistration16 ImageRegistration16.cxx )
target_link_libraries(ImageRegistration16 ${ITK_LIBRARIES})

add_executable(ImageRegistration18 ImageRegistration18.cxx )
target_link_libraries(ImageRegistration18 ${ITK_LIBRARIES})

add_executable(ImageRegistration19 ImageRegistration19.cxx )
target_link_libraries(ImageRegistration19 ${ITK_LIBRARIES})

add_executable(ImageRegistration20 ImageRegistration20.cxx )
target_link_libraries(ImageRegistration20 ${ITK_LIBRARIES})

add_executable(DeformableRegistration6 DeformableRegistration6.cxx )
target_link_libraries(DeformableRegistration6 ${ITK_LIBRARIES})

add_executable(DeformableRegistration7 DeformableRegistration7.cxx )
target_link_libraries(DeformableRegistration7 ${ITK_LIBRARIES})

add_executable(DeformableRegistration8 DeformableRegistration8.cxx )
target_link_libraries(DeformableRegistration8 ${ITK_LIBRARIES})

if( ITK_USE_FFTWD )
  add_executable(DeformableRegistration9 DeformableRegistration9.cxx )
  target_link_libraries(DeformableRegistration9 ${ITK_LIBRARIES})

  add_executable(DeformableRegistration10 DeformableRegistration10.cxx )
  target_link_libraries(DeformableRegistration10 ${ITK_LIBRARIES})
endif()

add_executable(MultiResImageRegistration1 MultiResImageRegistration1.cxx )
target_link_libraries(MultiResImageRegistration1 ${ITK_LIBRARIES})

add_executable(MultiResImageRegistration2 MultiResImageRegistration2.cxx )
target_link_libraries(MultiResImageRegistration2 ${ITK_LIBRARIES})

add_executable(MultiResImageRegistration3 MultiResImageRegistration3.cxx )
target_link_libraries(MultiResImageRegistration3 ${ITK_LIBRARIES})

add_executable(MultiStageImageRegistration1 MultiStageImageRegistration1.cxx )
target_link_libraries(MultiStageImageRegistration1 ${ITK_LIBRARIES})

add_executable(MultiStageImageRegistration2 MultiStageImageRegistration2.cxx )
target_link_libraries(MultiStageImageRegistration2 ${ITK_LIBRARIES})

add_executable(DeformableRegistration1 DeformableRegistration1.cxx )
target_link_libraries(DeformableRegistration1 ${ITK_LIBRARIES})

add_executable(DeformableRegistration11 DeformableRegistration11.cxx )
target_link_libraries(DeformableRegistration11 ${ITK_LIBRARIES})

add_executable(DeformableRegistration2 DeformableRegistration2.cxx )
target_link_libraries(DeformableRegistration2 ${ITK_LIBRARIES})

add_executable(DeformableRegistration3 DeformableRegistration3.cxx )
target_link_libraries(DeformableRegistration3 ${ITK_LIBRARIES})

add_executable(DeformableRegistration5 DeformableRegistration5.cxx )
target_link_libraries(DeformableRegistration5 ${ITK_LIBRARIES})

add_executable(DeformableRegistration12 DeformableRegistration12.cxx )
target_link_libraries(DeformableRegistration12 ${ITK_LIBRARIES})

add_executable(DeformableRegistration13 DeformableRegistration13.cxx )
target_link_libraries(DeformableRegistration13 ${ITK_LIBRARIES})

add_executable(DeformableRegistration14 DeformableRegistration14.cxx )
target_link_libraries(DeformableRegistration14 ${ITK_LIBRARIES})

add_executable(ChangeInformationImageFilter ChangeInformationImageFilter.cxx )
target_link_libraries(ChangeInformationImageFilter ${ITK_LIBRARIES})

add_executable(DeformableRegistration15 DeformableRegistration15.cxx )
target_link_libraries(DeformableRegistration15 ${ITK_LIBRARIES})

add_executable(DeformableRegistration16 DeformableRegistration16.cxx )
target_link_libraries(DeformableRegistration16 ${ITK_LIBRARIES})

add_executable(DeformableRegistration17 DeformableRegistration17.cxx )
target_link_libraries(DeformableRegistration17 ${ITK_LIBRARIES})

add_executable(DisplacementFieldInitialization DisplacementFieldInitialization.cxx )
target_link_libraries(DisplacementFieldInitialization ${ITK_LIBRARIES})

add_executable(LandmarkWarping2 LandmarkWarping2.cxx )
target_link_libraries(LandmarkWarping2 ${ITK_LIBRARIES})

add_executable(BSplineWarping1 BSplineWarping1.cxx )
target_link_libraries(BSplineWarping1 ${ITK_LIBRARIES})

add_executable(BSplineWarping2 BSplineWarping2.cxx )
target_link_libraries(BSplineWarping2 ${ITK_LIBRARIES})

add_executable(ThinPlateSplineWarp ThinPlateSplineWarp.cxx )
target_link_libraries(ThinPlateSplineWarp ${ITK_LIBRARIES})

add_executable(ModelToImageRegistration1 ModelToImageRegistration1.cxx )
target_link_libraries(ModelToImageRegistration1 ${ITK_LIBRARIES})

add_executable(ModelToImageRegistration2 ModelToImageRegistration2.cxx )
target_link_libraries(ModelToImageRegistration2 ${ITK_LIBRARIES})

add_executable(ImageRegistration1 ImageRegistration1.cxx )
target_link_libraries(ImageRegistration1 ${ITK_LIBRARIES})

add_executable(ImageRegistration2 ImageRegistration2.cxx )
target_link_libraries(ImageRegistration2 ${ITK_LIBRARIES})

add_executable(ImageRegistration17 ImageRegistration17.cxx )
target_link_libraries(ImageRegistration17 ${ITK_LIBRARIES})

add_executable(ImageRegistrationHistogramPlotter ImageRegistrationHistogramPlotter.cxx )
target_link_libraries(ImageRegistrationHistogramPlotter ${ITK_LIBRARIES})

add_executable(MeanSquaresImageMetric1  MeanSquaresImageMetric1.cxx )
target_link_libraries(MeanSquaresImageMetric1 ${ITK_LIBRARIES})

add_executable(IterativeClosestPoint1   IterativeClosestPoint1.cxx )
target_link_libraries(IterativeClosestPoint1 ${ITK_LIBRARIES})

add_executable(IterativeClosestPoint2   IterativeClosestPoint2.cxx )
target_link_libraries(IterativeClosestPoint2 ${ITK_LIBRARIES})

add_executable(IterativeClosestPoint3   IterativeClosestPoint3.cxx )
target_link_libraries(IterativeClosestPoint3 ${ITK_LIBRARIES})

if(BUILD_TESTING)
  add_subdirectory(test)
endif()
