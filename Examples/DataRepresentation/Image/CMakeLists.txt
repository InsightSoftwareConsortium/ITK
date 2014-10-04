project(ImageExamples)

add_executable(Image1 Image1.cxx )
target_link_libraries(Image1 ${ITK_LIBRARIES})

add_executable(Image2 Image2.cxx )
target_link_libraries(Image2 ${ITK_LIBRARIES})

add_executable(Image3 Image3.cxx )
target_link_libraries(Image3 ${ITK_LIBRARIES})

add_executable(Image4 Image4.cxx )
target_link_libraries(Image4 ${ITK_LIBRARIES})

add_executable(VectorImage VectorImage.cxx )
target_link_libraries(VectorImage ${ITK_LIBRARIES})

add_executable(ImageAdaptor2 ImageAdaptor2.cxx )
target_link_libraries(ImageAdaptor2 ${ITK_LIBRARIES})

add_executable(ImageAdaptor3 ImageAdaptor3.cxx )
target_link_libraries(ImageAdaptor3 ${ITK_LIBRARIES})

add_executable(ImageAdaptor4 ImageAdaptor4.cxx )
target_link_libraries(ImageAdaptor4 ${ITK_LIBRARIES})


add_executable(Image5 Image5.cxx )
target_link_libraries(Image5 ${ITK_LIBRARIES})

add_executable(ImageAdaptor1 ImageAdaptor1.cxx )
target_link_libraries(ImageAdaptor1 ${ITK_LIBRARIES})

add_executable(RGBImage RGBImage.cxx )
target_link_libraries(RGBImage ${ITK_LIBRARIES})



if(BUILD_TESTING)
  add_subdirectory(test)
endif()

