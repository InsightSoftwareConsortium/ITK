project(ImageExamples)
include_regular_expression("^.*$")

set(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)

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



if( NOT ITK_DISABLE_CXX_TESTING )

if(BUILD_TESTING)
itk_add_test(NAME ImageAdaptor2Test COMMAND ${ITK_TEST_DRIVER}
 $<TARGET_FILE:ImageAdaptor2>
    ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
    ${TEMP}/ImageAdaptor2Test.png
)

itk_add_test(NAME ImageAdaptor3Test COMMAND ${ITK_TEST_DRIVER}
  $<TARGET_FILE:ImageAdaptor3>
    ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
    ${TEMP}/ImageAdaptor3Test.mhd 1
)

itk_add_test(NAME ImageAdaptor4Test COMMAND ${ITK_TEST_DRIVER}
  $<TARGET_FILE:ImageAdaptor4>
    ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
    ${TEMP}/ImageAdaptor4Test.png 127
)

itk_add_test(NAME ImageAdaptor1Test COMMAND ${ITK_TEST_DRIVER}
  $<TARGET_FILE:ImageAdaptor1>  ${ITK_SOURCE_DIR}/Examples/Data/BrainMidSagittalSlice.png
  )

itk_add_test(NAME Image1Test COMMAND ${ITK_TEST_DRIVER}
 $<TARGET_FILE:Image1>
)

itk_add_test(NAME Image2Test COMMAND ${ITK_TEST_DRIVER}
  $<TARGET_FILE:Image2>  ${ITK_SOURCE_DIR}/Examples/Data/BrainMidSagittalSlice.png
)

itk_add_test(NAME Image3Test COMMAND ${ITK_TEST_DRIVER}
  $<TARGET_FILE:Image3>
)

itk_add_test(NAME Image4Test COMMAND ${ITK_TEST_DRIVER}
   $<TARGET_FILE:Image4>
)

itk_add_test(NAME Image5Test COMMAND ${ITK_TEST_DRIVER}
   $<TARGET_FILE:Image5> ${TEMP}/Image5Test.mhd
)

itk_add_test(NAME RGBImageTest COMMAND ${ITK_TEST_DRIVER}
  $<TARGET_FILE:RGBImage>  ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
)

itk_add_test(NAME VectorImageTest COMMAND ${ITK_TEST_DRIVER}
   $<TARGET_FILE:VectorImage>
)

endif(BUILD_TESTING)
endif( NOT ITK_DISABLE_CXX_TESTING )

