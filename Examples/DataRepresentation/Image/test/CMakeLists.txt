set(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)

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
