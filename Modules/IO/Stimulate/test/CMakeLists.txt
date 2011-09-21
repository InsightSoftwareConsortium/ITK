itk_module_test()
set(ITKIOStimulateTests
itkStimulateImageIOTest.cxx
itkStimulateImageIOTest2.cxx
)

CreateTestDriver(ITKIOStimulate  "${ITKIOStimulate-Test_LIBRARIES}" "${ITKIOStimulateTests}")

itk_add_test(NAME itkStimulateImageIOTest
      COMMAND ITKIOStimulateTestDriver itkStimulateImageIOTest
              ${ITK_TEST_OUTPUT_DIR}/test1.spr ${ITK_TEST_OUTPUT_DIR}/test2.spr)
itk_add_test(NAME itkStimulateImageIOTest2
      COMMAND ITKIOStimulateTestDriver itkStimulateImageIOTest2
              DATA{${ITK_DATA_ROOT}/Input/BigEndian.spr,BigEndian.img})
