itk_module_test()
set(ITK-IO-StimulateTests
itkStimulateImageIOTest.cxx
itkIOStimulateHeaderTest.cxx
itkStimulateImageIOTest2.cxx
)

CreateTestDriver(ITK-IO-Stimulate  "${ITK-IO-Stimulate-Test_LIBRARIES}" "${ITK-IO-StimulateTests}")

itk_add_test(NAME itkIOStimulateHeaderTest
      COMMAND ITK-IO-StimulateTestDriver itkIOStimulateHeaderTest)
itk_add_test(NAME itkStimulateImageIOTest
      COMMAND ITK-IO-StimulateTestDriver itkStimulateImageIOTest
              ${ITK_TEST_OUTPUT_DIR}/test1.spr ${ITK_TEST_OUTPUT_DIR}/test2.spr)
itk_add_test(NAME itkStimulateImageIOTest2
      COMMAND ITK-IO-StimulateTestDriver itkStimulateImageIOTest2
              ${ITK_DATA_ROOT}/Input/BigEndian.spr)
