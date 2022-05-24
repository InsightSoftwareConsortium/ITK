itk_module_test()

#${itk-module} will be the name of this module and will not need to be
#changed when this module is renamed.

set(${itk-module}Tests
  itkSLICImageFilterTest.cxx
)

CreateTestDriver(${itk-module}  "${${itk-module}-Test_LIBRARIES}" "${${itk-module}Tests}")

itk_add_test(NAME itkSLICImageFilterTest1
  COMMAND ${itk-module}TestDriver
    --compare "DATA{Data/Baseline/itkSLICImageFilterTest1.png,:}"
              "${ITK_TEST_OUTPUT_DIR}/itkSLICImageFilterTestOutput1.png"
   itkSLICImageFilterTest
    "DATA{Data/Input/VM1111Shrink-RGB.png}"
    "${ITK_TEST_OUTPUT_DIR}/itkSLICImageFilterTestOutput1.png" 25 0)


itk_add_test(NAME itkSLICImageFilterTest2
  COMMAND ${itk-module}TestDriver
    --compare "DATA{Data/Baseline/itkSLICImageFilterTest2.png}"
              "${ITK_TEST_OUTPUT_DIR}/itkSLICImageFilterTestOutput2.png"
  itkSLICImageFilterTest
    "DATA{Data/Input/VM1111Shrink-LAB.mha}"
    "${ITK_TEST_OUTPUT_DIR}/itkSLICImageFilterTestOutput2.png" 25 0)



itk_add_test(NAME itkSLICImageFilterTest3
  COMMAND ${itk-module}TestDriver
    --compare "DATA{Data/Baseline/itkSLICImageFilterTest3.png}"
              "${ITK_TEST_OUTPUT_DIR}/itkSLICImageFilterTestOutput3.png"
  itkSLICImageFilterTest
    "DATA{Data/Input/VM1111Shrink-LAB.mha}"
    "${ITK_TEST_OUTPUT_DIR}/itkSLICImageFilterTestOutput3.png" 25 1)


set(${itk-module}GTests
  itkSLICImageFilterGTest.cxx)

CreateGoogleTestDriver(${itk-module}  "${${itk-module}-Test_LIBRARIES}" "${${itk-module}GTests}")
