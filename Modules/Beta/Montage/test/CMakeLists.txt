itk_module_test()

set(MontageTests
  itkPhaseCorrelationImageRegistrationMethodTest.cxx
  )

CreateTestDriver(Montage "${Montage-Test_LIBRARIES}" "${MontageTests}")

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTest_2cc
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTest
    2cc
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTest_2ff
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTest
    2ff
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTest_2dd
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTest
    2dd
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTest_2cf
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTest
    2cf
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTest_2fd
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTest
    2fd
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTest_3cc
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTest
    3cc
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTest_3ff
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTest
    3ff
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTest_3dd
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTest
    3dd
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTest_3cf
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTest
    3cf
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTest_3fd
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTest
    3fd
  )
