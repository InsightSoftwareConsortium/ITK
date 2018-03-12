itk_module_test()

set(MontageTests
  itkPhaseCorrelationImageRegistrationMethodTest.cxx
  itkPhaseCorrelationImageRegistrationMethodTestFiles.cxx
  )

CreateTestDriver(Montage "${Montage-Test_LIBRARIES}" "${MontageTests}")

set(TESTING_OUTPUT_PATH "${CMAKE_CURRENT_BINARY_DIR}/../Testing/Temporary")

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTest_NoResampling
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTest
    3cc
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTest_NoResampling.nrrd
    1.0 1.0 1.0
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTest_2cc
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTest
    2cc
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTest_2cc.nrrd
    0.7 0.8
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTest_2ff
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTest
    2ff
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTest_2ff.nrrd
    0.7 0.8
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTest_2dd
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTest
    2dd
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTest_2dd.nrrd
    0.7 0.8
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTest_2cf
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTest
    2cf
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTest_2cf.nrrd
    0.7 0.8
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTest_2fd
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTest
    2fd
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTest_2cf.nrrd
    0.7 0.8
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTest_3cc
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTest
    3cc
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTest_3cc.nrrd
    0.7 0.8 0.9
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTest_3ff
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTest
    3ff
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTest_3ff.nrrd
    0.7 0.8 0.9
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTest_3dd
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTest
    3dd
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTest_3dd.nrrd
    0.7 0.8 0.9
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTest_3cf
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTest
    3cf
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTest_3cf.nrrd
    0.7 0.8 0.9
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTest_3fd
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTest
    3fd
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTest_3fd.nrrd
    0.7 0.8 0.9
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTestFiles
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTestFiles
    DATA{Input/OMC/FlatField/14/100.tif}
    DATA{Input/OMC/FlatField/14/101.tif}
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTestFiles.nrrd
    168
    -5
  )
