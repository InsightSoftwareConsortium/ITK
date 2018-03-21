itk_module_test()

set(MontageTests
  itkPhaseCorrelationImageRegistrationMethodTest.cxx
  itkPhaseCorrelationImageRegistrationMethodTestFiles.cxx
  itkMockMontageTest.cxx
  )

CreateTestDriver(Montage "${Montage-Test_LIBRARIES}" "${MontageTests}")

set(TESTING_OUTPUT_PATH "${CMAKE_CURRENT_BINARY_DIR}/../Testing/Temporary")

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTest_2cc
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTest
    2cc
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTest_2cc.nrrd
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTest_2cc.tfm
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTest_2ff
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTest
    2ff
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTest_2ff.nrrd
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTest_2ff.tfm
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTest_2dd
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTest
    2dd
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTest_2dd.nrrd
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTest_2dd.tfm
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTest_2cf
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTest
    2cf
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTest_2cf.nrrd
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTest_2cf.tfm
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTest_2fd
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTest
    2fd
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTest_2cf.nrrd
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTest_2cf.tfm
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTest_3cc
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTest
    3cc
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTest_3cc.nrrd
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTest_3cc.tfm
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTest_3ff
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTest
    3ff
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTest_3ff.nrrd
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTest_3ff.tfm
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTest_3dd
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTest
    3dd
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTest_3dd.nrrd
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTest_3dd.tfm
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTest_3cf
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTest
    3cf
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTest_3cf.nrrd
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTest_3cf.tfm
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTest_3fd
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTest
    3fd
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTest_3fd.nrrd
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTest_3fd.tfm
  )


itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTest_ShouldFail
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTest
    2cc
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTest_ShouldNotExist.nrrd
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTest_ShouldNotExist.tfm
    0.9 1.1
  )
set_tests_properties(itkPhaseCorrelationImageRegistrationMethodTest_ShouldFail PROPERTIES WILL_FAIL TRUE)

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTestFiles
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTestFiles
    DATA{Input/OMC/FlatField/14/100.tif}
    DATA{Input/OMC/FlatField/14/101.tif}
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTestFiles.nrrd
    1136.0
    0.0
    12.0651
    4.6711893
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTestTiles
  COMMAND MontageTestDriver
  itkMockMontageTest
    ${CMAKE_CURRENT_LIST_DIR}/Input/Tiles
    ${TESTING_OUTPUT_PATH}/itkMockMontageTest.csv
  )
