itk_module_test()

set(MontageTests
  itkPhaseCorrelationImageRegistrationMethodTestSynthetic.cxx
  itkPhaseCorrelationImageRegistrationMethodTestFiles.cxx
  itkMockMontageTestTiles.cxx
  itkMockMontageTestOMC.cxx
  )

CreateTestDriver(Montage "${Montage-Test_LIBRARIES}" "${MontageTests}")

set(TESTING_OUTPUT_PATH "${CMAKE_CURRENT_BINARY_DIR}/../Testing/Temporary")

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTestSynthetic_2cc
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTestSynthetic
    2cc
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTestSynthetic_2cc.nrrd
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTestSynthetic_2cc.tfm
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTestSynthetic_2ff
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTestSynthetic
    2ff
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTestSynthetic_2ff.nrrd
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTestSynthetic_2ff.tfm
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTestSynthetic_2dd
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTestSynthetic
    2dd
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTestSynthetic_2dd.nrrd
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTestSynthetic_2dd.tfm
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTestSynthetic_2cf
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTestSynthetic
    2cf
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTestSynthetic_2cf.nrrd
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTestSynthetic_2cf.tfm
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTestSynthetic_2fd
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTestSynthetic
    2fd
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTestSynthetic_2cf.nrrd
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTestSynthetic_2cf.tfm
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTestSynthetic_3cc
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTestSynthetic
    3cc
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTestSynthetic_3cc.nrrd
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTestSynthetic_3cc.tfm
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTestSynthetic_3ff
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTestSynthetic
    3ff
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTestSynthetic_3ff.nrrd
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTestSynthetic_3ff.tfm
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTestSynthetic_3dd
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTestSynthetic
    3dd
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTestSynthetic_3dd.nrrd
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTestSynthetic_3dd.tfm
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTestSynthetic_3cf
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTestSynthetic
    3cf
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTestSynthetic_3cf.nrrd
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTestSynthetic_3cf.tfm
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTestSynthetic_3fd
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTestSynthetic
    3fd
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTestSynthetic_3fd.nrrd
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTestSynthetic_3fd.tfm
  )


itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTestSynthetic_ShouldFail
  COMMAND MontageTestDriver
  itkPhaseCorrelationImageRegistrationMethodTestSynthetic
    2cc
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTestSynthetic_ShouldNotExist.nrrd
    ${TESTING_OUTPUT_PATH}/itkPhaseCorrelationImageRegistrationMethodTestSynthetic_ShouldNotExist.tfm
    0.9 1.1
  )
set_tests_properties(itkPhaseCorrelationImageRegistrationMethodTestSynthetic_ShouldFail PROPERTIES WILL_FAIL TRUE)

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

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTestOMC
  COMMAND MontageTestDriver
  itkMockMontageTestOMC
    DATA{${CMAKE_CURRENT_LIST_DIR}/Input/OMC/FlatField/14/,REGEX:.*}
    ${TESTING_OUTPUT_PATH}/itkMockMontageTestOMC
  )

itk_add_test(NAME itkPhaseCorrelationImageRegistrationMethodTestTiles
  COMMAND MontageTestDriver
  itkMockMontageTestTiles
    ${CMAKE_CURRENT_LIST_DIR}/Input/Tiles
    ${TESTING_OUTPUT_PATH}/itkMockMontageTestTiles
  )
