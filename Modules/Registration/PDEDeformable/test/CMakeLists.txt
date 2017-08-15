itk_module_test()
set(ITKPDEDeformableRegistrationTests
itkMultiResolutionPDEDeformableRegistrationTest.cxx
itkDemonsRegistrationFilterTest.cxx
itkDiffeomorphicDemonsRegistrationFilterTest.cxx
itkDiffeomorphicDemonsRegistrationFilterTest2.cxx
itkFastSymmetricForcesDemonsRegistrationFilterTest.cxx
itkLevelSetMotionRegistrationFilterTest.cxx
itkSymmetricForcesDemonsRegistrationFilterTest.cxx
)
 # Define some convenient locations
set(BASELINE ${ITK_DATA_ROOT}/Baseline/Algorithms)
set(INPUTDATA ${ITK_DATA_ROOT}/Input)
set(TEMP ${ITK_TEST_OUTPUT_DIR})

CreateTestDriver(ITKPDEDeformableRegistration  "${ITKPDEDeformableRegistration-Test_LIBRARIES}" "${ITKPDEDeformableRegistrationTests}")

itk_add_test(NAME itkDemonsRegistrationFilterTest
      COMMAND ITKPDEDeformableRegistrationTestDriver itkDemonsRegistrationFilterTest)
itk_add_test(NAME itkLevelSetMotionRegistrationFilterTest
      COMMAND ITKPDEDeformableRegistrationTestDriver itkLevelSetMotionRegistrationFilterTest
              ${ITK_TEST_OUTPUT_DIR}/itkLevelSetMotionRegistrationFilterTestFixedImage.mha ${ITK_TEST_OUTPUT_DIR}/itkLevelSetMotionRegistrationFilterTestMovingImage.mha ${ITK_TEST_OUTPUT_DIR}/itkLevelSetMotionRegistrationFilterTestResampledImage.mha)
itk_add_test(NAME itkSymmetricForcesDemonsRegistrationFilterTest
      COMMAND ITKPDEDeformableRegistrationTestDriver itkSymmetricForcesDemonsRegistrationFilterTest)
itk_add_test(NAME itkMultiResolutionPDEDeformableRegistrationTestD ${TestDriver}
      COMMAND ITKPDEDeformableRegistrationTestDriver
            --compare DATA{${BASELINE}/itkMultiResolutionPDEDeformableRegistrationTestPixelCentered.png}
            ${TEMP}/itkMultiResolutionPDEDeformableRegistrationTestPixelCentered.png
  --compareNumberOfPixelsTolerance 20
  itkMultiResolutionPDEDeformableRegistrationTest
            ${TEMP}/itkMultiResolutionPDEDeformableRegistrationTestPixelCentered.png)
itk_add_test(NAME itkDiffeomorphicDemonsRegistrationFilterTest01
      COMMAND ITKPDEDeformableRegistrationTestDriver  itkDiffeomorphicDemonsRegistrationFilterTest
              0 0 0.001 20.0 200 0.01 0.08 5 10)
itk_add_test(NAME itkDiffeomorphicDemonsRegistrationFilterTest02
      COMMAND ITKPDEDeformableRegistrationTestDriver  --redirectOutput itkDiffeomorphicDemonsRegistrationFilterTest02 --redirectOutput ${TEMP}/itkDiffeomorphicDemonsRegistrationFilterTest02.txt
          itkDiffeomorphicDemonsRegistrationFilterTest
              1 0 0.01 3.0 400 0.2 0.9 10 70)
set_tests_properties(itkDiffeomorphicDemonsRegistrationFilterTest02 PROPERTIES ATTACHED_FILES_ON_FAIL ${TEMP}/itkDiffeomorphicDemonsRegistrationFilterTest02.txt)

itk_add_test(NAME itkDiffeomorphicDemonsRegistrationFilterTest03
      COMMAND ITKPDEDeformableRegistrationTestDriver  itkDiffeomorphicDemonsRegistrationFilterTest
              2 0 0.001 20.0 200 0.01 0.08 5 10)
itk_add_test(NAME itkDiffeomorphicDemonsRegistrationFilterTest04
      COMMAND ITKPDEDeformableRegistrationTestDriver  itkDiffeomorphicDemonsRegistrationFilterTest
              3 0 0.01 3.0 200 0.2 0.8 5 70)

itk_add_test(NAME itkDiffeomorphicDemonsRegistrationFilterTest05
      COMMAND ITKPDEDeformableRegistrationTestDriver  --redirectOutput ${TEMP}/itkDiffeomorphicDemonsRegistrationFilterTest05.txt
          itkDiffeomorphicDemonsRegistrationFilterTest
              0 1 0.01 3.0 400 0.2 0.08 5 70)
set_tests_properties(itkDiffeomorphicDemonsRegistrationFilterTest05 PROPERTIES ATTACHED_FILES_ON_FAIL ${TEMP}/itkDiffeomorphicDemonsRegistrationFilterTest05.txt)

itk_add_test(NAME itkDiffeomorphicDemonsRegistrationFilterTest06
      COMMAND ITKPDEDeformableRegistrationTestDriver  itkDiffeomorphicDemonsRegistrationFilterTest
              0 0 0.005 20.0 200 0.01 0.08 5 10)
itk_add_test(NAME itkDiffeomorphicDemonsRegistrationFilterTest07
      COMMAND ITKPDEDeformableRegistrationTestDriver  itkDiffeomorphicDemonsRegistrationFilterTest2
              ${ITK_EXAMPLE_DATA_ROOT}/RatLungSlice1.mha ${ITK_EXAMPLE_DATA_ROOT}/RatLungSlice2.mha ${ITK_TEST_OUTPUT_DIR}/itkDiffeomorphicDemonsRegistrationFilterTest07.mha 0 0 0.001 0.1)
itk_add_test(NAME itkDiffeomorphicDemonsRegistrationFilterTest08
      COMMAND ITKPDEDeformableRegistrationTestDriver  itkDiffeomorphicDemonsRegistrationFilterTest2
              ${ITK_EXAMPLE_DATA_ROOT}/RatLungSlice1.mha ${ITK_EXAMPLE_DATA_ROOT}/RatLungSlice2.mha ${ITK_TEST_OUTPUT_DIR}/itkDiffeomorphicDemonsRegistrationFilterTest08.mha 1 0 0.001 0.1)
itk_add_test(NAME itkDiffeomorphicDemonsRegistrationFilterTest09
      COMMAND ITKPDEDeformableRegistrationTestDriver  itkDiffeomorphicDemonsRegistrationFilterTest2
              ${ITK_EXAMPLE_DATA_ROOT}/RatLungSlice1.mha ${ITK_EXAMPLE_DATA_ROOT}/RatLungSlice2.mha ${ITK_TEST_OUTPUT_DIR}/itkDiffeomorphicDemonsRegistrationFilterTest09.mha 2 0 0.001 0.1)
itk_add_test(NAME itkDiffeomorphicDemonsRegistrationFilterTest10
      COMMAND ITKPDEDeformableRegistrationTestDriver  itkDiffeomorphicDemonsRegistrationFilterTest2
              ${ITK_EXAMPLE_DATA_ROOT}/RatLungSlice1.mha ${ITK_EXAMPLE_DATA_ROOT}/RatLungSlice2.mha ${ITK_TEST_OUTPUT_DIR}/itkDiffeomorphicDemonsRegistrationFilterTest10.mha 3 0 0.001 0.1)
itk_add_test(NAME itkDiffeomorphicDemonsRegistrationFilterTest11
      COMMAND ITKPDEDeformableRegistrationTestDriver  itkDiffeomorphicDemonsRegistrationFilterTest2
              ${ITK_EXAMPLE_DATA_ROOT}/RatLungSlice1.mha ${ITK_EXAMPLE_DATA_ROOT}/RatLungSlice2.mha ${ITK_TEST_OUTPUT_DIR}/itkDiffeomorphicDemonsRegistrationFilterTest11.mha 0 1 0.001 0.1)
itk_add_test(NAME itkFastSymmetricForcesDemonsRegistrationFilterTest
      COMMAND ITKPDEDeformableRegistrationTestDriver itkFastSymmetricForcesDemonsRegistrationFilterTest)
