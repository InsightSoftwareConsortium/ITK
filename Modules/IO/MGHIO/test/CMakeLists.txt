itk_module_test()
set(ITKIOMGHTests
    itkMGHImageIOTest.cxx
)

set(MGH_DATA_ROOT ${CMAKE_CURRENT_SOURCE_DIR}/MD5)

CreateTestDriver(ITKIOMGH  "${ITKIOMGH-Test_LIBRARIES}" "${ITKIOMGHTests}")

itk_add_test(NAME MGHFactoryCreationTest
      COMMAND ITKIOMGHTestDriver itkMGHImageIOTest
              ${ITK_TEST_OUTPUT_DIR} FactoryCreationTest )

itk_add_test(NAME MGHReadImagesTest_mgz
      COMMAND ITKIOMGHTestDriver itkMGHImageIOTest
              ${ITK_TEST_OUTPUT_DIR} ReadImagesTest
              DATA{${MGH_DATA_ROOT}/T1.mgz} TEST.mgz
              )

itk_add_test(NAME MGHReadImagesTest_mgh
      COMMAND ITKIOMGHTestDriver itkMGHImageIOTest
              ${ITK_TEST_OUTPUT_DIR} ReadImagesTest
              DATA{${MGH_DATA_ROOT}/T1_uncompresed.mgh} TEST.mgh
              )

itk_add_test(NAME MGHReadImagesTest_mgh.gz
      COMMAND ITKIOMGHTestDriver itkMGHImageIOTest
              ${ITK_TEST_OUTPUT_DIR} ReadImagesTest
              DATA{${MGH_DATA_ROOT}/T1_longname.mgh.gz} TEST.mgh.gz
              )

itk_add_test(NAME itkITKIOMGHInternalTests
      COMMAND ITKIOMGHTestDriver itkMGHImageIOTest
              ${ITK_TEST_OUTPUT_DIR} TestReadWriteOfSmallImageOfAllTypes )
itk_add_test(NAME itkITKIOMGHOriginTest
  COMMAND ITKIOMGHTestDriver itkMGHImageIOTest
  ${ITK_TEST_OUTPUT_DIR} TestOriginWriteTest
  DATA{${MGH_DATA_ROOT}/T1_longname.mgh.gz} OriginTEST.mgh.gz
  )

