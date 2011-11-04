itk_module_test()
set(ITKImageSourceTests
  itkGaborImageSourceTest.cxx
  itkGaussianImageSourceTest.cxx
  itkGridImageSourceTest.cxx
)

CreateTestDriver(ITKImageSource "${ITKImageSource-Test_LIBRARIES}" "${ITKImageSourceTests}")

itk_add_test(NAME itkGaborImageSourceTest0
      COMMAND ITKImageSourceTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Filtering/GaborImageSourceTest0.mha}
              ${ITK_TEST_OUTPUT_DIR}/GaborImageSourceTest0.mha
    itkGaborImageSourceTest ${ITK_TEST_OUTPUT_DIR}/GaborImageSourceTest0.mha 0)
itk_add_test(NAME itkGaborImageSourceTest1
      COMMAND ITKImageSourceTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Filtering/GaborImageSourceTest1.mha}
              ${ITK_TEST_OUTPUT_DIR}/GaborImageSourceTest1.mha
    itkGaborImageSourceTest ${ITK_TEST_OUTPUT_DIR}/GaborImageSourceTest1.mha 1)
itk_add_test(NAME itkGaussianImageSourceTest
      COMMAND ITKImageSourceTestDriver itkGaussianImageSourceTest)
itk_add_test(NAME itkGridImageSourceTest1
      COMMAND ITKImageSourceTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Filtering/itkGridImageSourceTest1.nhdr,itkGridImageSourceTest1.raw.gz}
              ${ITK_TEST_OUTPUT_DIR}/itkGridImageSourceTest1.nhdr
    itkGridImageSourceTest ${ITK_TEST_OUTPUT_DIR}/itkGridImageSourceTest1.nhdr 0)
itk_add_test(NAME itkGridImageSourceTest2
      COMMAND ITKImageSourceTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Filtering/itkGridImageSourceTest2.nhdr,itkGridImageSourceTest2.raw.gz}
              ${ITK_TEST_OUTPUT_DIR}/itkGridImageSourceTest2.nhdr
    itkGridImageSourceTest ${ITK_TEST_OUTPUT_DIR}/itkGridImageSourceTest2.nhdr 1)
itk_add_test(NAME itkGridImageSourceTest3
      COMMAND ITKImageSourceTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Filtering/itkGridImageSourceTest3.nhdr,itkGridImageSourceTest3.raw.gz}
              ${ITK_TEST_OUTPUT_DIR}/itkGridImageSourceTest3.nhdr
    itkGridImageSourceTest ${ITK_TEST_OUTPUT_DIR}/itkGridImageSourceTest3.nhdr 2)
itk_add_test(NAME itkGridImageSourceTest4
      COMMAND ITKImageSourceTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Filtering/itkGridImageSourceTest4.nhdr,itkGridImageSourceTest4.raw.gz}
              ${ITK_TEST_OUTPUT_DIR}/itkGridImageSourceTest4.nhdr
    itkGridImageSourceTest ${ITK_TEST_OUTPUT_DIR}/itkGridImageSourceTest4.nhdr 3)
