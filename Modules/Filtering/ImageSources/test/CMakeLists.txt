itk_module_test()
set(ITKImageSourcesTests
  itkGaborKernelFunctionTest.cxx
  itkGaborImageSourceTest.cxx
  itkGaussianImageSourceTest.cxx
  itkGridImageSourceTest.cxx
  itkPhysicalPointImageSourceTest.cxx
)

CreateTestDriver(ITKImageSources "${ITKImageSources-Test_LIBRARIES}" "${ITKImageSourcesTests}")

itk_add_test(NAME itkGaborKernelFunctionTest
      COMMAND ITKImageSourcesTestDriver itkGaborKernelFunctionTest)
itk_add_test(NAME itkGaborImageSourceTest0
      COMMAND ITKImageSourcesTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Filtering/GaborImageSourceTest0.mha}
              ${ITK_TEST_OUTPUT_DIR}/GaborImageSourceTest0.mha
    itkGaborImageSourceTest ${ITK_TEST_OUTPUT_DIR}/GaborImageSourceTest0.mha 0)
itk_add_test(NAME itkGaborImageSourceTest1
      COMMAND ITKImageSourcesTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Filtering/GaborImageSourceTest1.mha}
              ${ITK_TEST_OUTPUT_DIR}/GaborImageSourceTest1.mha
    itkGaborImageSourceTest ${ITK_TEST_OUTPUT_DIR}/GaborImageSourceTest1.mha 1)
itk_add_test(NAME itkGaussianImageSourceTest1
      COMMAND ITKImageSourcesTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkGaussianImageSourceTest1.nrrd
              551a41aa2478ba271e3e1820ddffa03d
    itkGaussianImageSourceTest ${ITK_TEST_OUTPUT_DIR}/itkGaussianImageSourceTest1.nrrd 0)
itk_add_test(NAME itkGaussianImageSourceTest2
      COMMAND ITKImageSourcesTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkGaussianImageSourceTest2.nrrd
              5bf79f0539922f9c654dafd2601330b1
    itkGaussianImageSourceTest ${ITK_TEST_OUTPUT_DIR}/itkGaussianImageSourceTest2.nrrd 1)
itk_add_test(NAME itkGridImageSourceTest1
      COMMAND ITKImageSourcesTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Filtering/itkGridImageSourceTest1.nhdr,itkGridImageSourceTest1.raw.gz}
              ${ITK_TEST_OUTPUT_DIR}/itkGridImageSourceTest1.nhdr
    itkGridImageSourceTest ${ITK_TEST_OUTPUT_DIR}/itkGridImageSourceTest1.nhdr 64 3 0 8.0 0 0.0 1 0 1 0)
itk_add_test(NAME itkGridImageSourceTest2
      COMMAND ITKImageSourcesTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Filtering/itkGridImageSourceTest2.nhdr,itkGridImageSourceTest2.raw.gz}
              ${ITK_TEST_OUTPUT_DIR}/itkGridImageSourceTest2.nhdr
    itkGridImageSourceTest ${ITK_TEST_OUTPUT_DIR}/itkGridImageSourceTest2.nhdr 64 3 0 16.0 0 0.0 1 0 1 3)
itk_add_test(NAME itkGridImageSourceTest3
      COMMAND ITKImageSourcesTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Filtering/itkGridImageSourceTest3.nhdr,itkGridImageSourceTest3.raw.gz}
              ${ITK_TEST_OUTPUT_DIR}/itkGridImageSourceTest3.nhdr
    itkGridImageSourceTest ${ITK_TEST_OUTPUT_DIR}/itkGridImageSourceTest3.nhdr 32 3 0 4.0 0 0.0 1 1 0 0)
itk_add_test(NAME itkGridImageSourceTest4
      COMMAND ITKImageSourcesTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Filtering/itkGridImageSourceTest4.nhdr,itkGridImageSourceTest4.raw.gz}
              ${ITK_TEST_OUTPUT_DIR}/itkGridImageSourceTest4.nhdr
    itkGridImageSourceTest ${ITK_TEST_OUTPUT_DIR}/itkGridImageSourceTest4.nhdr 64 1.0 1 32.0 1 0.0 1 0 0 0)
itk_add_test(NAME itkPhysicalPointImageSourceTest0
      COMMAND ITKImageSourcesTestDriver
      --compare DATA{${ITK_DATA_ROOT}/Baseline/Filtering/itkPhysicalPointImageSourceTest0.mha}
                ${ITK_TEST_OUTPUT_DIR}/itkPhysicalPointImageSourceTest0.mha
      itkPhysicalPointImageSourceTest ${ITK_TEST_OUTPUT_DIR}/itkPhysicalPointImageSourceTest0.mha 0)
itk_add_test(NAME itkPhysicalPointImageSourceTest1
      COMMAND ITKImageSourcesTestDriver
      # this should be the same baseline as test 0
      --compare DATA{${ITK_DATA_ROOT}/Baseline/Filtering/itkPhysicalPointImageSourceTest0.mha}
                ${ITK_TEST_OUTPUT_DIR}/itkPhysicalPointImageSourceTest1.mha
      itkPhysicalPointImageSourceTest ${ITK_TEST_OUTPUT_DIR}/itkPhysicalPointImageSourceTest1.mha 1)
itk_add_test(NAME itkPhysicalPointImageSourceTest2
      COMMAND ITKImageSourcesTestDriver
      --compare DATA{${ITK_DATA_ROOT}/Baseline/Filtering/itkPhysicalPointImageSourceTest2.mha}
                ${ITK_TEST_OUTPUT_DIR}/itkPhysicalPointImageSourceTest2.mha
      itkPhysicalPointImageSourceTest ${ITK_TEST_OUTPUT_DIR}/itkPhysicalPointImageSourceTest2.mha 2)
itk_add_test(NAME itkPhysicalPointImageSourceTest3
      COMMAND ITKImageSourcesTestDriver
      --compare DATA{${ITK_DATA_ROOT}/Baseline/Filtering/itkPhysicalPointImageSourceTest0.mha}
                ${ITK_TEST_OUTPUT_DIR}/itkPhysicalPointImageSourceTest3.mha
      itkPhysicalPointImageSourceTest ${ITK_TEST_OUTPUT_DIR}/itkPhysicalPointImageSourceTest3.mha 3 0.0)
itk_add_test(NAME itkPhysicalPointImageSourceTest4
      COMMAND ITKImageSourcesTestDriver
      --compareIntensityTolerance .0001
      --compare DATA{${ITK_DATA_ROOT}/Baseline/Filtering/itkPhysicalPointImageSourceTest4.nrrd}
                ${ITK_TEST_OUTPUT_DIR}/itkPhysicalPointImageSourceTest4.nrrd
      itkPhysicalPointImageSourceTest ${ITK_TEST_OUTPUT_DIR}/itkPhysicalPointImageSourceTest4.nrrd 3 0.785398163 )
