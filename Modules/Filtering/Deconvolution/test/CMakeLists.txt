itk_module_test()
set(ITKDeconvolutionTests
  itkLandweberDeconvolutionImageFilterTest.cxx
  itkProjectedIterativeDeconvolutionImageFilterTest.cxx
  itkProjectedLandweberDeconvolutionImageFilterTest.cxx
  itkRichardsonLucyDeconvolutionImageFilterTest.cxx
)

CreateTestDriver(ITKDeconvolution "${ITKDeconvolution-Test_LIBRARIES}" "${ITKDeconvolutionTests}")

itk_add_test(NAME itkRichardsonLucyDeconvolutionImageFilterGaussianKernelTest
      COMMAND ITKDeconvolutionTestDriver
    --compare DATA{Baseline/itkRichardsonLucyDeconvolutionImageFilterGaussianKernelTest.nrrd}
              ${ITK_TEST_OUTPUT_DIR}/itkRichardsonLucyDeconvolutionImageFilterGaussianKernelTest.nrrd
    itkRichardsonLucyDeconvolutionImageFilterTest
      DATA{Input/itkDeconvolutionImageFilterTestInput.png}
      DATA{Input/itkDeconvolutionImageFilterTestKernel.png}
      ${ITK_TEST_OUTPUT_DIR}/itkRichardsonLucyDeconvolutionImageFilterGaussianKernelTest.nrrd 1
)
itk_add_test(NAME itkRichardsonLucyDeconvolutionImageFilterIrregularKernelTest
      COMMAND ITKDeconvolutionTestDriver
    --compare DATA{Baseline/itkRichardsonLucyDeconvolutionImageFilterIrregularKernelTest.nrrd}
              ${ITK_TEST_OUTPUT_DIR}/itkRichardsonLucyDeconvolutionImageFilterIrregularKernelTest.nrrd
    itkRichardsonLucyDeconvolutionImageFilterTest
      DATA{Input/itkDeconvolutionImageFilterTestInput.png}
      DATA{Input/itkDeconvolutionImageFilterTestKernelIrregular.tif}
      ${ITK_TEST_OUTPUT_DIR}/itkRichardsonLucyDeconvolutionImageFilterIrregularKernelTest.nrrd 1
)
itk_add_test(NAME itkLandweberDeconvolutionImageFilterGaussianKernelTest
      COMMAND ITKDeconvolutionTestDriver
    --compare DATA{Baseline/itkLandweberDeconvolutionImageFilterGaussianKernelTest.nrrd}
              ${ITK_TEST_OUTPUT_DIR}/itkLandweberDeconvolutionImageFilterGaussianKernelTest.nrrd
    itkLandweberDeconvolutionImageFilterTest
      DATA{Input/itkDeconvolutionImageFilterTestInput.png}
      DATA{Input/itkDeconvolutionImageFilterTestKernel.png}
      ${ITK_TEST_OUTPUT_DIR}/itkLandweberDeconvolutionImageFilterGaussianKernelTest.nrrd 1 2.0
)
itk_add_test(NAME itkLandweberDeconvolutionImageFilterIrregularKernelTest
      COMMAND ITKDeconvolutionTestDriver
    --compare DATA{Baseline/itkLandweberDeconvolutionImageFilterIrregularKernelTest.nrrd}
              ${ITK_TEST_OUTPUT_DIR}/itkLandweberDeconvolutionImageFilterIrregularKernelTest.nrrd
    itkLandweberDeconvolutionImageFilterTest
      DATA{Input/itkDeconvolutionImageFilterTestInput.png}
      DATA{Input/itkDeconvolutionImageFilterTestKernelIrregular.tif}
      ${ITK_TEST_OUTPUT_DIR}/itkLandweberDeconvolutionImageFilterIrregularKernelTest.nrrd 1 2.0
)
itk_add_test(NAME itkProjectedIterativeDeconvolutionimageFilterTest
      COMMAND ITKDeconvolutionTestDriver
      itkProjectedIterativeDeconvolutionImageFilterTest
)
itk_add_test(NAME itkProjectedLandweberDeconvolutionImageFilterGaussianKernelTest
      COMMAND ITKDeconvolutionTestDriver
    --compare DATA{Baseline/itkProjectedLandweberDeconvolutionImageFilterGaussianKernelTest.nrrd}
              ${ITK_TEST_OUTPUT_DIR}/itkProjectedLandweberDeconvolutionImageFilterGaussianKernelTest.nrrd
    itkProjectedLandweberDeconvolutionImageFilterTest
      DATA{Input/itkDeconvolutionImageFilterTestInput.png}
      DATA{Input/itkDeconvolutionImageFilterTestKernel.png}
      ${ITK_TEST_OUTPUT_DIR}/itkProjectedLandweberDeconvolutionImageFilterGaussianKernelTest.nrrd 1 2.0
)
itk_add_test(NAME itkProjectedLandweberDeconvolutionImageFilterIrregularKernelTest
      COMMAND ITKDeconvolutionTestDriver
    --compare DATA{Baseline/itkProjectedLandweberDeconvolutionImageFilterIrregularKernelTest.nrrd}
              ${ITK_TEST_OUTPUT_DIR}/itkProjectedLandweberDeconvolutionImageFilterIrregularKernelTest.nrrd
    itkProjectedLandweberDeconvolutionImageFilterTest
      DATA{Input/itkDeconvolutionImageFilterTestInput.png}
      DATA{Input/itkDeconvolutionImageFilterTestKernelIrregular.tif}
      ${ITK_TEST_OUTPUT_DIR}/itkProjectedLandweberDeconvolutionImageFilterIrregularKernelTest.nrrd 1 2.0
)
