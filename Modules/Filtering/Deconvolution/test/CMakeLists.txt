itk_module_test()
set(ITKDeconvolutionTests
  itkInverseDeconvolutionImageFilterTest.cxx
  itkLandweberDeconvolutionImageFilterTest.cxx
  itkProjectedIterativeDeconvolutionImageFilterTest.cxx
  itkProjectedLandweberDeconvolutionImageFilterTest.cxx
  itkRichardsonLucyDeconvolutionImageFilterTest.cxx
  itkTikhonovDeconvolutionImageFilterTest.cxx
  itkWienerDeconvolutionImageFilterTest.cxx
  itkParametricBlindLeastSquaresDeconvolutionImageFilterTest.cxx
)

CreateTestDriver(ITKDeconvolution "${ITKDeconvolution-Test_LIBRARIES}" "${ITKDeconvolutionTests}")

itk_add_test(NAME itkRichardsonLucyDeconvolutionImageFilterGaussianKernelTest
      COMMAND ITKDeconvolutionTestDriver
    --compare DATA{Baseline/itkRichardsonLucyDeconvolutionImageFilterGaussianKernelTest.nrrd}
              ${ITK_TEST_OUTPUT_DIR}/itkRichardsonLucyDeconvolutionImageFilterGaussianKernelTest.nrrd
    --ignoreInputInformation
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
itk_add_test(NAME itkRichardsonLucyDeconvolutionImageFilterNonNullImageOriginTest
      COMMAND ITKDeconvolutionTestDriver
    --compare DATA{Baseline/itkRichardsonLucyDeconvolutionImageFilterGaussianKernelNonNullImageOriginTest.nrrd}
      ${ITK_TEST_OUTPUT_DIR}/itkRichardsonLucyDeconvolutionImageFilterNonNullImageOriginTest.nrrd
    itkRichardsonLucyDeconvolutionImageFilterTest
      DATA{Input/itkDeconvolutionImageFilterTestNonNullOriginInput.mha}
      DATA{Input/itkDeconvolutionImageFilterTestKernel.png}
      ${ITK_TEST_OUTPUT_DIR}/itkRichardsonLucyDeconvolutionImageFilterNonNullImageOriginTest.nrrd 1
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
itk_add_test(NAME itkInverseDeconvolutionImageFilterGaussianKernelTest
      COMMAND ITKDeconvolutionTestDriver
    --compare DATA{Baseline/itkInverseDeconvolutionImageFilterGaussianKernelTest.nrrd}
              ${ITK_TEST_OUTPUT_DIR}/itkInverseDeconvolutionImageFilterGaussianKernelTest.nrrd
    itkInverseDeconvolutionImageFilterTest
      DATA{Input/itkDeconvolutionImageFilterTestInput.png}
      DATA{Input/itkDeconvolutionImageFilterTestKernel.png}
      ${ITK_TEST_OUTPUT_DIR}/itkInverseDeconvolutionImageFilterGaussianKernelTest.nrrd
      1
)
itk_add_test(NAME itkInverseDeconvolutionImageFilterIrregularKernelTest
      COMMAND ITKDeconvolutionTestDriver
    --compare DATA{Baseline/itkInverseDeconvolutionImageFilterIrregularKernelTest.nrrd}
              ${ITK_TEST_OUTPUT_DIR}/itkInverseDeconvolutionImageFilterIrregularKernelTest.nrrd
    itkInverseDeconvolutionImageFilterTest
      DATA{Input/itkDeconvolutionImageFilterTestInput.png}
      DATA{Input/itkDeconvolutionImageFilterTestKernelIrregular.tif}
      ${ITK_TEST_OUTPUT_DIR}/itkInverseDeconvolutionImageFilterIrregularKernelTest.nrrd
      1
)
itk_add_test(NAME itkTikhonovDeconvolutionImageFilterGaussianKernelTest
      COMMAND ITKDeconvolutionTestDriver
    --compare DATA{Baseline/itkTikhonovDeconvolutionImageFilterGaussianKernelTest.nrrd}
              ${ITK_TEST_OUTPUT_DIR}/itkTikhonovDeconvolutionImageFilterGaussianKernelTest.nrrd
    itkTikhonovDeconvolutionImageFilterTest
      DATA{Input/itkDeconvolutionImageFilterTestInput.png}
      DATA{Input/itkDeconvolutionImageFilterTestKernel.png}
      ${ITK_TEST_OUTPUT_DIR}/itkTikhonovDeconvolutionImageFilterGaussianKernelTest.nrrd
      1
)
itk_add_test(NAME itkTikhonovDeconvolutionImageFilterIrregularKernelTest
      COMMAND ITKDeconvolutionTestDriver
    --compare DATA{Baseline/itkTikhonovDeconvolutionImageFilterIrregularKernelTest.nrrd}
              ${ITK_TEST_OUTPUT_DIR}/itkTikhonovDeconvolutionImageFilterIrregularKernelTest.nrrd
    itkTikhonovDeconvolutionImageFilterTest
      DATA{Input/itkDeconvolutionImageFilterTestInput.png}
      DATA{Input/itkDeconvolutionImageFilterTestKernelIrregular.tif}
      ${ITK_TEST_OUTPUT_DIR}/itkTikhonovDeconvolutionImageFilterIrregularKernelTest.nrrd
      1
)
itk_add_test(NAME itkWienerDeconvolutionImageFilterGaussianKernelTest
      COMMAND ITKDeconvolutionTestDriver
    --compare DATA{Baseline/itkWienerDeconvolutionImageFilterGaussianKernelTest.nrrd}
              ${ITK_TEST_OUTPUT_DIR}/itkWienerDeconvolutionImageFilterGaussianKernelTest.nrrd
    itkWienerDeconvolutionImageFilterTest
      DATA{Input/itkDeconvolutionImageFilterTestInput.png}
      DATA{Input/itkDeconvolutionImageFilterTestKernel.png}
      ${ITK_TEST_OUTPUT_DIR}/itkWienerDeconvolutionImageFilterGaussianKernelTest.nrrd
      1
)
itk_add_test(NAME itkWienerDeconvolutionImageFilterIrregularKernelTest
      COMMAND ITKDeconvolutionTestDriver
    --compare DATA{Baseline/itkWienerDeconvolutionImageFilterIrregularKernelTest.nrrd}
              ${ITK_TEST_OUTPUT_DIR}/itkWienerDeconvolutionImageFilterIrregularKernelTest.nrrd
    itkWienerDeconvolutionImageFilterTest
      DATA{Input/itkDeconvolutionImageFilterTestInput.png}
      DATA{Input/itkDeconvolutionImageFilterTestKernelIrregular.tif}
      ${ITK_TEST_OUTPUT_DIR}/itkWienerDeconvolutionImageFilterIrregularKernelTest.nrrd
      1
)
itk_add_test(NAME itkParametricBlindLeastSquaresDeconvolutionImageFilterTest
      COMMAND ITKDeconvolutionTestDriver
    --compare DATA{Baseline/itkParametricBlindLeastSquaresDeconvolutionImageFilterTest.nrrd}
              ${ITK_TEST_OUTPUT_DIR}/itkParametricBlindLeastSquaresDeconvolutionImageFilterTest.nrrd
    itkParametricBlindLeastSquaresDeconvolutionImageFilterTest
      DATA{Input/itkDeconvolutionImageFilterTestInput.png}
      ${ITK_TEST_OUTPUT_DIR}/itkParametricBlindLeastSquaresDeconvolutionImageFilterTest.nrrd
      1 1 0.5
      ${ITK_TEST_OUTPUT_DIR}/itkParametricBlindLeastSquaresDeconvolutionImageFilterTestInput.nrrd
)
