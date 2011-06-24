itk_module_test()
set(ITK-ImageFilterBaseTests
itkImageFilterBaseHeaderTest.cxx
itkNeighborhoodOperatorImageFilterTest.cxx
itkImageToImageFilterTest.cxx
itkVectorNeighborhoodOperatorImageFilterTest.cxx
itkGaussianImageSourceTest.cxx
itkMaskNeighborhoodOperatorImageFilterTest.cxx
itkCastImageFilterTest.cxx
itkClampImageFilterTest.cxx
)

# Disable optimization on the tests below to avoid possible
# optimization bugs in some compilers. This option will appear on the
# command line after any other optimization flag (including those from
# CMAKE_CXX_FLAGS) which means that on gcc, MSVC, and the Intel
# compiler, it will override any other optimization flag.
if (DEFINED ITK_CXX_DISABLE_OPTIMIZATION_FLAG)
  set_source_files_properties(
    itkCastImageFilterTest.cxx
    itkClampImageFilterTest.cxx
    PROPERTIES COMPILE_FLAGS ${ITK_CXX_DISABLE_OPTIMIZATION_FLAG}
  )
endif()


CreateTestDriver(ITK-ImageFilterBase  "${ITK-ImageFilterBase-Test_LIBRARIES}" "${ITK-ImageFilterBaseTests}")

itk_add_test(NAME itkImageFilterBaseHeaderTest
      COMMAND ITK-ImageFilterBaseTestDriver itkImageFilterBaseHeaderTest)
itk_add_test(NAME itkNeighborhoodOperatorImageFilterTest
      COMMAND ITK-ImageFilterBaseTestDriver itkNeighborhoodOperatorImageFilterTest)
itk_add_test(NAME itkImageToImageFilterTest
      COMMAND ITK-ImageFilterBaseTestDriver itkImageToImageFilterTest)
itk_add_test(NAME itkVectorNeighborhoodOperatorImageFilterTest
      COMMAND ITK-ImageFilterBaseTestDriver itkVectorNeighborhoodOperatorImageFilterTest)
itk_add_test(NAME itkGaussianImageSourceTest
      COMMAND ITK-ImageFilterBaseTestDriver itkGaussianImageSourceTest)
itk_add_test(NAME itkMaskNeighborhoodOperatorImageFilterTest
      COMMAND ITK-ImageFilterBaseTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/MaskNeighborhoodOperatorImageFilterTest.png
              ${ITK_TEST_OUTPUT_DIR}/MaskNeighborhoodOperatorImageFilterTest.png
    itkMaskNeighborhoodOperatorImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/MaskNeighborhoodOperatorImageFilterTest.png)
itk_add_test(NAME itkCastImageFilterTest
      COMMAND ITK-ImageFilterBaseTestDriver itkCastImageFilterTest)
itk_add_test(NAME itkClampImageFilterTest
      COMMAND ITK-ImageFilterBaseTestDriver itkClampImageFilterTest)
