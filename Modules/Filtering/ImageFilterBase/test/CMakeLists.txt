itk_module_test()
set(ITKImageFilterBaseTests
itkNeighborhoodOperatorImageFilterTest.cxx
itkImageToImageFilterTest.cxx
itkVectorNeighborhoodOperatorImageFilterTest.cxx
itkMaskNeighborhoodOperatorImageFilterTest.cxx
itkCastImageFilterTest.cxx
)

# Disable optimization on the tests below to avoid possible
# optimization bugs in some compilers. This option will appear on the
# command line after any other optimization flag (including those from
# CMAKE_CXX_FLAGS) which means that on gcc, MSVC, and the Intel
# compiler, it will override any other optimization flag.
if (DEFINED ITK_CXX_DISABLE_OPTIMIZATION_FLAG)
  set_source_files_properties(
    itkCastImageFilterTest.cxx
    PROPERTIES COMPILE_FLAGS ${ITK_CXX_DISABLE_OPTIMIZATION_FLAG}
  )
endif()


CreateTestDriver(ITKImageFilterBase  "${ITKImageFilterBase-Test_LIBRARIES}" "${ITKImageFilterBaseTests}")

itk_add_test(NAME itkNeighborhoodOperatorImageFilterTest
      COMMAND ITKImageFilterBaseTestDriver itkNeighborhoodOperatorImageFilterTest)
itk_add_test(NAME itkImageToImageFilterTest
      COMMAND ITKImageFilterBaseTestDriver itkImageToImageFilterTest)
itk_add_test(NAME itkVectorNeighborhoodOperatorImageFilterTest
      COMMAND ITKImageFilterBaseTestDriver itkVectorNeighborhoodOperatorImageFilterTest)
itk_add_test(NAME itkMaskNeighborhoodOperatorImageFilterTest
      COMMAND ITKImageFilterBaseTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/MaskNeighborhoodOperatorImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/MaskNeighborhoodOperatorImageFilterTest.png
    itkMaskNeighborhoodOperatorImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/MaskNeighborhoodOperatorImageFilterTest.png)
itk_add_test(NAME itkCastImageFilterTest
      COMMAND ITKImageFilterBaseTestDriver itkCastImageFilterTest)
