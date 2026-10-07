itk_module_test()

set(StructuralSimilarityGTests itkStructuralSimilarityImageFilterGTest.cxx)

creategoogletestdriver(StructuralSimilarity "${StructuralSimilarity-Test_LIBRARIES}" "${StructuralSimilarityGTests}")

ExternalData_Expand_Arguments(
  ITKData
  _input_dir
  "DATA{${ITK_DATA_ROOT}/Input/,cthead1.png,cthead1.jpg,cake_easy.png,cake_hard.png,smooth_circle.png,smooth_square.png,STAPLE1.png,STAPLE2.png,Number1inText.png,Number2inText.png}"
)
ExternalData_Expand_Arguments(
  ITKData
  _baseline_dir
  "DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/,ConnectedComponentImageFilterTest.png,GrayscaleMorphologicalClosingImageFilterTest.png,InvertIntensityImageFilterTest.png}"
)
cmake_path(GET _input_dir PARENT_PATH _data_root)
target_compile_definitions(
  StructuralSimilarityGTestDriver
  PRIVATE
    "ITK_DATA_ROOT=\"${_data_root}\""
)
