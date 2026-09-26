itk_module_test()

set(StructuralSimilarityGTests itkStructuralSimilarityImageFilterGTest.cxx)

creategoogletestdriver(StructuralSimilarity "${StructuralSimilarity-Test_LIBRARIES}" "${StructuralSimilarityGTests}")

# <macro>=<file under ITK_DATA_ROOT>; each macro expands to the local path of the file.
set(
  _structural_similarity_test_images
  "CTHEAD1_PNG_INPUT=Input/cthead1.png"
  "CTHEAD1_JPG_INPUT=Input/cthead1.jpg"
  "CTHEAD1_PNG_CC=Baseline/BasicFilters/ConnectedComponentImageFilterTest.png"
  "CTHEAD1_CLOSING_PNG=Baseline/BasicFilters/GrayscaleMorphologicalClosingImageFilterTest.png"
  "CTHEAD1_INVERTED_PNG=Baseline/BasicFilters/InvertIntensityImageFilterTest.png"
  "CAKE_EASY_PNG=Input/cake_easy.png"
  "CAKE_HARD_PNG=Input/cake_hard.png"
  "SMOOTH_CIRCLE_PNG=Input/smooth_circle.png"
  "SMOOTH_SQUARE_PNG=Input/smooth_square.png"
  "STAPLE1_PNG=Input/STAPLE1.png"
  "STAPLE2_PNG=Input/STAPLE2.png"
  "NUMBER1_PNG=Input/Number1inText.png"
  "NUMBER2_PNG=Input/Number2inText.png"
)
foreach(_image IN LISTS _structural_similarity_test_images)
  string(REPLACE "=" ";" _image "${_image}")
  list(GET _image 0 _macro)
  list(GET _image 1 _file)
  ExternalData_Expand_Arguments(
    ITKData
    _path
    "DATA{${ITK_DATA_ROOT}/${_file}}"
  )
  target_compile_definitions(
    StructuralSimilarityGTestDriver
    PRIVATE
      "${_macro}=${_path}"
  )
endforeach()
