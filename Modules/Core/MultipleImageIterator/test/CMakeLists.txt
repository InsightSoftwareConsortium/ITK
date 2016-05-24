itk_module_test()

set(MultipleImageIteratorTests DumpIntensities.cxx)

createtestdriver(MultipleImageIterator "${MultipleImageIterator_LIBRARIES}" "${MultipleImageIteratorTests}")

itk_add_test(
  NAME MultipleImageIteratorTest
  COMMAND
    MultipleImageIteratorTestDriver
    --compare
    ${CMAKE_SOURCE_DIR}/test/randImage.nrrd
    randImage.nrrd
    DumpIntensities
    randImage.nrrd
    ${CMAKE_SOURCE_DIR}/test/img1.png
    ${CMAKE_SOURCE_DIR}/test/img2.png
    ${CMAKE_SOURCE_DIR}/test/img3.png
)
