itk_module_test()

set(MultipleImageIteratorTests DumpIntensities.cxx)

createtestdriver(MultipleImageIterator "${MultipleImageIterator_LIBRARIES}" "${MultipleImageIteratorTests}")

itk_add_test(
  NAME MultipleImageIteratorTest
  COMMAND
    MultipleImageIteratorTestDriver
    --compare
    DATA{randBase.nrrd}
    "${ITK_TEST_OUTPUT_DIR}/randOut.nrrd"
    DumpIntensities
    "${ITK_TEST_OUTPUT_DIR}/randOut.nrrd"
    DATA{img1.png}
    DATA{img2.png}
    DATA{img3.png}
)
