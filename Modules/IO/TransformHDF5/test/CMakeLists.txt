itk_module_test()
set(ITKIOTransformHDF5Tests
itkIOTransformHDF5Test.cxx
itkThinPlateTransformWriteReadTest.cxx
)

CreateTestDriver(ITKIOTransformHDF5 "${ITKIOTransformHDF5-Test_LIBRARIES}" "${ITKIOTransformHDF5Tests}")

itk_add_test(NAME itkIOTransformHDF5Test
      COMMAND ITKIOTransformHDF5TestDriver itkIOTransformHDF5Test uncompressed)

itk_add_test(NAME itkIOTransformHDF5TestCompressed
        COMMAND ITKIOTransformHDF5TestDriver itkIOTransformHDF5Test compressed)

itk_add_test(NAME itkThinPlateTransformWriteReadTest
      COMMAND ITKIOTransformHDF5TestDriver itkThinPlateTransformWriteReadTest ${ITK_TEST_OUTPUT_DIR})
