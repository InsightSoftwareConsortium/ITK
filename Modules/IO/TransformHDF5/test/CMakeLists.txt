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

# A test to read transform file that was written before v5.0a02 when the internal paths were incorrect
itk_add_test(NAME itkReadOldHDF5MisspelledPathTest
        COMMAND ITKIOTransformHDF5TestDriver itkIOTransformHDF5Test DATA{${ITK_DATA_ROOT}/Input/historical_misspelled_TranformParameters.h5})
