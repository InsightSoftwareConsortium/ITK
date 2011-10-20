itk_module_test()
set(ITKIOTransformHDF5Tests
itkIOTransformHDF5Test.cxx
)

CreateTestDriver(ITKIOTransformHDF5 "${ITKIOTransformHDF5-Test_LIBRARIES}" "${ITKIOTransformHDF5Tests}")

itk_add_test(NAME itkIOTransformHDF5Test
      COMMAND ITKIOTransformHDF5TestDriver itkIOTransformHDF5Test)
