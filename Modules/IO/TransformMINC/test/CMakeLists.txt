itk_module_test()

if(NOT ITK_USE_SYSTEM_HDF5)
  include_directories(
    "${ITKHDF5_SOURCE_DIR}/src/itkhdf5"
    "${ITKHDF5_SOURCE_DIR}/src/itkhdf5/src"
    "${ITKHDF5_BINARY_DIR}/src/itkhdf5"
    )
endif()

set(ITKIOTransformMINCTests
    itkIOTransformMINCTest.cxx
    itkMINCTransformAdapterTest.cxx
 )

CreateTestDriver(ITKIOTransformMINC "${ITKIOTransformMINC-Test_LIBRARIES}" "${ITKIOTransformMINCTests}")
  TARGET_LINK_LIBRARIES(ITKIOTransformMINCTestDriver LINK_PUBLIC ITKIOTransformBase )

itk_add_test(NAME itkIOTransformMINCTest
      COMMAND ITKIOTransformMINCTestDriver itkIOTransformMINCTest)

itk_add_test(NAME itkMINCTransformAdapterTest
      COMMAND ITKIOTransformMINCTestDriver itkMINCTransformAdapterTest)
