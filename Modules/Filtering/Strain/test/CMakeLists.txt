itk_module_test()

set(StrainTests itkStrainImageFilterTest.cxx)

createtestdriver(Strain "${Strain-Test_LIBRARIES}" "${StrainTests}")
