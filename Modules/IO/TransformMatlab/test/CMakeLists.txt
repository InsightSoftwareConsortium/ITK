itk_module_test()

set(ITKIOTransformMatlabGTests itkIOTransformMatlabGTest.cxx)

creategoogletestdriver(ITKIOTransformMatlab "${ITKIOTransformMatlab-Test_LIBRARIES}" "${ITKIOTransformMatlabGTests}")
