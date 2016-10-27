itk_module_test()

Set(${itk-module}Tests
  itkAdditiveGaussianNoiseMeshFilterTest.cxx
  itkAdditiveGaussianNoiseQuadEdgeMeshFilterTest.cxx
)

CreateTestDriver(${itk-module} "${${itk-module}-Test_LIBRARIES}" "${${itk-module}Tests}")

itk_add_test(NAME itkAdditiveGaussianNoiseMeshFilterTest
  COMMAND ${itk-module}TestDriver itkAdditiveGaussianNoiseMeshFilterTest )
itk_add_test(NAME itkAdditiveGaussianNoiseQuadEdgeMeshFilterTest
  COMMAND ${itk-module}TestDriver itkAdditiveGaussianNoiseQuadEdgeMeshFilterTest )
