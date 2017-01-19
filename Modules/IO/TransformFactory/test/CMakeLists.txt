itk_module_test()
set(ITKTransformFactoryTests
itkTransformFactoryBaseTest.cxx
)

add_definitions("-DITK_TRANSFORM_FACTORY_MAX_DIM=${ITK_TRANSFORM_FACTORY_MAX_DIM}")
CreateTestDriver(ITKTransformFactory "${ITKTransformFactory-Test_LIBRARIES}" "${ITKTransformFactoryTests}")

itk_add_test(NAME itkTransformFactoryBaseTest
      COMMAND ITKTransformFactoryTestDriver itkTransformFactoryBaseTest)
