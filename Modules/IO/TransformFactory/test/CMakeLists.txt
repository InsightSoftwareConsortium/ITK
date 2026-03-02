itk_module_test()
set(ITKTransformFactoryTests itkTransformFactoryBaseTest.cxx)

createtestdriver(ITKTransformFactory "${ITKTransformFactory-Test_LIBRARIES}" "${ITKTransformFactoryTests}")
target_compile_definitions(
  ITKTransformFactoryTestDriver
  PRIVATE
    "ITK_TRANSFORM_FACTORY_MAX_DIM=${ITK_TRANSFORM_FACTORY_MAX_DIM}"
)

itk_add_test(
  NAME itkTransformFactoryBaseTest
  COMMAND
    ITKTransformFactoryTestDriver
    itkTransformFactoryBaseTest
)
