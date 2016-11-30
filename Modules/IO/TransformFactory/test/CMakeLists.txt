itk_module_test()
set(ITKTransformFactoryTests
itkTransformFactoryBaseTest.cxx
)

CreateTestDriver(ITKTransformFactory "${ITKTransformFactory-Test_LIBRARIES}" "${ITKTransformFactoryTests}")

itk_add_test(NAME itkTransformFactoryBaseTest
      COMMAND ITKTransformFactoryTestDriver itkTransformFactoryBaseTest)

target_compile_definitions(ITKTransformFactoryTestDriver PRIVATE ITK_TRANSFORM_FACTORY_MAX_DIM=${ITK_TRANSFORM_FACTORY_MAX_DIM})
