itk_module(ITK-Transform DEPENDS ITK-ImageStatistics TEST_DEPENDS ITK-TestKernel ITK-ImageFunction ITK-ImageGrid ITK-SpatialObjects)
# Extra test depedency on ImageFunction and ImageGrid is introduced by itkBSplineDeformableTransformTest.
# Extra test dependency on  SpatialObjects is introduced by itkCenteredVersorTransformInitializerTest.
