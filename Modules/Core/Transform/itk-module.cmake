set(DOCUMENTATION "Transforms are essential components of image registration
framework in ITK. They are typically used for representing the mapping between
the physical coordinate system of one image and the physical coordinate system
of another image. They are also commonly used in the process of resampling
images, particulaly when mapping them between coordinate systems. Transforms
are a large family in ITK and form a prolific group of classes in the
toolkit.")

itk_module(ITKTransform
  ENABLE_SHARED
  DEPENDS
    ITKCommon
  COMPILE_DEPENDS
    ITKStatistics
    ITKImageFilterBase
  TEST_DEPENDS
    ITKTestKernel
    ITKDisplacementField
  DESCRIPTION
    "${DOCUMENTATION}"
)

# Extra dependencies on ImageFilterBase introduced by itkGaussianSmoothingOnUpdateDisplacementFieldTransform.
# Extra test dependencies on ImageFunction and ImageGrid are introduced by itkBSplineDeformableTransformTest.
# Extra test dependency on  SpatialObjects is introduced by itkCenteredVersorTransformInitializerTest.
