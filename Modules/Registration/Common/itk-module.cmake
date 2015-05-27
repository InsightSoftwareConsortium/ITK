set(DOCUMENTATION "This module contains classes to perform registration within
the default ITK registration framework.  A moving image is registered against a
fixed image by optimizing the parameters of a transform.  This module contains
metrics to compare the fixed and moving image and classes to coordinate the
registration procedure.  There are classes to perform multi-resolution image
registration and also classes to registrations other that image-to-image
registrations, e.g. point set-to-image  or point set-to-point set
registrations.  Transforms used in the registration can be found in \\\\ref
ITKTransform, and optimizers can be found in \\\\ref ITKOptimizers.  To compare
the moving image to the fixed image with the image metric, an interpolator is
required-- these can be found in \\\\ref ITKImageFunction.")

itk_module(ITKRegistrationCommon
  DEPENDS
    ITKOptimizers
    ITKImageIntensity
    ITKImageFunction
    ITKImageGrid
    ITKSpatialObjects
    ITKSmoothing
    ITKImageGradient
    ITKImageFeature
    ITKFiniteDifference
    ITKDisplacementField
    ITKStatistics
  TEST_DEPENDS
    ITKTestKernel
    ITKDistanceMap
    ITKImageSources
    ITKColormap
    ITKQuadEdgeMesh
  DESCRIPTION
    "${DOCUMENTATION}"
)

# Extra test dependency on ITKDistanceMap is introduced by itkPointSetToPointSetRegistrationTest.
# Dependency on ITKStatistics is introduced by itkPointsLocator.
