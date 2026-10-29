set(
  DOCUMENTATION
  "This module contains filters for computing and manipulating
deformation fields. In ITK a deformation field is usually represented as an
image of vectors. That is, every pixel contains a vector. In this module you
will find filters for: computing a deformation field from a transform,
computing inverse deformation fields, and computing the Jacobian Determinant of
a deformation field."
)

# Python wrapping tests use the NumPy bridge.
if(ITK_WRAP_PYTHON)
  set(_numpy_test_depends ITKBridgeNumPy)
endif()

itk_module(
  ITKDisplacementField
  DEPENDS
    ITKSmoothing
    ITKImageIntensity
  COMPILE_DEPENDS
    ITKImageGrid
  TEST_DEPENDS
    ITKTestKernel
    ${_numpy_test_depends}
    ITKGoogleTest
  DESCRIPTION "${DOCUMENTATION}"
)
