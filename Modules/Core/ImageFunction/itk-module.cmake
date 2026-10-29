set(
  DOCUMENTATION
  "ImageFunctions are typically used as helper classes in the
implementation of ITK filters. In many cases they may act as pluggable
components at run time, while in other cases they may be implementing the
actions that a filter will apply at every iteration, or the operation a
filter may apply to every pixel. These classes are rarely used from
applications. Their main role is to assist in the implementation of ITK
filters."
)

# Python wrapping tests use the NumPy bridge.
if(ITK_WRAP_PYTHON)
  set(_numpy_test_depends ITKBridgeNumPy)
endif()

itk_module(
  ITKImageFunction
  COMPILE_DEPENDS
    ITKImageAdaptors
    ITKTransform
    ITKStatistics
  TEST_DEPENDS
    ITKGoogleTest
    ITKTestKernel
    ${_numpy_test_depends}
  DESCRIPTION "${DOCUMENTATION}"
)
