set(DOCUMENTATION "This modules provides methods to convert wrapped
itk::Image's to NumPy arrays and vice versa.")

if(ITK_WRAP_PYTHON)
  set(_exclude_from_default )
else()
  set(_exclude_from_default EXCLUDE_FROM_DEFAULT)
endif()

itk_module(ITKBridgeNumPy
  DEPENDS
    ITKCommon
  TEST_DEPENDS
    ITKTestKernel
  ${_exclude_from_default}
  DESCRIPTION
    "${DOCUMENTATION}"
  )
