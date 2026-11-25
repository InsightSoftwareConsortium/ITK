set(
  DOCUMENTATION
  "This Module contains helper classes used for the Testing
infrastructure of ITK. As an application developer you should not normally need
any of these classes. However, they are essential for the quality control
system supporting ITK."
)

# Python wrapping tests run under itkTestDriver and use the NumPy bridge.
if(ITK_WRAP_PYTHON)
  set(_itk_test_kernel_python_depends ITKBridgeNumPy)
endif()

itk_module(
  ITKTestKernel
  DEPENDS
    ITKCommon
    ITKIOImageBase
  PRIVATE_DEPENDS
    ITKIOBMP
    ITKIOGDCM
    ITKIOGIPL
    ITKIOJPEG
    ITKIOMeta
    ITKIONIFTI
    ITKIONRRD
    ITKIOPNG
    ITKIOTIFF
    ITKIOVTK
    ITKIOMeshVTK
    ITKIOMeshBYU
    ITKIOMeshFreeSurfer
    ITKIOMeshGifti
    ITKIOMeshOBJ
    ITKIOMeshOFF
    ITKFFT
    ${_itk_test_kernel_python_depends}
  COMPILE_DEPENDS
    ITKKWSys
    ITKDoubleConversion
  TEST_DEPENDS
    ITKImageStatistics
    ITKGoogleTest
  DESCRIPTION "${DOCUMENTATION}"
)
