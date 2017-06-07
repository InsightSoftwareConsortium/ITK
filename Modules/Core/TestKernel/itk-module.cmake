set(DOCUMENTATION "This Module contains helper classes used for the Testing
infrastructure of ITK. As an application developer you should not normally need
any of these classes. However, they are essential for the quality control
system supporting ITK.")

itk_module(ITKTestKernel
  DEPENDS
    ITKIOImageBase
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
  PRIVATE_DEPENDS
    ITKCommon
  COMPILE_DEPENDS
    ITKKWSys
    ITKDoubleConversion
  TEST_DEPENDS
    ITKImageStatistics
    ITKGoogleTest
  DESCRIPTION
    "${DOCUMENTATION}"
)
