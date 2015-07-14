set(DOCUMENTATION "This Module contains helper classes used for the Testing
infrastructure of ITK. As an application developer you should not normally need
any of these classes. However, they are essential for the quality control
system supporting ITK.")

itk_module(ITKTestKernel
  COMPILE_DEPENDS
    ITKIOBioRad
    ITKIOBMP
    ITKIOGDCM
    ITKIOGIPL
    ITKIOJPEG
    ITKIOLSM
    ITKIOMeta
    ITKIONIFTI
    ITKIONRRD
    ITKIOPNG
    ITKIOStimulate
    ITKIOTIFF
    ITKIOVTK
    ITKKWSys
    ITKDoubleConversion
  TEST_DEPENDS
    ITKImageStatistics
  DESCRIPTION
    "${DOCUMENTATION}"
)
