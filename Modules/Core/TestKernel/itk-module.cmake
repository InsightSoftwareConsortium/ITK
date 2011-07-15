set(DOCUMENTATION "This Module contains helper classes used for the Testing
infrastructure of ITK. As an application developer you should not normally need
any of these classes. However, they are essential for the quality control
system supporting ITK.")

itk_module(ITKTestKernel
  DEPENDS
    ITKIOPNG
    ITKIOMeta
    ITKIOTIFF
    ITKIOBMP
    ITKIOVTK
    ITKIOJPEG
    ITKIONRRD
    ITKIOGDCM
    ITKIOGIPL
    ITKIONIFTI
    ITKKWSys
  DESCRIPTION
    "${DOCUMENTATION}"
)
