set(DOCUMENTATION "This Module contains helper classes used for the Testing
infrastructure of ITK. As an application developer you should not normally need
any of these classes. However, they are essential for the quality control
system supporting ITK.")

# ITKIOLSM uses custom TIFF flags, which need a version of libtiff that
# is not installed.
if(ITK_USE_SYSTEM_TIFF)
  set(lsm_depends "")
else()
  set(lsm_depends "ITKIOLSM")
endif()

itk_module(ITKTestKernel
  DEPENDS
    ITKIOBioRad
    ITKIOBMP
    ITKIOGDCM
    ITKIOGIPL
    ITKIOJPEG
    ${lsm_depends}
    ITKIOMeta
    ITKIONIFTI
    ITKIONRRD
    ITKIOPNG
    ITKIOStimulate
    ITKIOTIFF
    ITKIOVTK
    ITKKWSys
    ITKDoubleConversion
  DESCRIPTION
    "${DOCUMENTATION}"
)
