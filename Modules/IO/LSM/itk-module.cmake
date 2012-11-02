set(DOCUMENTATION "This module contains an ImageIO class for reading LSM (Zeiss)
images LSM is a line of confocal laser scanning microscopes produced by the
Zeiss company. LSM files are essentially extensions of the TIFF multiple image
stack file format.")

# ITKIOLSM uses custom TIFF flags, which need a version of libtiff that
# is not installed.
if(ITK_USE_SYSTEM_TIFF)
  set(exclude EXCLUDE_FROM_ALL)
  if(Module_ITKIOLSM)
    message(SEND_ERROR "ITKIOLSM cannot be build with ITK_USE_SYSTEM_TIFF.")
  endif()
else()
  set(exclude "")
endif()

itk_module(ITKIOLSM
  DEPENDS
    ITKIOTIFF
  TEST_DEPENDS
    ITKTestKernel
  ${exclude}
  DESCRIPTION
    "${DOCUMENTATION}"
)
