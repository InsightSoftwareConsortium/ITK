set(DOCUMENTATION "This module contains an ImageIO class for reading LSM (Zeiss)
images LSM is a line of confocal laser scanning microscopes produced by the
Zeiss company. LSM files are essentially extensions of the TIFF multiple image
stack file format.")

itk_module(ITKIOLSM
  ENABLE_SHARED
  DEPENDS
    ITKIOTIFF
  TEST_DEPENDS
    ITKTestKernel
  DESCRIPTION
    "${DOCUMENTATION}"
)
