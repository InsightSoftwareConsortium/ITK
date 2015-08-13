set(DOCUMENTATION "This module contain ITK ImageIO classes for the <a
href=\"http://sourceforge.net/projects/gdcm/\">Grass Root DICOM (GDCM)</a> based
readers and writers of the medical imaging DICOM standard.")

itk_module(ITKIOGDCM
  ENABLE_SHARED
  PRIVATE_DEPENDS
    ITKGDCM
    ITKIOImageBase
  TEST_DEPENDS
    ITKTestKernel
    ITKGDCM
    ITKImageIntensity
  DESCRIPTION
    "${DOCUMENTATION}"
)
