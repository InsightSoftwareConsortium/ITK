set(DOCUMENTATION "This module contains ITK ImageIO classes for the <a
href=\"http://sourceforge.net/projects/gdcm/\">Grassroots DICOM (GDCM)</a> based
readers and writers of the medical imaging DICOM standard.")

itk_module(ITKIOGDCM
  ENABLE_SHARED
  DEPENDS
    ITKCommon
    ITKIOImageBase
  PRIVATE_DEPENDS
    ITKGDCM
  TEST_DEPENDS
    ITKTestKernel
    ITKGDCM
    ITKImageIntensity
  FACTORY_NAMES
    ImageIO::GDCM
  DESCRIPTION
    "${DOCUMENTATION}"
)
