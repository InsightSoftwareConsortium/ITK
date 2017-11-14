set(DOCUMENTATION "This module contains the third party <a
href=\"http://dicom.offis.de/dcmtk/\">DCMTK</a> DCMTK is a collection of libraries and applications implementing large parts the DICOM standard.")

itk_module(ITKIODCMTK
  ENABLE_SHARED
  PRIVATE_DEPENDS
    ITKDCMTK
    ITKIOImageBase
  TEST_DEPENDS
    ITKTestKernel
    ITKImageIntensity
  FACTORY_NAMES
    ImageIO::DCMTK
  DESCRIPTION
    "${DOCUMENTATION}"
  EXCLUDE_FROM_DEFAULT
)
