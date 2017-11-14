set(DOCUMENTATION "This module contains ImageIO classes for reading medical
images produced by General Electric (GE) scanners.  In particular, it has
classes for the GE4, GE5, and GEAdw scanners.")

itk_module(ITKIOGE
  ENABLE_SHARED
  DEPENDS
    ITKIOIPL
  PRIVATE_DEPENDS
    ITKIOImageBase
  TEST_DEPENDS
    ITKTestKernel
    ITKIOIPL
    ITKIOSiemens
  FACTORY_NAMES
    ImageIO::GE4
    ImageIO::GE5
  DESCRIPTION
    "${DOCUMENTATION}"
)
