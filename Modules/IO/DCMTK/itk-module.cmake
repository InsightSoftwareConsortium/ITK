set(DOCUMENTATION "This module contains the third party <a
href=\"http://dicom.offis.de/dcmtk/\">DCMTK</a> DCMTK is a collection of libraries and applications implementing large parts the DICOM standard.")

if(MSVC)
  set(ITKIODCMTK_EXCLUDE_FROM_ALL "EXCLUDE_FROM_ALL")
else()
  set(ITKIODCMTK_EXCLUDE_FROM_ALL "")
endif()

itk_module(ITKIODCMTK
  DEPENDS
    ITKDCMTK
    ITKIOImageBase
  TEST_DEPENDS
    ITKTestKernel
    ITKImageIntensity
  DESCRIPTION
    "${DOCUMENTATION}"
  ${ITKIODCMTK_EXCLUDE_FROM_ALL}
)
