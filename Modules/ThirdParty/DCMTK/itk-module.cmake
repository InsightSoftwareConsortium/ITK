set(DOCUMENTATION "This module builds the DCMTK library <a
href=\"http://dicom.offis.de/dcmtk.php.en\">DCMTK</a> DICOM
library suite.")

if(WIN32)
  set(ITKDCMTK_EXCLUDE_FROM_ALL "EXCLUDE_FROM_ALL")
else()
  set(ITKDCMTK_EXCLUDE_FROM_ALL "")
endif()

itk_module(ITKDCMTK
  DEPENDS
    ITKZLIB
    ITKJPEG
    ITKTIFF
  DESCRIPTION
    "${DOCUMENTATION}"
  ${ITKDCMTK_EXCLUDE_FROM_ALL}
)
