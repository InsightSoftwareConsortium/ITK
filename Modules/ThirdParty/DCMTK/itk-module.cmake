set(DOCUMENTATION "This module builds the DCMTK library <a
href=\"http://dicom.offis.de/dcmtk.php.en\">DCMTK</a> DICOM
library suite.")

itk_module(ITKDCMTK
  DEPENDS
    ITKZLIB
    ITKJPEG
    ITKTIFF
  DESCRIPTION
    "${DOCUMENTATION}"
  EXCLUDE_FROM_ALL
)
