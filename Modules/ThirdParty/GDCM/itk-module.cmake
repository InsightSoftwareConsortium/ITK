set(DOCUMENTATION "This module contains the third party <a
href=\"http://sourceforge.net/projects/gdcm/\">GDCM</a> library.
Grassroots DiCoM is a C++ library for DICOM medical files.")

itk_module(ITKGDCM
  DEPENDS
    ITKZLIB
    ITKExpat
    ITKOpenJPEG
  DESCRIPTION
    "${DOCUMENTATION}"
)
