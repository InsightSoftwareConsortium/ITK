set(
  DOCUMENTATION
  "This module contains the third party <a
href=\"http://sourceforge.net/projects/gdcm/\">GDCM</a> library.
Grassroots DiCoM is a C++ library for DICOM medical files."
)

if(ITK_USE_SYSTEM_GDCM)
  itk_module(
    ITKGDCM
    DESCRIPTION "${DOCUMENTATION}"
    EXCLUDE_FROM_DEFAULT
    SPDX_LICENSE "BSD-3-Clause"
    SPDX_DOWNLOAD_LOCATION "https://gdcm.sourceforge.net"
    SPDX_COPYRIGHT "Copyright GDCM contributors"
  )
else()
  itk_module(
    ITKGDCM
    DEPENDS
      ITKZLIB
      ITKExpat
      ITKOpenJPEG
    DESCRIPTION "${DOCUMENTATION}"
    SPDX_LICENSE "BSD-3-Clause"
    SPDX_DOWNLOAD_LOCATION "https://gdcm.sourceforge.net"
    SPDX_COPYRIGHT "Copyright GDCM contributors"
  )
endif()
