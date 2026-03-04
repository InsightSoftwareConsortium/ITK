set(
  DOCUMENTATION
  "This module contains the third party <a
href=\"http://www.nitrc.org/projects/gifti/\">GIFTI</a> library.
Geometry format under the Neuroimaging Informatics Technology Initiative"
)

itk_module(
  ITKGIFTI
  DEPENDS
    ITKZLIB
    ITKExpat
    ITKNIFTI
  DESCRIPTION "${DOCUMENTATION}"
  SPDX_LICENSE "LicenseRef-NITRC-Public-Domain"
  SPDX_DOWNLOAD_LOCATION "https://www.nitrc.org/projects/gifti"
  SPDX_COPYRIGHT "NOASSERTION"
  SPDX_CUSTOM_LICENSE_NAME "NITRC GIFTI Public Domain License"
  SPDX_CUSTOM_LICENSE_TEXT "The GIFTI library is released into the public domain under the NITRC project."
)
