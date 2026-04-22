set(
  DOCUMENTATION
  "This module contains the third party <a
href=\"http://niftilib.sourceforge.net/\">NIFTI</a> library.
Neuroimaging Informatics Technology Initiative provides an Analyze-style MRI
file format."
)

itk_module(
  ITKNIFTI
  DEPENDS
    ITKZLIB
  DESCRIPTION "${DOCUMENTATION}"
  SPDX_LICENSE
  "LicenseRef-NIFTI-Public-Domain"
  SPDX_VERSION
  "3.0.0"
  SPDX_DOWNLOAD_LOCATION
  "https://github.com/NIFTI-Imaging/nifti_clib"
  SPDX_COPYRIGHT
  "NOASSERTION"
  SPDX_CUSTOM_LICENSE_NAME
  "NIFTI Public Domain License"
  SPDX_CUSTOM_LICENSE_TEXT
  "This software is in the public domain. The NIFTI header and library are released into the public domain."
  SPDX_PURL
  "pkg:github/NIFTI-Imaging/nifti_clib@v3.0.0"
)
