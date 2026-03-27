set(
  DOCUMENTATION
  "This module contains the third party <a
href=\"http://teem.sourceforge.net/nrrd/lib.html\">NRRD</a> image file format."
)

itk_module(
  ITKNrrdIO
  DEPENDS ITKZLIB
  DESCRIPTION "${DOCUMENTATION}"
  SPDX_LICENSE "LGPL-2.1-only"
  SPDX_DOWNLOAD_LOCATION "https://teem.sourceforge.net/nrrd"
  SPDX_COPYRIGHT "Copyright Teem contributors"
)
