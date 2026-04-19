set(
  DOCUMENTATION
  "This module contains the third party <a
href=\"http://www.libtiff.org/\">Tag Image File Format (TIFF)</a>
image file format library."
)

itk_module(
  ITKTIFF
  DEPENDS
    ITKZLIB
    ITKJPEG
  DESCRIPTION "${DOCUMENTATION}"
  SPDX_LICENSE
  "libtiff"
  SPDX_VERSION
  "4.7.0"
  SPDX_DOWNLOAD_LOCATION
  "https://libtiff.maptools.org"
  SPDX_COPYRIGHT
  "Copyright libtiff contributors"
  SPDX_PURL
  "pkg:generic/libtiff@4.7.0"
)
