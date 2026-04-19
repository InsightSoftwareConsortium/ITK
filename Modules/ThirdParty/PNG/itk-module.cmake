set(
  DOCUMENTATION
  "This module contains the third party <a
href=\"http://www.libpng.org/pub/png/libpng.html/\">Portable Network Graphics
(PNG)</a> image file format library."
)

itk_module(
  ITKPNG
  DEPENDS
    ITKZLIB
  DESCRIPTION "${DOCUMENTATION}"
  SPDX_LICENSE
  "Libpng"
  SPDX_VERSION
  "1.6.54"
  SPDX_DOWNLOAD_LOCATION
  "https://www.libpng.org/pub/png/libpng.html"
  SPDX_COPYRIGHT
  "Copyright libpng contributors"
  SPDX_PURL
  "pkg:generic/libpng@1.6.54"
)
