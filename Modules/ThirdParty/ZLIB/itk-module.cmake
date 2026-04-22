set(
  DOCUMENTATION
  "This module contains the third party <a
href=\"https://github.com/zlib-ng/zlib-ng\">zlib-ng</a>
general purpose data compression library,
designed as a drop-in replacement for ZLIB."
)

itk_module(
  ITKZLIB
  DESCRIPTION "${DOCUMENTATION}"
  SPDX_LICENSE
  "Zlib"
  SPDX_VERSION
  "2.2.5"
  SPDX_DOWNLOAD_LOCATION
  "https://github.com/zlib-ng/zlib-ng"
  SPDX_COPYRIGHT
  "Copyright zlib-ng contributors"
  SPDX_PURL
  "pkg:github/zlib-ng/zlib-ng@2.2.5"
)
