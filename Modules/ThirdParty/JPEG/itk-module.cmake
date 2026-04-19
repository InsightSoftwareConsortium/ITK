set(
  DOCUMENTATION
  "This module contains the third party JPEG
library published by the
<a href=\"http://www.ijg.org/\">Independent JPEG Group</a> and libjpeg-turbo."
)

itk_module(
  ITKJPEG
  DESCRIPTION "${DOCUMENTATION}"
  SPDX_LICENSE
  "IJG AND BSD-3-Clause AND Zlib"
  SPDX_VERSION
  "3.0.4"
  SPDX_DOWNLOAD_LOCATION
  "https://libjpeg-turbo.org"
  SPDX_COPYRIGHT
  "Copyright libjpeg-turbo contributors"
  SPDX_PURL
  "pkg:generic/libjpeg-turbo@3.0.4"
)
