set(
  DOCUMENTATION
  "This module contains the third party <a
href=\"http://www.netlib.org/slatec/\">netlib slatec</a> routines.  They are
used by the probability distributions in ITK."
)

itk_module(
  ITKNetlib
  DEPENDS ITKVNL
  DESCRIPTION "${DOCUMENTATION}"
  SPDX_LICENSE "LicenseRef-Netlib-SLATEC"
  SPDX_DOWNLOAD_LOCATION "https://www.netlib.org/slatec"
  SPDX_COPYRIGHT "NOASSERTION"
  SPDX_CUSTOM_LICENSE_NAME "Netlib SLATEC Public Domain License"
  SPDX_CUSTOM_LICENSE_TEXT "The SLATEC Common Mathematical Library is issued by the U.S. Government and is in the public domain."
)
