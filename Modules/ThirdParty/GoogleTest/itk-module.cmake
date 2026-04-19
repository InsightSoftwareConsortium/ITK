set(
  DOCUMENTATION
  "This module contains the third party <a href=\"https://github.com/google/googletest\">googletest</a> library,
Google's C++ test framework. This module provides the GTest::gtest and GTest::gtest_main targets in the build directory only, and are not installed."
)

itk_module(
  ITKGoogleTest
  DEPENDS
  DESCRIPTION "${DOCUMENTATION}"
  SPDX_LICENSE
  "BSD-3-Clause"
  SPDX_VERSION
  "1.17.0"
  SPDX_DOWNLOAD_LOCATION
  "https://github.com/google/googletest"
  SPDX_COPYRIGHT
  "Copyright Google Inc."
  SPDX_PURL
  "pkg:github/google/googletest@v1.17.0"
)
