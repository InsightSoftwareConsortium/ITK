set(
  DOCUMENTATION
  "This module contains the third party <a href=\"https://github.com/eigenteam/eigen-git-mirror\">Eigen</a> library.
Eigen is a C++ template library for linear algebra: matrices, vectors, numerical solvers, and related algorithms."
)

itk_module(
  ITKEigen3
  DEPENDS
  DESCRIPTION "${DOCUMENTATION}"
  EXCLUDE_FROM_DEFAULT
  SPDX_LICENSE
  "MPL-2.0 OR Apache-2.0"
  SPDX_VERSION
  "3.4.90"
  SPDX_DOWNLOAD_LOCATION
  "https://gitlab.com/libeigen/eigen"
  SPDX_COPYRIGHT
  "Copyright Eigen contributors"
  SPDX_PURL
  "pkg:gitlab/libeigen/eigen@3.4.90"
)
