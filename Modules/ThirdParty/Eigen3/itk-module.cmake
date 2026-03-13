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
  SPDX_LICENSE "MPL-2.0"
  SPDX_DOWNLOAD_LOCATION "https://eigen.tuxfamily.org"
  SPDX_COPYRIGHT "Copyright Eigen contributors"
)
