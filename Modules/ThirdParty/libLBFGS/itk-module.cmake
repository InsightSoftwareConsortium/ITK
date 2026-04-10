set(
  DOCUMENTATION
  "This module contains the third party <a
href=\"https://github.com/chokkan/liblbfgs\">libLBFGS</a> library,
a C++ implementaiton of the LBFGS implementation in Netlib."
)

itk_module(
  ITKLIBLBFGS
  DESCRIPTION "${DOCUMENTATION}"
  SPDX_LICENSE "MIT"
  SPDX_DOWNLOAD_LOCATION "https://github.com/chokkan/liblbfgs"
  SPDX_COPYRIGHT "Copyright Naoaki Okazaki"
)
