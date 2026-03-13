set(
  DOCUMENTATION
  "This module contains the third party <a
href=\"http://sourceforge.net/projects/dicomparser/\">DICOMParser</a> library.
DICOMParser is a small, lightweight C++ toolkit for reading DICOM format medical
image files."
)

itk_module(
  ITKDICOMParser
  DESCRIPTION "${DOCUMENTATION}"
  SPDX_LICENSE "BSD-3-Clause"
  SPDX_DOWNLOAD_LOCATION "https://github.com/InsightSoftwareConsortium/ITK"
  SPDX_COPYRIGHT "Copyright Kitware Inc."
)
