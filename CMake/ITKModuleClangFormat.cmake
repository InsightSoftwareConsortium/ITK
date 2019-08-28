# Download and configure clang-format for style uniformity during development.
#
# This code helps download a version of clang-format binaries consistent with
# ITK's clang-format configuration. Git configuration is added during
# development to help automatically apply the formatting.
#
# Globally, the ITK/.clang-format file provides the base style configuration.
# For more information on ITK coding style, see the ITK Coding Style Guide in
# the ITK Software Guide.
option(ITK_USE_CLANG_FORMAT "Enable the use of clang-format for coding style formatting." ${BUILD_TESTING})
mark_as_advanced(ITK_USE_CLANG_FORMAT)

if(BUILD_TESTING AND ITK_USE_CLANG_FORMAT AND NOT CMAKE_CROSSCOMPILING AND NOT ITK_FORBID_DOWNLOADS)
  include(${ITK_CMAKE_DIR}/../Utilities/ClangFormat/DownloadClangFormat.cmake)
endif()
