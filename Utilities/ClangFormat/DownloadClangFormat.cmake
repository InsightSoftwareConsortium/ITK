include(ExternalProject)

if(WIN32)
  set(exe .exe)
endif()
set(CLANG_FORMAT_EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/ClangFormat-prefix/src/ClangFormat/clang-format${exe})
configure_file("${ITK_CMAKE_DIR}/ITKClangFormatConfig.cmake.in"
               "${CMAKE_CURRENT_BINARY_DIR}/ITKClangFormatConfig.cmake" @ONLY)

set(_clang_format_hash )
set(_clang_format_url )
if(CMAKE_HOST_SYSTEM_NAME MATCHES "Linux" AND CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "x86_64")
  set(_clang_format_hash
    b14de32036c48f6c62998e2ebab509e71a0ae71464acb4616484e3a6eb941e1d9fac38559f5d27ea0cbbb512d590279ffb3015fae17779229e1090c2763ebcf3)
  set(_clang_format_url "https://data.kitware.com/api/v1/file/hashsum/sha512/${_clang_format_hash}/download")
elseif(CMAKE_HOST_SYSTEM_NAME MATCHES "Darwin" AND CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "x86_64" AND (NOT CMAKE_HOST_SYSTEM_VERSION VERSION_LESS "13.0.0"))
  set(_clang_format_hash
    97460f9eef556a27592ccd99d8fc894554e5b3196326df4e33bfcdecfcb7eda2b5c7488008abc4dd923d607f2cb47d61567b1da99df60f31f719195118c117a9)
  set(_clang_format_url "https://data.kitware.com/api/v1/file/hashsum/sha512/${_clang_format_hash}/download")
elseif(CMAKE_HOST_SYSTEM_NAME MATCHES "Windows" AND CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "AMD64")
  set(_clang_format_hash
    e96dd15938fd9b1c57028a519189f138397774eb6b66971d114300d2a80248adda9f74b192985a3c91c7de52c4dbe21800bc6b3cc8201c4985fc39ecfc64fdbe)
  set(_clang_format_url "https://data.kitware.com/api/v1/file/hashsum/sha512/${_clang_format_hash}/download")
else()
  # If desired, a compatible clang-format can be provided manually with
  #   `git config clangFormat.binary /path/to/clang-format`.
endif()

# XXX Implementation of the itk_download_attempt_check macro copied from the
#     ITK main CMakeLists.txt. This allows external modules to use the logic
#     which is not defined when building against an ITK build tree.
#     Equivalent to "itk_download_attempt_check(ClangFormat)".
if(ITK_FORBID_DOWNLOADS)
  message(SEND_ERROR "Attempted to download ClangFormat when ITK_FORBID_DOWNLOADS is ON")
endif()
if(NOT TARGET ClangFormat AND _clang_format_hash)
  ExternalProject_add(ClangFormat
    URL ${_clang_format_url}
    URL_HASH SHA512=${_clang_format_hash}
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ""
    LOG_DOWNLOAD 1
    LOG_UPDATE 0
    LOG_CONFIGURE 0
    LOG_BUILD 0
    LOG_TEST 0
    LOG_INSTALL 0
    INSTALL_COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_BINARY_DIR}/ITKClangFormatConfig.cmake
    )
endif()
