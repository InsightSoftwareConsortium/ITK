# -----------------------------------------------------------------------------------------
# Provides clang-format identification to assist with maintaining consistent
# style across the ITK toolkit

# Globally the ${ITK_SOURCE_DIR}/.clang-format file is used to configure the enforced style.
#
#
# Step 1: Search for local binaries that have the correct clang-format version
#    if acceptable version not found do step 2
# Step 2a [OPTIONAL]: Configure to External project for future download of clang-format binaries
#                     setting variables to values that will eventually be the location of the
#                     binaries that are downloaded during the build phase of ITK as part of
#                     the ClangFormat build target
# Step 2b [OPTIONAL]: Build ITK, including the ClangFormat target that downloads clang-format binaries
# Step 2c [OPTIONAL]: Search again for binaries that meet the version requirements.
# Step 3: configure ITKClangFormatConfig.cmake.in to setup the git controlled hooks to clang-format
#         with best guess of what the executable will be after building ITK.
#
# The ITK style guidelines are represented by clang-format version ${CLANG_FORMAT_MIN_VERSION}
# rules defined in ${ITK_SOURCE_DIR}/.clang-format
set(CLANG_FORMAT_MIN_VERSION 8.0)  # First acceptable version
set(CLANG_FORMAT_MAX_VERSION 9.0)  # First unacceptable version

function(check_clang_format_version EXECUTABLE_FOUND_VARNAME CLANG_FORMAT_EXECUTABLE)
  # This function has the required signature for the
  # VALIDATOR feature of cmake >= 3.25 find_program function
  #
  # The check_clang_format_version function test that correct version of clang-format is available
  #
  if(CLANG_FORMAT_EXECUTABLE)
    # Get the version of clang-format
    execute_process(
            COMMAND ${CLANG_FORMAT_EXECUTABLE} --version
            OUTPUT_VARIABLE CLANG_FORMAT_VERSION_RESPONSE
    )
    string(REGEX MATCH "([0-9]+\\.[0-9]+\\.[0-9]+)" VERSION_MATCH "${CLANG_FORMAT_VERSION_RESPONSE}")

    # Extract the matched version number
    if(VERSION_MATCH)
      set(CLANG_FORMAT_VERSION "${CMAKE_MATCH_1}")
    endif()
    unset(CLANG_FORMAT_VERSION_RESPONSE)
    unset(VERSION_MATCH)

    # Print the version
    if( CLANG_FORMAT_VERSION VERSION_GREATER_EQUAL "${CLANG_FORMAT_MIN_VERSION}" AND CLANG_FORMAT_VERSION VERSION_LESS "${CLANG_FORMAT_MAX_VERSION}" )
      message(STATUS "Found CLANG_FORMAT_EXECUTABLE=${CLANG_FORMAT_EXECUTABLE} with acceptable version condition ${CLANG_FORMAT_MIN_VERSION} <= ${CLANG_FORMAT_VERSION} < ${CLANG_FORMAT_MAX_VERSION}")
      set(${EXECUTABLE_FOUND_VARNAME} TRUE PARENT_SCOPE)
    else()
      message(STATUS "unacceptable version found: CLANG_FORMAT_EXECUTABLE=${CLANG_FORMAT_EXECUTABLE}")
      message(STATUS "unmet version condition: ${CLANG_FORMAT_MIN_VERSION} <= ${CLANG_FORMAT_VERSION} < ${CLANG_FORMAT_MAX_VERSION}" )
      set(${EXECUTABLE_FOUND_VARNAME} FALSE PARENT_SCOPE)
    endif()
  else()
    set(${EXECUTABLE_FOUND_VARNAME} FALSE PARENT_SCOPE)
  endif()
  unset(CLANG_FORMAT_VERSION)
  unset(CLANG_FORMAT_MIN_VERSION)
  unset(CLANG_FORMAT_MAX_VERSION)
endfunction()


if(ITK_USE_CLANG_FORMAT)
  get_filename_component(CLANG_FORMAT_EXECUTABLE_LOCAL_DOWNLOAD_DIR "${ITK_VERSIONED_CLANG_FORMAT_EXECUTABLE}" DIRECTORY)
  get_filename_component(CLANG_FORMAT_EXECUTABLE_LOCAL_DOWNLOAD_PROGRAM_NAME "${ITK_VERSIONED_CLANG_FORMAT_EXECUTABLE}" NAME)

  if(CMAKE_VERSION LESS "3.25")
    # If CLANG_FORMAT_EXECUTABLE is manually set to an incorrect version, unset the variable
    check_clang_format_version( CORRECT_VERSION_CLANG_FORMAT_EXECUTABLE_FOUND "${CLANG_FORMAT_EXECUTABLE}")
    if (NOT CORRECT_VERSION_CLANG_FORMAT_EXECUTABLE_FOUND)
      unset(CLANG_FORMAT_EXECUTABLE CACHE)
    endif()
    unset(CORRECT_VERSION_CLANG_FORMAT_EXECUTABLE_FOUND)

    # Search for clang-format, if the first one found is not a valid version, then manually
    # setting the correct version will be required.  Use the downloaded binaries as the first
    # check.
    find_program(CLANG_FORMAT_EXECUTABLE
            NAMES "${CLANG_FORMAT_EXECUTABLE_LOCAL_DOWNLOAD_PROGRAM_NAME}"  clang-format
            PATHS "${CLANG_FORMAT_EXECUTABLE_LOCAL_DOWNLOAD_DIR}" )
    check_clang_format_version( CLANG_FORMAT_EXECUTABLE_FOUND ${CLANG_FORMAT_EXECUTABLE})
    if (NOT CLANG_FORMAT_EXECUTABLE_FOUND)
      message(FATAL_ERROR "Disable ITK_USE_CLANG_FORMAT or set CLANG_FORMAT_EXECUTABLE to one "
              "in the acceptable version in range ${CLANG_FORMAT_MIN_VERSION} <= Version < ${CLANG_FORMAT_MAX_VERSION}" )
    else()
      message(STATUS "clang-format with acceptable version found: ${CLANG_FORMAT_EXECUTABLE}" )
    endif()
  else()
    find_program(CLANG_FORMAT_EXECUTABLE
            NAMES clang-format "${CLANG_FORMAT_EXECUTABLE_LOCAL_DOWNLOAD_PROGRAM_NAME}"
            PATHS /Users/johnsonhj/Dashboard/src/ITK/venv/bin "${CLANG_FORMAT_EXECUTABLE_LOCAL_DOWNLOAD_DIR}"
            VALIDATOR check_clang_format_version)
  endif()
  mark_as_advanced(CLANG_FORMAT_EXECUTABLE)
  unset(CLANG_FORMAT_EXECUTABLE_LOCAL_DOWNLOAD_DIR)
  unset(CLANG_FORMAT_EXECUTABLE_LOCAL_DOWNLOAD_PROGRAM_NAME)
else()
  unset(CLANG_FORMAT_EXECUTABLE CACHE)
endif()
# ======================


# setup external project do download local clang-format if acceptable version not found
if(NOT EXISTS CLANG_FORMAT_EXECUTABLE)
  include(ExternalProject)

  if(WIN32)
    set(exe .exe)
  endif()

  # NOTE: Configuration of variables occurs during cmake config, download of binary occurs during build of ITK
  set(ITK_VERSIONED_CLANG_FORMAT_EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/ClangFormat-prefix/src/ClangFormat/clang-format${exe})
  configure_file("${ITK_CMAKE_DIR}/ITKClangFormatConfig.cmake.in"
                 "${CMAKE_CURRENT_BINARY_DIR}/ITKClangFormatConfig.cmake" @ONLY)

  set(_clang_format_hash)
  set(_clang_format_url)
  if(CMAKE_HOST_SYSTEM_NAME MATCHES "Linux" AND CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "x86_64")
    set(_clang_format_hash
        b14de32036c48f6c62998e2ebab509e71a0ae71464acb4616484e3a6eb941e1d9fac38559f5d27ea0cbbb512d590279ffb3015fae17779229e1090c2763ebcf3
    )
    set(_clang_format_url "https://data.kitware.com/api/v1/file/hashsum/sha512/${_clang_format_hash}/download")
  elseif(
    CMAKE_HOST_SYSTEM_NAME MATCHES "Darwin"
    AND ( CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "x86_64" OR CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "arm64" )
    AND (NOT
         CMAKE_HOST_SYSTEM_VERSION
         VERSION_LESS
         "13.0.0"
        ))
    set(_clang_format_hash
        97460f9eef556a27592ccd99d8fc894554e5b3196326df4e33bfcdecfcb7eda2b5c7488008abc4dd923d607f2cb47d61567b1da99df60f31f719195118c117a9
    )
    set(_clang_format_url "https://data.kitware.com/api/v1/file/hashsum/sha512/${_clang_format_hash}/download")
  elseif(CMAKE_HOST_SYSTEM_NAME MATCHES "Windows" AND CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "AMD64")
    set(_clang_format_hash
        e96dd15938fd9b1c57028a519189f138397774eb6b66971d114300d2a80248adda9f74b192985a3c91c7de52c4dbe21800bc6b3cc8201c4985fc39ecfc64fdbe
    )
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
  if(POLICY CMP0135)
    cmake_policy(PUSH)
    cmake_policy(SET CMP0135 NEW)
  endif()
  if(NOT TARGET ClangFormat AND _clang_format_hash)
    ExternalProject_Add(
      ClangFormat
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
      INSTALL_COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_BINARY_DIR}/ITKClangFormatConfig.cmake)
  endif()
  if(POLICY CMP0135)
    cmake_policy(POP)
  endif()
else()
  set(ITK_VERSIONED_CLANG_FORMAT_EXECUTABLE "${CLANG_FORMAT_EXECUTABLE}")
endif()
