# Provides itk_clangformat_setup to assist with maintaining consistent
# style across the ITK toolkit
#
# Globally the ${ITK_SOURCE_DIR}/.clang-format file is used to configure the enforced style.
#
# Clang version 8.0 is required
#
# The ITK style guidelines are represented by clang-format version 8.0.0
# rules defined in ${ITK_SOURCE_DIR}/.clang-format
#
option(ITK_USE_CLANG_FORMAT "Enable the use of clang-format enforce ITK coding style." ${BUILD_TESTING})
mark_as_advanced(ITK_USE_CLANG_FORMAT)

if(ITK_USE_CLANG_FORMAT AND NOT EXISTS "${CLANG_FORMAT_EXECUTABLE}")
  # Download pre-built binaries (about 2M) of clang-format extracted from
  # https://releases.llvm.org/download.html and cached on data.kitware.com
  #
  # Darwin               - macOS/OS X (Clang, GCC)             https://releases.llvm.org/8.0.0/clang+llvm-8.0.0-x86_64-apple-darwin.tar.xz
  # Windows              - Windows (Visual Studio, MinGW GCC)  https://releases.llvm.org/8.0.0/LLVM-8.0.0-win64.exe
  #                                                            https://releases.llvm.org/8.0.0/LLVM-8.0.0-win32.exe
  # Linux                - Linux (GCC, Intel, PGI)             https://releases.llvm.org/8.0.0/clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-14.04.tar.xz
  #                                                            https://releases.llvm.org/8.0.0/clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-16.04.tar.xz
  #                                                            https://releases.llvm.org/8.0.0/clang+llvm-8.0.0-x86_64-linux-gnu-ubuntu-18.04.tar.xz
  #                                                            https://releases.llvm.org/8.0.0/clang+llvm-8.0.0-x86_64-linux-sles11.3.tar.xz
  #                                                            https://releases.llvm.org/8.0.0/clang+llvm-8.0.0-armv7a-linux-gnueabihf.tar.xz
  #                                                            https://releases.llvm.org/8.0.0/clang+llvm-8.0.0-aarch64-linux-gnu.tar.xz
  #
  # FreeBSD              - FreeBSD                             https://releases.llvm.org/8.0.0/clang+llvm-8.0.0-amd64-unknown-freebsd11.tar.xz
  #                                                            https://releases.llvm.org/8.0.0/clang+llvm-8.0.0-i386-unknown-freebsd11.tar.xz
  #
  # Android              - Android NDK (GCC, Clang)
  # CrayLinuxEnvironment - Cray supercomputers (Cray compiler)
  # MSYS                 - Windows (MSYS2 shell native GCC)
  if("${CMAKE_SYSTEM_NAME}" STREQUAL "Darwin")
    set(CLANG_FORMAT_DOWNLOAD_URL "https://data.kitware.com/api/v1/file/5d274e88877dfcc902effc47/download")
  elseif("${CMAKE_SYSTEM_NAME}" STREQUAL "Windows")
    set(CLANG_FORMAT_DOWNLOAD_URL "https://data.kitware.com/api/v1/file/5d2b8775877dfcc902fd8236/download")
  elseif("${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
    set(CLANG_FORMAT_DOWNLOAD_URL "https://data.kitware.com/api/v1/file/5d2b8c87877dfcc902fda594/download")
  else()
    message(FATAL_ERROR "UNKNOWN SYSTEM: Set CLANG_FORMAT_EXECUTABLE manually to the clang-format version 8.0 binary")
  endif()
  file(MAKE_DIRECTORY "${ITK_BINARY_DIR}/temp")
  if(WIN32)
    set(exe .exe)
  endif()
  set(CLANG_FORMAT_EXECUTABLE_NAME "clang-format-${CMAKE_SYSTEM_NAME}${exe}")
  file(DOWNLOAD "${CLANG_FORMAT_DOWNLOAD_URL}" "${ITK_BINARY_DIR}/temp/${CLANG_FORMAT_EXECUTABLE_NAME}")
  file(
    COPY "${ITK_BINARY_DIR}/temp/${CLANG_FORMAT_EXECUTABLE_NAME}"
    DESTINATION "${ITK_BINARY_DIR}"
    FILE_PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ)
  unset(CLANG_FORMAT_DOWNLOAD_URL)
  set(CLANG_FORMAT_EXECUTABLE
      "${ITK_BINARY_DIR}/${CLANG_FORMAT_EXECUTABLE_NAME}"
      CACHE FILEPATH "The binary for clang-format" FORCE)
endif()
if(CLANG_FORMAT_EXECUTABLE)
  mark_as_advanced(CLANG_FORMAT_EXECUTABLE)
endif()
if(ITK_USE_CLANG_FORMAT AND NOT EXISTS "${CLANG_FORMAT_EXECUTABLE}")
  message(FATAL_ERROR "Missing suitable clang-format executable, set CLANG_FORMAT_EXECUTABLE variable to version 8.0")
  unset(CLANG_FORMAT_EXECUTABLE)
endif()
