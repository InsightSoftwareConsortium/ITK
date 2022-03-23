###############################################################################
option(ITK_USE_SYSTEM_CASTXML "Use system castxml. If OFF, castxml is built as external project." OFF)
mark_as_advanced(ITK_USE_SYSTEM_CASTXML)
if(WIN32)
  set(exe .exe)
endif()
set(castxml_ep ${CMAKE_CURRENT_BINARY_DIR}/castxml/bin/castxml${exe})

if(ITK_USE_SYSTEM_CASTXML)
  # the path set for the EP build prevents find_program to do its job
  if("${CASTXML_EXECUTABLE}" STREQUAL "${castxml_ep}")
    unset(CASTXML_EXECUTABLE CACHE)
  endif()
  find_program(CASTXML_EXECUTABLE castxml)
  if(NOT CASTXML_EXECUTABLE)
    message(FATAL_ERROR "Could NOT find castxml. Turn ITK_USE_SYSTEM_CASTXML to OFF to build it with ITK.")
  endif()
else()
  include(ExternalProject)

  set(_download_castxml_binaries 0)
  set(_castxml_url )
  set(_castxml_hash )

  # If 64 bit Linux build host, use the CastXML binary
  if(CMAKE_HOST_SYSTEM_NAME MATCHES "Linux" AND CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "x86_64")

    set(_castxml_hash dc78771884ce27837b408f339cc7bb7c5152552f88ebf30ffbc2069ae0407bb27ab2b37679adeff10ed26739571bf4c455dad309891c1c7f0fdc990fea474db1)
    set(_castxml_url "https://data.kitware.com/api/v1/file/hashsum/sha512/${_castxml_hash}/download")
    set(_download_castxml_binaries 1)
  elseif(CMAKE_HOST_SYSTEM_NAME MATCHES "Linux" AND CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "aarch64")

    set(_castxml_hash 611035b111b6c632e7d009a8e036c095661ef9e0ba43ee849e655675bee9492558edfa96348b83349f667581d6b0c9c894fe184d8beaec1482a7c69a0288c06d)
    set(_castxml_url "https://data.kitware.com/api/v1/file/hashsum/sha512/${_castxml_hash}/download")
    set(_download_castxml_binaries 1)

  # If 64 bit Windows build host, use the CastXML binary
  elseif(CMAKE_HOST_SYSTEM_NAME MATCHES "Windows" AND CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "AMD64")

    set(_castxml_hash 54a10b68399d17d41360a28c2f5091ed2dfcd104b8acb0f68a6224263de56348792af453355c1b243fb019efe476d01ad68dbd68db0908ce9cf62f606a5a31bb)
    set(_castxml_url "https://data.kitware.com/api/v1/file/hashsum/sha512/${_castxml_hash}/download")
    set(_download_castxml_binaries 1)

  # If 64 bit Mac OS X build host ( >= 10.9, Mavericks), use the CastXML binary
  elseif(CMAKE_HOST_SYSTEM_NAME MATCHES "Darwin" AND CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "x86_64" AND (NOT CMAKE_HOST_SYSTEM_VERSION VERSION_LESS "13.0.0"))

    set(_castxml_hash f7d62660b8c3f19481c0f2b87b144f9cd2a500ad94b1b50b27c1f36d4c0096ca46df9c8a833426392a1d5152bd58dabd2bb0b525f56e341f5310afcf98013748)
    set(_castxml_url "https://data.kitware.com/api/v1/file/hashsum/sha512/${_castxml_hash}/download")
    set(_download_castxml_binaries 1)
  elseif(CMAKE_HOST_SYSTEM_NAME MATCHES "Darwin" AND CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "arm64")

    set(_castxml_hash 5faef93541cb5f30414286a844e47b5732777f26246c6e0933170668e9cf2ff4f3fa4292e861e73e91ed50971caa9d6e7481eead9c9cefda6d54a9042634bf5c)
    set(_castxml_url "https://data.kitware.com/api/v1/file/hashsum/sha512/${_castxml_hash}/download")
    set(_download_castxml_binaries 1)

  endif()

  # Download binaries
  if(_download_castxml_binaries AND NOT TARGET castxml)
    if(ITK_BINARY_DIR)
      itk_download_attempt_check(CastXML)
    endif()
    ExternalProject_Add(castxml
      URL ${_castxml_url}
      URL_HASH SHA512=${_castxml_hash}
      CONFIGURE_COMMAND ""
      BUILD_COMMAND ""
      INSTALL_COMMAND "${CMAKE_COMMAND}" -E copy_directory "${CMAKE_CURRENT_BINARY_DIR}/castxml-prefix/src/castxml" "${CMAKE_CURRENT_BINARY_DIR}/castxml"
      )
    set(CASTXML_EXECUTABLE ${castxml_ep})
    set(CASTXML_EXECUTABLE ${castxml_ep} CACHE FILEPATH "castxml executable." FORCE)
  # Build from source
  elseif(NOT TARGET castxml)
    set(compiler_information)
    if(NOT CMAKE_CROSSCOMPILING)
      set(CMAKE_CXX_COMPILER_LAUNCHER_FLAG -DCMAKE_CXX_COMPILER_LAUNCHER:FILEPATH=${CMAKE_CXX_COMPILER_LAUNCHER})
      set(CMAKE_C_COMPILER_LAUNCHER_FLAG -DCMAKE_C_COMPILER_LAUNCHER:FILEPATH=${CMAKE_C_COMPILER_LAUNCHER})
      set(compiler_information
            -DCMAKE_CXX_COMPILER:FILEPATH=${CMAKE_CXX_COMPILER}
            ${CMAKE_CXX_COMPILER_LAUNCHER_FLAG}
            "-DCMAKE_CXX_FLAGS:STRING=${CMAKE_CXX_FLAGS} -w"
            -DCMAKE_C_COMPILER:FILEPATH=${CMAKE_C_COMPILER}
            ${CMAKE_C_COMPILER_LAUNCHER_FLAG}
            "-DCMAKE_C_FLAGS:STRING=${CMAKE_C_FLAGS} -w"
          )
    endif()
    # might be set to on by default when llvm/clang 3.6 are released
    # option(ITK_USE_SYSTEM_LLVM "Use system llvm and clang. If OFF, llvm and clang are built as external projects." ON)
    set(ITK_USE_SYSTEM_LLVM OFF)
    if(ITK_USE_SYSTEM_LLVM)
      find_package(LLVM REQUIRED)
      set(castxml_deps)
    else()
      # check the version of clang compiler
      if(APPLE AND (((CMAKE_CXX_COMPILER_ID STREQUAL "Clang") AND CMAKE_CXX_COMPILER_VERSION  VERSION_LESS "3.2") OR
        ((CMAKE_CXX_COMPILER_ID STREQUAL "AppleClang") AND CMAKE_CXX_COMPILER_VERSION  VERSION_LESS "5.0")))
        message(FATAL_ERROR "Mac OS X 10.9 (Mavericks) or newer is required to wrap ITK.")
      endif()
      # If we are building ITK
      if(ITK_BINARY_DIR)
        itk_download_attempt_check(LLVM)
        itk_download_attempt_check(Clang)
      endif()
      set(llvm_version 11.1.0)
      set(llvm_folder ${llvm_version})
      set(llvm_md5 69bc06661ce8f1872e27b40ff96002b2)
      ExternalProject_Add(llvm
        URL "https://github.com/llvm/llvm-project/releases/download/llvmorg-${llvm_folder}/llvm-${llvm_version}.src.tar.xz"
        URL_MD5 ${llvm_md5}
        SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR}/llvm-${llvm_version}
        CMAKE_ARGS -Wno-dev
        CMAKE_GENERATOR "${CMAKE_GENERATOR}"
        CMAKE_CACHE_ARGS
          ${compiler_information}
          -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
          -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
          -DLLVM_ENABLE_TERMINFO:BOOL=OFF
          -DLLVM_INCLUDE_TESTS:BOOL=OFF
          -DLLVM_INCLUDE_EXAMPLES:BOOL=OFF
          -DLLVM_INCLUDE_DOCS:BOOL=OFF
        INSTALL_DIR ${CMAKE_CURRENT_BINARY_DIR}/llvm
        )
      set(clang_md5 133c6719e22bfded74fcaf1d3092e979)
      ExternalProject_Add(clang
        URL https://github.com/llvm/llvm-project/releases/download/llvmorg-${llvm_folder}/clang-${llvm_version}.src.tar.xz
        URL_MD5 ${clang_md5}
        DEPENDS llvm
        SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR}/cfe-${llvm_version}
        CMAKE_ARGS -Wno-dev
        CMAKE_GENERATOR "${CMAKE_GENERATOR}"
        CMAKE_CACHE_ARGS
          ${compiler_information}
          -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
          -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
          -DCLANG_INCLUDE_DOCS:BOOL=OFF
          -DCLANG_INCLUDE_TESTS:BOOL=OFF
          -DLLVM_CONFIG:PATH=${CMAKE_CURRENT_BINARY_DIR}/llvm/bin/llvm-config${exe}
        INSTALL_DIR ${CMAKE_CURRENT_BINARY_DIR}/llvm
        )
      set(LLVM_DIR ${CMAKE_CURRENT_BINARY_DIR}/llvm/lib/cmake/llvm/)
      set(castxml_deps llvm clang)
    endif()

    # If we are building ITK
    if(ITK_BINARY_DIR)
      itk_download_attempt_check(CastXML)
    endif()
    ExternalProject_Add(castxml
      GIT_REPOSITORY ${git_protocol}://github.com/CastXML/CastXML.git
      # CastXML master, 2020-04-27
      GIT_TAG v0.4.3
      UPDATE_COMMAND ""
      DEPENDS ${castxml_deps}
      CMAKE_ARGS -Wno-dev
      CMAKE_GENERATOR "${CMAKE_GENERATOR}"
      CMAKE_CACHE_ARGS
        ${compiler_information}
        -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
        -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
        -DLLVM_DIR:PATH=${LLVM_DIR}
      INSTALL_DIR ${CMAKE_CURRENT_BINARY_DIR}/castxml
      )
    set(CASTXML_EXECUTABLE ${castxml_ep})
    set(CASTXML_EXECUTABLE ${castxml_ep} CACHE FILEPATH "castxml executable." FORCE)
  endif()
endif()
mark_as_advanced(CASTXML_EXECUTABLE)

###############################################################################
# store the current dir, so it can be reused later
set(ITK_WRAP_CASTXML_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}" CACHE INTERNAL "castxml source dir")
