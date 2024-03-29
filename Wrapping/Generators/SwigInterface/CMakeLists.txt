###############################################################################
# Swig and PCRE do not support blank spaces in install directory namespaces
# This means that ITK cannot be built in a path that contains spaces
# See https://www.swig.org/Doc1.3/Windows.html
if("${CMAKE_BINARY_DIR}" MATCHES "^.* .*$")
  message(FATAL_ERROR "Swig and PCRE do not support paths with space characters. Please change build directory name.")
endif()

###############################################################################
# Build swig

option(ITK_USE_SYSTEM_SWIG "Use system swig. If OFF, swig is built as an external project." OFF)
mark_as_advanced(ITK_USE_SYSTEM_SWIG)

# Minimal swig version
set(swig_version_min 4.2.0)
# Version used in vendored binary or source build configured in this file.
set(ITK_SWIG_VERSION 2024-03-26-master)

if(WIN32)
  set(swig_cmake_version 4.3.0)
  set(swigwin_hash
      "cec9eeebfec7f2a8ccf7b166a11cf8dbbc5e1eacca35563e4f0882b2b261658f394f6607243813d7083e7e2a2bbec23c5cf8b4dd92ad85838c6eb971f3833715"
  )
  set(swigwin_cid "bafybeibljxzip2irc3q3w5qlh2ae5ns27xpi7mo6iskxni45dcmwtk2x6a")

  set(swig_ep "${CMAKE_CURRENT_BINARY_DIR}/swigwin-amd64-${ITK_SWIG_VERSION}/bin/swig.exe")
elseif(CMAKE_HOST_SYSTEM_NAME MATCHES "Linux" AND (CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "x86_64" OR CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "aarch64"))
  set(swig_cmake_version 4.3.0)
  if (CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "aarch64")
    set(swiglinux_hash
        "9a3cfa330235ac78799c7401299506db5ea7459314b781bf27f2db8ddb582508ec5b15c2fa96fc241d5b832c4f702cd9ef043a61e37a574cac7ca78e4aba8f78"
    )
    set(swiglinux_cid "bafybeiaa2xv7oxnvaz6qupdaykgwakldrmahk54n2pp2z2ianuvq57uvmy")
    set(swiglinux_arch "arm64")
  else()
    set(swiglinux_hash
        "bdf82ad5281dfdba4b24c83b0d15c76e83cd58c6c78ecfc7a449f869f524d12fad9ad7f517995f864c2c00e61e7cd04132a442038197b92821b4011673a7d4fe"
    )
    set(swiglinux_cid "bafybeihp7hk4ljxuf7duqzei2h7y7xshzyhahxaot5mapv2xevkvwuci5m")
    set(swiglinux_arch "amd64")
  endif()

  set(swig_ep "${CMAKE_CURRENT_BINARY_DIR}/swiglinux-${swiglinux_arch}-${ITK_SWIG_VERSION}/bin/swig")
elseif(CMAKE_HOST_SYSTEM_NAME MATCHES "Darwin")
  set(swig_cmake_version 4.3.0)
  if (CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "arm64")
    set(swigmacos_hash
      "96eec93fd9e1df35813f8cc194a8811c8c84f265284c1515ec81aa7e696aee2c952f426e5155c011c163376e76245f5d7f479525f0d1ea8396524629ed6b8aec"
    )
    set(swigmacos_cid "bafybeialov6ur5q2yli4mtclrvqoirk2nsteum3vgnswjypquwejwomxnm")
    set(swigmacos_arch "arm64")
  else()
    set(swigmacos_hash
      "169760d2a34c2a95a907e80d93c1dc7ebc33a7dd34eb7ed7a9841866db3e25f7d7e93bf49ea28db01843aca5c2c6712756c1f01f6531d6ee027dc6eb293d95d7"
    )
    set(swigmacos_cid "bafybeifkl5wfscum7pnsjdmwhshrv5v6srilinkrftunq5g4gqzgqmyqf4")
    set(swigmacos_arch "amd64")
  endif()

  set(swig_ep "${CMAKE_CURRENT_BINARY_DIR}/swigmacos-${swigmacos_arch}-${ITK_SWIG_VERSION}/bin/swig")
else()
  # Build from source

  set(swig_hash "9292f6786abed379278b8024f91b91f293f1be9764fe3c1a19023f7891c4e40587f965680ac6a595a5610cfb6650a73fd2f3932a83d5effad83f351fa70810a9")
  set(swig_cid "bafybeib56uexrqtccgzxjgmlammjii5ntwush4732kbkr6okbtjhzcmvia")

  # follow the standard EP_PREFIX locations
  set(swig_binary_dir ${CMAKE_CURRENT_BINARY_DIR}/swig-prefix/src/swig-build)
  set(swig_source_dir ${CMAKE_CURRENT_BINARY_DIR}/swig-prefix/src/swig)
  set(swig_install_dir ${CMAKE_CURRENT_BINARY_DIR}/swig)

  set(swig_ep "${swig_install_dir}/bin/swig")
endif()

if(ITK_USE_SYSTEM_SWIG)

  # the path set for the EP build prevents find_package to do its job
  if("${SWIG_EXECUTABLE}" STREQUAL "${swig_ep}")
    unset(SWIG_DIR CACHE)
    unset(SWIG_EXECUTABLE CACHE)
    unset(SWIG_VERSION CACHE)
  endif()

  # Look for system swig
  find_package(SWIG REQUIRED)

  # Check for the swig version
  if(${SWIG_VERSION} VERSION_LESS ${swig_version_min})
    message(WARNING "Swig version less than ${swig_version_min}: \"${SWIG_VERSION}\".")
  endif()

else()
  if(NOT TARGET swig)
    # Install swig ourselves
    set(SWIG_VERSION ${ITK_SWIG_VERSION})

    include(ExternalProject)

    # Set the timestamps of the extracted files to their archived
    # timestamps. Necessary to build configure/autoconf-based
    # projects such as SWIG and PCRE robustly.
    if(${CMAKE_VERSION} VERSION_LESS 3.24)
      set(download_extract_timestamp_flag)
    else()
      set(download_extract_timestamp_flag DOWNLOAD_EXTRACT_TIMESTAMP TRUE)
    endif()

    if(WIN32)
      # If we are building ITK
      if(ITK_BINARY_DIR)
        itk_download_attempt_check(SWIG)
      endif()
      ExternalProject_Add(
        swig
        URL "https://data.kitware.com/api/v1/file/hashsum/sha512/${swigwin_hash}/download"
            "https://dweb.link/ipfs/${swigwin_cid}/swigwin-amd64-${SWIG_VERSION}.zip"
            "https://itk.mypinata.cloud/ipfs/${swigwin_cid}/swigwin-amd64-${SWIG_VERSION}.zip"
            "https://w3s.link/ipfs/${swigwin_cid}/swigwin-amd64-${SWIG_VERSION}.zip"
        URL_HASH SHA512=${swigwin_hash}
        SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR}/swigwin-amd64-${SWIG_VERSION}
        CONFIGURE_COMMAND ""
        BUILD_COMMAND ""
        INSTALL_COMMAND "" ${download_extract_timestamp_flag})
      set(SWIG_DIR ${CMAKE_CURRENT_BINARY_DIR}/swigwin-amd64-${SWIG_VERSION}/bin)
    elseif(CMAKE_HOST_SYSTEM_NAME MATCHES "Linux" AND (CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "x86_64" OR CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "aarch64"))
      # If we are building ITK
      if(ITK_BINARY_DIR)
        itk_download_attempt_check(SWIG)
      endif()
      ExternalProject_Add(
        swig
        URL "https://data.kitware.com/api/v1/file/hashsum/sha512/${swiglinux_hash}/download"
            "https://dweb.link/ipfs/${swiglinux_cid}/swiglinux-${swiglinux_arch}-${SWIG_VERSION}.zip"
            "https://itk.mypinata.cloud/ipfs/${swiglinux_cid}/swiglinux-${swiglinux_arch}-${SWIG_VERSION}.zip"
            "https://w3s.link/ipfs/${swiglinux_cid}/swiglinux-${swiglinux_arch}-${SWIG_VERSION}.zip"
        URL_HASH SHA512=${swiglinux_hash}
        SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR}/swiglinux-${swiglinux_arch}-${SWIG_VERSION}
        CONFIGURE_COMMAND ""
        BUILD_COMMAND ""
        INSTALL_COMMAND "" ${download_extract_timestamp_flag})
      set(SWIG_DIR
          ${CMAKE_CURRENT_BINARY_DIR}/swiglinux-${swiglinux_arch}-${SWIG_VERSION}/share/swig/${swig_cmake_version}
          CACHE FILEPATH "swig directory.")
    elseif(CMAKE_HOST_SYSTEM_NAME MATCHES "Darwin")
      # If we are building ITK
      if(ITK_BINARY_DIR)
        itk_download_attempt_check(SWIG)
      endif()
      ExternalProject_Add(
        swig
        URL "https://data.kitware.com/api/v1/file/hashsum/sha512/${swigmacos_hash}/download"
            "https://dweb.link/ipfs/${swigmacos_cid}/swigmacos-${swigmacos_arch}-${SWIG_VERSION}.zip"
            "https://itk.mypinata.cloud/ipfs/${swigmacos_cid}/swigmacos-${swigmacos_arch}-${SWIG_VERSION}.zip"
            "https://w3s.link/ipfs/${swigmacos_cid}/swigmacos-${swigmacos_arch}-${SWIG_VERSION}.zip"
        URL_HASH SHA512=${swigmacos_hash}
        SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR}/swigmacos-${swigmacos_arch}-${SWIG_VERSION}
        CONFIGURE_COMMAND ""
        BUILD_COMMAND ""
        INSTALL_COMMAND "" ${download_extract_timestamp_flag})
      set(SWIG_DIR
          ${CMAKE_CURRENT_BINARY_DIR}/swigmacos-${swigmacos_arch}-${SWIG_VERSION}/share/swig/${swig_cmake_version}
          CACHE FILEPATH "swig directory.")
    else()
      # build swig as an external project

      # If we are building ITK
      if(ITK_BINARY_DIR)
        itk_download_attempt_check(PCRE)
      endif()
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
            "-DCMAKE_C_FLAGS:STRING=${CMAKE_C_FLAGS} -w")
      endif()
      set(pcre_hash
          "e1183226ea3e56a1c7ec97e2c7d7ad5a163b9b576930c997e6ac4c26f1c92fc7e9491b5b297b1dd68d43432f7fbbddcc6207a189beb37ef158ad68bad5598988"
      )
      set(pcre_cid "bafybeicnpyjts3vq4lpdfhktr5qioq4qdifmtjkbemyyqpkayxjsqtwz74")
      ExternalProject_Add(
        PCRE
        URL "https://data.kitware.com/api/v1/file/hashsum/sha512/${pcre_hash}/download"
            "https://dweb.link/ipfs/${pcre_cid}/pcre-10.43.tar.gz"
            "https://itk.pinata.cloud/ipfs/${pcre_cid}/pcre-10.43.tar.gz"
            "https://w3s.link/ipfs/${pcre_cid}/pcre-10.43.tar.gz"
        URL_HASH SHA512=${pcre_hash}
        CMAKE_GENERATOR "${CMAKE_GENERATOR}"
        CMAKE_CACHE_ARGS
          ${compiler_information}
          -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
          -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
        INSTALL_DIR ${CMAKE_CURRENT_BINARY_DIR}/PCRE2
        ${download_extract_timestamp_flag}
        )

      # swig uses bison find it by cmake and pass it down
      find_package(BISON)
      set(BISON_FLAGS
          ""
          CACHE STRING "Flags used by bison")
      mark_as_advanced(BISON_FLAGS)

      # If we are building ITK
      if(ITK_BINARY_DIR)
        itk_download_attempt_check(SWIG)
      endif()

      ExternalProject_Add(
        swig
        URL "https://data.kitware.com/api/v1/file/hashsum/sha512/${swig_hash}/download"
            "https://dweb.link/ipfs/${swig_cid}/swig-4.0.2.tar.gz"
            "https://itk.pinata.cloud/ipfs/${swig_cid}/swig-4.0.2.tar.gz"
            "https://w3s.link/ipfs/${swig_cid}/swig-4.0.2.tar.gz"
        URL_HASH SHA512=${swig_hash}
        CMAKE_GENERATOR "${CMAKE_GENERATOR}"
        CMAKE_CACHE_ARGS
          ${compiler_information}
          -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
          -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
          -DPCRE2_DIR:PATH=${CMAKE_CURRENT_BINARY_DIR}/PCRE2
        INSTALL_DIR ${CMAKE_CURRENT_BINARY_DIR}/swig
        DEPENDS PCRE ${download_extract_timestamp_flag}
        )
    endif()

    set(SWIG_DIR
        ${CMAKE_CURRENT_BINARY_DIR}/swig/share/swig/${SWIG_VERSION}
        CACHE FILEPATH "swig directory.")
    mark_as_advanced(SWIG_DIR)
    set(SWIG_EXECUTABLE
        ${swig_ep}
        CACHE FILEPATH "swig executable." FORCE)
  endif()
  mark_as_advanced(SWIG_EXECUTABLE)
endif()

set(PYGCCXML_DIR
    ${ITK_CMAKE_DIR}/../Modules/ThirdParty/pygccxml/src
    CACHE INTERNAL "pygccxml path")

###############################################################################
# store the current dir, so it can be reused later
set(ITK_WRAP_SWIGINTERFACE_SOURCE_DIR
    "${CMAKE_CURRENT_SOURCE_DIR}"
    CACHE INTERNAL "swig interface source dir")
set(ITK_WRAP_SWIGINTERFACE_BINARY_DIR
    "${CMAKE_CURRENT_BINARY_DIR}"
    CACHE INTERNAL "swig interface binary dir")

set(WRAPPER_MASTER_INDEX_OUTPUT_DIR
    "${ITK_DIR}/Wrapping/Typedefs"
    CACHE INTERNAL "typedefs dir")
set(IGENERATOR
    "${CMAKE_CURRENT_SOURCE_DIR}/igenerator.py"
    CACHE INTERNAL "igenerator.py path" FORCE)

macro(itk_wrap_simple_type_swig_interface wrap_class swig_name)
  string(APPEND SWIG_INTERFACE_TYPEDEFS "using ${swig_name} = ${wrap_class};\n")
endmacro()

include_directories("${WRAPPER_MASTER_INDEX_OUTPUT_DIR}")
