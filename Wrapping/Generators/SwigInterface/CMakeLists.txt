###############################################################################
# Swig and PCRE do not support blank spaces in install directory namespaces
# This means that ITK cannot be built in a path that contains spaces
# See https://www.swig.org/Doc1.3/Windows.html
if("${CMAKE_BINARY_DIR}" MATCHES "^.* .*$")
  message(
    FATAL_ERROR
    "Swig and PCRE do not support paths with space characters. Please change build directory name."
  )
endif()

###############################################################################
# Build swig

option(
  ITK_USE_SYSTEM_SWIG
  "Use system swig. If OFF, swig is built as an external project."
  OFF
)
mark_as_advanced(ITK_USE_SYSTEM_SWIG)

# Minimal swig version
set(swig_version_min 4.3.0)
# Version of the vendored prebuilt binary or source build configured here.
set(ITK_SWIG_VERSION 4.4.1)
# Layout version of the share/swig/<dir> directory inside the vendored binary.
set(swig_cmake_version 4.4.1)

# The prebuilt binary's version is verified against swig_version_min after
# download; the source build produces exactly ITK_SWIG_VERSION, so assert here
# that the configured version satisfies the floor for every vendored path.
if(ITK_SWIG_VERSION VERSION_LESS swig_version_min)
  message(
    FATAL_ERROR
    "ITK_SWIG_VERSION (${ITK_SWIG_VERSION}) is below the required swig_version_min (${swig_version_min})."
  )
endif()

# Select the prebuilt binary for this host. The vendored swig<os>-<arch>-${VER}.zip
# archives are repackaged from the official, broadly-portable SWIG PyPI wheels
# (manylinux / macOS universal2 / Windows) so every archive shares the same
# bin/swig[.exe] + share/swig/${swig_cmake_version} layout. They are downloaded
# from data.kitware.com keyed by their SHA512 hash.
set(swig_prebuilt 0)
if(WIN32)
  set(swig_exe_subpath "bin/swig.exe")
else()
  set(swig_exe_subpath "bin/swig")
endif()
set(swig_dir_subpath "share/swig/${swig_cmake_version}")
if(WIN32)
  set(swig_prebuilt 1)
  set(
    swig_archive_hash
    "700bd4deed1abc33800efb70fd7ba7e59fc7e2b330e32e41165c942521b55ca752153558e1ccf2e5930b214db368d22b00dcfef06b07e4086e2ec55e338f3892"
  )
  set(
    swig_archive_cid
    "bafybeifxjnbydus4eq46bocwb2hcj5ufpbj4etocia5ims426gfiw7krfi"
  )
elseif(
  CMAKE_HOST_SYSTEM_NAME
    MATCHES
    "Linux"
  AND
    (
      CMAKE_HOST_SYSTEM_PROCESSOR
        STREQUAL
        "x86_64"
      OR
        CMAKE_HOST_SYSTEM_PROCESSOR
          STREQUAL
          "aarch64"
    )
)
  set(swig_prebuilt 1)
  if(CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "aarch64")
    set(
      swig_archive_hash
      "44a6d174c73a5f2e3ccbae4283e9888c55f88bc40df972abd27c7f041cb29cf03806067e8585fb08d01be7e858203a675f0c13d769f566a6517a0cb4c8902d24"
    )
    set(
      swig_archive_cid
      "bafybeifaqaus3yzp5oujxupxk422hfmcg2idf3oyvdtsppzq7ymn64nvze"
    )
  else()
    set(
      swig_archive_hash
      "6447a7b965077ceec0980daabe8628cc88fe1956bb2488a9edf569ad25ab8aa9ba2b568f4384e9dc3a213d58bc330e9d585f806461eb826ee2340b450d88ca11"
    )
    set(
      swig_archive_cid
      "bafybeiep2wbecrcii7xzfe2cekhfco6mlxqym4jhfdl6xi3p4aieblgz6m"
    )
  endif()
elseif(CMAKE_HOST_SYSTEM_NAME MATCHES "Darwin")
  set(swig_prebuilt 1)
  # macOS archive is the universal2 (x86_64 + arm64) wheel; both host
  # architectures resolve to the same content.
  set(
    swig_archive_hash
    "17c4637d49ab26dfe3442d668a6fc95068b9ee4a743fa45a90448bf28062bf4554b7fc4eeb4b8c19b620677e4017ae4491b59b7629a9eb11af4d6d191ce3cef9"
  )
  set(
    swig_archive_cid
    "bafybeidllsbllkqwoby6r5w4ueqzky2o7r6qkjbh45q57mczyh2fwfwd3q"
  )
endif()

# Assert a prebuilt/system swig satisfies swig_version_min at configure time, so
# a wrong or corrupt upload fails here rather than thousands of lines into the
# build.
function(itk_assert_swig_version _swig_exe _min_version)
  execute_process(
    COMMAND
      "${_swig_exe}" -version
    OUTPUT_VARIABLE _swig_version_output
    ERROR_VARIABLE _swig_version_error
    RESULT_VARIABLE _swig_version_result
    OUTPUT_STRIP_TRAILING_WHITESPACE
  )
  if(NOT _swig_version_result EQUAL 0)
    message(
      FATAL_ERROR
      "Failed to run vendored swig \"${_swig_exe}\": ${_swig_version_output}${_swig_version_error}"
    )
  endif()
  # The captured group is read from CMAKE_MATCH_1; the match output is unused.
  string(
    REGEX
    MATCH
    "SWIG Version ([0-9]+\\.[0-9]+\\.[0-9]+)"
    _
    "${_swig_version_output}"
  )
  if(NOT CMAKE_MATCH_1)
    message(
      FATAL_ERROR
      "Could not parse SWIG version from: ${_swig_version_output}"
    )
  endif()
  if(CMAKE_MATCH_1 VERSION_LESS _min_version)
    message(
      FATAL_ERROR
      "Vendored SWIG ${CMAKE_MATCH_1} is older than the required ${_min_version}."
    )
  endif()
  message(STATUS "Using vendored SWIG ${CMAKE_MATCH_1} (${_swig_exe})")
endfunction()

if(ITK_USE_SYSTEM_SWIG)
  # A cached executable from a prior prebuilt download lives under the build
  # tree and would prevent find_package from doing its job. Use a literal
  # substring test (not MATCHES, which treats the path as a regex).
  string(
    FIND
    "${SWIG_EXECUTABLE}"
    "${CMAKE_CURRENT_BINARY_DIR}"
    _swig_executable_in_build_tree
  )
  if(NOT _swig_executable_in_build_tree EQUAL -1)
    unset(SWIG_DIR CACHE)
    unset(SWIG_EXECUTABLE CACHE)
    unset(SWIG_VERSION CACHE)
  endif()
  unset(_swig_executable_in_build_tree)

  # Look for system swig
  find_package(SWIG REQUIRED)

  # Check for the swig version
  if(${SWIG_VERSION} VERSION_LESS ${swig_version_min})
    message(
      FATAL_ERROR
      "Swig version less than ${swig_version_min}: \"${SWIG_VERSION}\". Set ITK_USE_SYSTEM_SWIG=OFF or provide a newer swig."
    )
  endif()
elseif(swig_prebuilt)
  # Download and extract the prebuilt swig binary at configure time via
  # FetchContent so SWIG_EXECUTABLE exists immediately and can be verified.
  # Re-fetch if the cached path no longer exists (e.g. _deps was cleaned but
  # CMakeCache.txt kept).
  if(NOT SWIG_EXECUTABLE OR NOT EXISTS "${SWIG_EXECUTABLE}")
    set(SWIG_VERSION ${ITK_SWIG_VERSION})

    if(ITK_BINARY_DIR)
      itk_download_attempt_check(SWIG)
    endif()

    include(FetchContent)
    # DOWNLOAD_EXTRACT_TIMESTAMP requires CMake >= 3.24.
    if(${CMAKE_VERSION} VERSION_LESS 3.24)
      set(swig_extract_timestamp)
    else()
      set(
        swig_extract_timestamp
        DOWNLOAD_EXTRACT_TIMESTAMP
        TRUE
      )
    endif()

    # data.kitware.com (keyed by SHA512) is the primary; ITKTestingData serves the
    # same bytes by content id over the GitHub Pages CDN, and the IPFS gateways
    # provide further fallback once pinned. URL_HASH validates whichever responds.
    FetchContent_Declare(
      itkswig
      URL
        "https://data.kitware.com/api/v1/file/hashsum/sha512/${swig_archive_hash}/download"
        "https://insightsoftwareconsortium.github.io/ITKTestingData/CID/${swig_archive_cid}"
        "https://dweb.link/ipfs/${swig_archive_cid}"
        "https://ipfs.io/ipfs/${swig_archive_cid}"
        "https://gateway.pinata.cloud/ipfs/${swig_archive_cid}"
      URL_HASH SHA512=${swig_archive_hash}
      ${swig_extract_timestamp}
    )
    FetchContent_MakeAvailable(itkswig)

    set(
      SWIG_EXECUTABLE
      "${itkswig_SOURCE_DIR}/${swig_exe_subpath}"
      CACHE FILEPATH
      "swig executable."
      FORCE
    )
    set(
      SWIG_DIR
      "${itkswig_SOURCE_DIR}/${swig_dir_subpath}"
      CACHE FILEPATH
      "swig directory."
      FORCE
    )
    mark_as_advanced(SWIG_DIR)

    if(NOT EXISTS "${SWIG_DIR}/swig.swg")
      message(
        FATAL_ERROR
        "Vendored swig library not found at \"${SWIG_DIR}\" (expected swig.swg). The archive layout may not match share/swig/${swig_cmake_version}."
      )
    endif()

    itk_assert_swig_version("${SWIG_EXECUTABLE}" ${swig_version_min})
  endif()
  mark_as_advanced(SWIG_EXECUTABLE)
else()
  # No prebuilt binary for this host: build swig from source.
  if(NOT TARGET swig)
    set(SWIG_VERSION ${ITK_SWIG_VERSION})

    set(
      swig_hash
      "5ca61bd3eac0b3c6a3196afdfc2c4f40d3dce5d626b29543ba5f16560efada1c38b7f53184f79e983b32a0c0ebc26e63fe035b5680fcea0ba9a1de6aa33550c7"
    )
    set(swig_cid "bafybeignhqa2zcb3x4dykjgw6h3ghxxkkd3jh5jn4yxd3uhu7u6mxbf5mi")
    set(swig_install_dir ${CMAKE_CURRENT_BINARY_DIR}/swig)
    set(swig_ep "${swig_install_dir}/bin/swig")

    include(ExternalProject)

    # Set the timestamps of the extracted files to their archived timestamps.
    # Necessary to build configure/autoconf-based projects such as SWIG and
    # PCRE robustly.
    if(${CMAKE_VERSION} VERSION_LESS 3.24)
      set(download_extract_timestamp_flag)
    else()
      set(
        download_extract_timestamp_flag
        DOWNLOAD_EXTRACT_TIMESTAMP
        TRUE
      )
    endif()

    # If we are building ITK
    if(ITK_BINARY_DIR)
      itk_download_attempt_check(PCRE)
    endif()
    set(compiler_information)
    if(NOT CMAKE_CROSSCOMPILING)
      set(
        CMAKE_CXX_COMPILER_LAUNCHER_FLAG
        -DCMAKE_CXX_COMPILER_LAUNCHER:FILEPATH=${CMAKE_CXX_COMPILER_LAUNCHER}
      )
      set(
        CMAKE_C_COMPILER_LAUNCHER_FLAG
        -DCMAKE_C_COMPILER_LAUNCHER:FILEPATH=${CMAKE_C_COMPILER_LAUNCHER}
      )
      set(
        compiler_information
        -DCMAKE_CXX_COMPILER:FILEPATH=${CMAKE_CXX_COMPILER}
        ${CMAKE_CXX_COMPILER_LAUNCHER_FLAG}
        "-DCMAKE_CXX_FLAGS:STRING=${CMAKE_CXX_FLAGS} -w"
        -DCMAKE_C_COMPILER:FILEPATH=${CMAKE_C_COMPILER}
        ${CMAKE_C_COMPILER_LAUNCHER_FLAG}
        "-DCMAKE_C_FLAGS:STRING=${CMAKE_C_FLAGS} -w"
      )
    endif()
    set(
      pcre_hash
      "e1183226ea3e56a1c7ec97e2c7d7ad5a163b9b576930c997e6ac4c26f1c92fc7e9491b5b297b1dd68d43432f7fbbddcc6207a189beb37ef158ad68bad5598988"
    )
    set(pcre_cid "bafybeicnpyjts3vq4lpdfhktr5qioq4qdifmtjkbemyyqpkayxjsqtwz74")
    ExternalProject_Add(
      PCRE
      URL
        "https://data.kitware.com/api/v1/file/hashsum/sha512/${pcre_hash}/download"
        "https://dweb.link/ipfs/${pcre_cid}/pcre-10.43.tar.gz"
        "https://itk.pinata.cloud/ipfs/${pcre_cid}/pcre-10.43.tar.gz"
        "https://w3s.link/ipfs/${pcre_cid}/pcre-10.43.tar.gz"
      URL_HASH SHA512=${pcre_hash}
      CMAKE_GENERATOR "${CMAKE_GENERATOR}"
      CMAKE_CACHE_ARGS
        ${compiler_information} -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
        -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
      INSTALL_DIR ${CMAKE_CURRENT_BINARY_DIR}/PCRE2
      ${download_extract_timestamp_flag}
    )

    # swig uses bison find it by cmake and pass it down
    find_package(BISON)
    set(BISON_FLAGS "" CACHE STRING "Flags used by bison")
    mark_as_advanced(BISON_FLAGS)

    # If we are building ITK
    if(ITK_BINARY_DIR)
      itk_download_attempt_check(SWIG)
    endif()

    ExternalProject_Add(
      swig
      URL
        "https://data.kitware.com/api/v1/file/hashsum/sha512/${swig_hash}/download"
        "https://insightsoftwareconsortium.github.io/ITKTestingData/CID/${swig_cid}"
        "https://dweb.link/ipfs/${swig_cid}"
        "https://ipfs.io/ipfs/${swig_cid}"
        "https://gateway.pinata.cloud/ipfs/${swig_cid}"
      URL_HASH SHA512=${swig_hash}
      CMAKE_GENERATOR "${CMAKE_GENERATOR}"
      CMAKE_CACHE_ARGS
        ${compiler_information} -DCMAKE_INSTALL_PREFIX:PATH=<INSTALL_DIR>
        -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
        -DPCRE2_DIR:PATH=${CMAKE_CURRENT_BINARY_DIR}/PCRE2
      INSTALL_DIR ${CMAKE_CURRENT_BINARY_DIR}/swig
      ${download_extract_timestamp_flag}
      DEPENDS
        PCRE
    )

    set(
      SWIG_DIR
      ${CMAKE_CURRENT_BINARY_DIR}/swig/share/swig/${SWIG_VERSION}
      CACHE FILEPATH
      "swig directory."
    )
    mark_as_advanced(SWIG_DIR)
    set(SWIG_EXECUTABLE ${swig_ep} CACHE FILEPATH "swig executable." FORCE)
  endif()
  mark_as_advanced(SWIG_EXECUTABLE)
endif()

set(
  PYGCCXML_DIR
  ${ITK_CMAKE_DIR}/../Modules/ThirdParty/pygccxml/src
  CACHE INTERNAL
  "pygccxml path"
)

###############################################################################
# store the current dir, so it can be reused later
set(
  ITK_WRAP_SWIGINTERFACE_SOURCE_DIR
  "${CMAKE_CURRENT_SOURCE_DIR}"
  CACHE INTERNAL
  "swig interface source dir"
)
set(
  ITK_WRAP_SWIGINTERFACE_BINARY_DIR
  "${CMAKE_CURRENT_BINARY_DIR}"
  CACHE INTERNAL
  "swig interface binary dir"
)

set(
  WRAPPER_MASTER_INDEX_OUTPUT_DIR
  "${ITK_DIR}/Wrapping/Typedefs"
  CACHE INTERNAL
  "typedefs dir"
)
set(
  IGENERATOR
  "${CMAKE_CURRENT_SOURCE_DIR}/igenerator.py"
  CACHE INTERNAL
  "igenerator.py path"
  FORCE
)

macro(itk_wrap_simple_type_swig_interface wrap_class swig_name)
  string(APPEND SWIG_INTERFACE_TYPEDEFS "using ${swig_name} = ${wrap_class};\n")
endmacro()

include_directories("${WRAPPER_MASTER_INDEX_OUTPUT_DIR}")
