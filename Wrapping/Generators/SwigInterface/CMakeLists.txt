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
if(WIN32)
  set(swig_version_min 4.0.2)
  set(ITK_SWIG_VERSION 4.0.2)
  set(swigwin_hash  "b8f105f9b9db6acc1f6e3741990915b533cd1bc206eb9645fd6836457fd30789b7229d2e3219d8e35f2390605ade0fbca493ae162ec3b4bc4e428b57155db03d")
else()
  set(ITK_SWIG_VERSION 4.0.2)
  set(swig_version_min 4.0.2)
  set(swig_hash     "05e7da70ce6d9a733b96c0bcfa3c1b82765bd859f48c74759bbf4bb1467acb1809caa310cba5e2b3280cd704fca249eaa0624821dffae1d2a75097c7f55d14ed")
endif()

if(WIN32)
  set(swig_ep "${CMAKE_CURRENT_BINARY_DIR}/swigwin-${ITK_SWIG_VERSION}/swig.exe")
else()

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
      ExternalProject_Add(swig
        URL "https://data.kitware.com/api/v1/file/hashsum/sha512/${swigwin_hash}/download"
        URL_HASH SHA512=${swigwin_hash}
        SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR}/swigwin-${SWIG_VERSION}
        CONFIGURE_COMMAND ""
        BUILD_COMMAND ""
        INSTALL_COMMAND ""
        ${download_extract_timestamp_flag}
        )
      set(SWIG_DIR ${CMAKE_CURRENT_BINARY_DIR}/swigwin-${SWIG_VERSION})
    else()
      # From PCRE configure
      # Some influential environment variables:
      #  CC          C compiler command
      #  CFLAGS      C compiler flags
      #  LDFLAGS     linker flags, e.g. -L<lib dir> if you have libraries in a
      #              nonstandard directory <lib dir>
      #  LIBS        libraries to pass to the linker, e.g. -l<library>
      #  CPPFLAGS    (Objective) C/C++ preprocessor flags, e.g. -I<include dir> if
      #              you have headers in a nonstandard directory <include dir>
      #  CXX         C++ compiler command
      #  CXXFLAGS    C++ compiler flags
      #  CPP         C preprocessor
      #  CXXCPP      C++ preprocessor

      # build swig as an external project

      # If we are building ITK
      if(ITK_BINARY_DIR)
        itk_download_attempt_check(PCRE)
      endif()
      set(pcre_env)
      if(NOT CMAKE_CROSSCOMPILING)
        set(pcre_env
          env
            "AR=${CMAKE_AR}"
            "CC=${CMAKE_C_COMPILER_LAUNCHER} ${CMAKE_C_COMPILER}"
            "CFLAGS=${CMAKE_C_FLAGS} ${CMAKE_C_FLAGS_RELEASE} -w"
            "LDFLAGS=$ENV{LDFLAGS}"
            "LIBS=$ENV{LIBS}"
            "CPPFLAGS=$ENV{CPPFLAGS}"
            "CXX=${CMAKE_CXX_COMPILER_LAUNCHER} ${CMAKE_CXX_COMPILER}"
            "CXXFLAGS=${CMAKE_CXX_FLAGS} ${CMAKE_CXX_FLAGS_RELEASE} -w"
            "CPP=$ENV{CPP}"
            "CXXPP=$ENV{CXXPP}"
          )
        if(APPLE)
          # If building on OS X, the compiler must know what version of the OS X SDK to use
          # without SDKROOT set, configuring PCRE fails.  The deployment target is set to
          # ensure the built library is compatible with the correct OS X version.  This may
          # not be strictly necessary for configure, but the compiler determines which
          # header files to use based on both of these settings.  Adding it for safety.
          set(pcre_env ${pcre_env}
            "SDKROOT=${CMAKE_OSX_SYSROOT}"
            "MACOSX_DEPLOYMENT_TARGET=${CMAKE_OSX_DEPLOYMENT_TARGET}"
          )
        endif()
      endif()
      set(extra_external_project_commands)
      if(APPLE)
        # If building on OS X, we have to set the SDKROOT and DEPOLYMENT_TARGET environment variables
        # so that XCode's compilers know which version of the OS X SDK to use.
        list(APPEND extra_external_project_commands
          BUILD_COMMAND
            env
              "SDKROOT=${CMAKE_OSX_SYSROOT}"
               "MACOSX_DEPLOYMENT_TARGET=${CMAKE_OSX_DEPLOYMENT_TARGET}"
            make
          INSTALL_COMMAND
            env
              "SDKROOT=${CMAKE_OSX_SYSROOT}"
              "MACOSX_DEPLOYMENT_TARGET=${CMAKE_OSX_DEPLOYMENT_TARGET}"
            make
              install
        )
      endif()
      set(pcre_hash "abac4c4f9df9e61d7d7761a9c50843882611752e1df0842a54318f358c28f5953025eba2d78997d21ee690756b56cc9f1c04a5ed591dd60654cc78ba16d9ecfb")
      ExternalProject_Add(PCRE
        URL "https://data.kitware.com/api/v1/file/hashsum/sha512/${pcre_hash}/download"
        URL_HASH SHA512=${pcre_hash}
        CONFIGURE_COMMAND
          ${pcre_env}
          ../PCRE/configure
          --prefix=${CMAKE_CURRENT_BINARY_DIR}/PCRE
          --enable-shared=no
        ${extra_external_project_commands}
        ${download_extract_timestamp_flag}
        )

      # swig uses bison find it by cmake and pass it down
      find_package(BISON)
      set(BISON_FLAGS "" CACHE STRING "Flags used by bison")
      mark_as_advanced(BISON_FLAGS)


      # From SWIG configure
      # Some influential environment variables:
      #  CC          C compiler command
      #  CFLAGS      C compiler flags
      #  LDFLAGS     linker flags, e.g. -L<lib dir> if you have libraries in a
      #              nonstandard directory <lib dir>
      #  LIBS        libraries to pass to the linker, e.g. -l<library>
      #  CPPFLAGS    (Objective) C/C++ preprocessor flags, e.g. -I<include dir> if
      #              you have headers in a nonstandard directory <include dir>
      #  CXX         C++ compiler command
      #  CXXFLAGS    C++ compiler flags
      #  CPP         C preprocessor
      #  PCRE_CONFIG config script used for pcre
      #  PCRE_CFLAGS CFLAGS used for pcre
      #  PCRE_LIBS   LIBS used for pcre
      #  YACC        The `Yet Another C Compiler' implementation to use. Defaults to
      #              the first program found out of: `bison -y', `byacc', `yacc'.
      #  YFLAGS      The list of arguments that will be passed by default to $YACC.
      #              This script will default YFLAGS to the empty string to avoid a
      #              default value of `-d' given by some make applications.

      # If we are building ITK
      if(ITK_BINARY_DIR)
        itk_download_attempt_check(SWIG)
      endif()
      # Swig configure step
      # Run in a CMake script because it will be flagged as a false-positive
      # warning when executed with CTEST_USE_LAUNCHERS
      set(swig_env)
      if(NOT CMAKE_CROSSCOMPILING)
        set(swig_env
"
set(ENV{CC} \"${CMAKE_C_COMPILER_LAUNCHER} ${CMAKE_C_COMPILER}\")
set(ENV{CFLAGS} \"${CMAKE_C_FLAGS} ${CMAKE_C_FLAGS_RELEASE} -w\")
set(ENV{LDFLAGS} \"$ENV{LDFLAGS}\")
set(ENV{LIBS} \"$ENV{LIBS}\")
set(ENV{CPPFLAGS} \"$ENV{CPPFLAGS}\")
set(ENV{CXX} \"${CMAKE_CXX_COMPILER_LAUNCHER} ${CMAKE_CXX_COMPILER}\")
set(ENV{CXXFLAGS} \"${CMAKE_CXX_FLAGS} ${CMAKE_CXX_FLAGS_RELEASE} -w\")
set(ENV{CPP} \"$ENV{CPP}\")
set(ENV{YACC} \"${BISON_EXECUTABLE}\")
set(ENV{YFLAGS} \"${BISON_FLAGS}\")
"
          )
      endif()
      set(_swig_configure_script ${CMAKE_CURRENT_BINARY_DIR}/swig_configure_step.cmake)
      file(WRITE ${_swig_configure_script} "
      ${swig_env}
execute_process(COMMAND ../swig/configure
        \"--prefix=${CMAKE_CURRENT_BINARY_DIR}/swig\"
        \"--with-pcre-prefix=${CMAKE_CURRENT_BINARY_DIR}/PCRE\"
  WORKING_DIRECTORY \"${CMAKE_CURRENT_BINARY_DIR}/swig-prefix/src/swig-build\"
  RESULT_VARIABLE result
  OUTPUT_VARIABLE output
  ERROR_VARIABLE error
  )

set(output_file \"${CMAKE_CURRENT_BINARY_DIR}/swig_configure_output.txt\")
file(WRITE \${output_file} \${output})

set(error_file \"${CMAKE_CURRENT_BINARY_DIR}/swig_configure_error.txt\")
file(WRITE \${error_file} \${error})

if(NOT \${result} EQUAL 0)
  message(STATUS \"Swig configure errors detected - See below.\n\${output}\n\${error}\")
  message(FATAL_ERROR \"Swig configure error. See \${output_file} and \${error_file}\")
endif()

message(STATUS \"Swig configure successfully completed.\")
")
      set(extra_swig_configure_env)
      if(APPLE)
        # If building on OS X, the compiler must know what version of the OS X SDK to use
        # without SDKROOT set, configuring swig fails.  The deployment target is set to
        # ensure the built library is compatible with the correct OS X version.  This may
        # not be strictly necessary for configure, but the compiler determines which
        # header files to use based on both of these settings.  Adding it for safety.
        list(APPEND extra_swig_configure_env
          env
            "SDKROOT=${CMAKE_OSX_SYSROOT}"
            "MACOSX_DEPLOYMENT_TARGET=${CMAKE_OSX_DEPLOYMENT_TARGET}"
        )
      endif()

      ExternalProject_Add(swig
        URL "https://data.kitware.com/api/v1/file/hashsum/sha512/${swig_hash}/download"
        URL_HASH SHA512=${swig_hash}
        CONFIGURE_COMMAND
          ${extra_swig_configure_env} ${CMAKE_COMMAND} -P "${_swig_configure_script}"
        ${extra_external_project_commands}
        DEPENDS PCRE
        ${download_extract_timestamp_flag}
        )
    endif()

    set(SWIG_DIR ${CMAKE_CURRENT_BINARY_DIR}/swig/share/swig/${SWIG_VERSION} CACHE FILEPATH "swig directory." FORCE)
    mark_as_advanced(SWIG_DIR)
    set(SWIG_EXECUTABLE ${swig_ep} CACHE FILEPATH "swig executable." FORCE)

  endif()
  mark_as_advanced(SWIG_EXECUTABLE)
endif()


set(PYGCCXML_DIR ${ITK_CMAKE_DIR}/../Modules/ThirdParty/pygccxml/src CACHE INTERNAL "pygccxml path")


###############################################################################
# store the current dir, so it can be reused later
set(ITK_WRAP_SWIGINTERFACE_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}" CACHE INTERNAL "swig interface source dir")
set(ITK_WRAP_SWIGINTERFACE_BINARY_DIR "${CMAKE_CURRENT_BINARY_DIR}" CACHE INTERNAL "swig interface binary dir")

set(WRAPPER_MASTER_INDEX_OUTPUT_DIR "${ITK_DIR}/Wrapping/Typedefs" CACHE INTERNAL "typedefs dir")
set(IGENERATOR  "${CMAKE_CURRENT_SOURCE_DIR}/igenerator.py" CACHE INTERNAL "igenerator.py path" FORCE)


macro(itk_wrap_simple_type_swig_interface wrap_class swig_name)
  string(APPEND SWIG_INTERFACE_TYPEDEFS "using ${swig_name} = ${wrap_class};\n")
endmacro()

include_directories("${WRAPPER_MASTER_INDEX_OUTPUT_DIR}")
