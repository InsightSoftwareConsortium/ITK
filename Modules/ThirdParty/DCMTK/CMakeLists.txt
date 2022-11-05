project(ITKDCMTK)

set(ITKDCMTK_THIRD_PARTY 1)

# This depends on the external project, nothing locally built
set(ITKDCMTK_NO_SRC 1)

include(CMakeParseArguments)


if(DCMTK_USE_ICU)
  option(ITK_USE_SYSTEM_ICU "Use an installed version of ICU" OFF)
  if(NOT ITK_USE_SYSTEM_ICU)
    set(ITKDCMTK_PREREQS ${ITKDCMTK_BINARY_DIR}/DCMTK_Prereqs)
    set(ITKDCMTK_ICU_LIBRARIES )
    if(WIN32)
      message(FATAL_ERROR "ICU is not built as part of ITK on Windows. Please compile it outside of ITK.")
    else()
      set(ITKDCMTK_ICU_LIBRARY_NAMES uc data)
    endif()
    foreach(lib_name ${ITKDCMTK_ICU_LIBRARY_NAMES})
      set(lib ICU::${lib_name})
      list(APPEND ITKDCMTK_ICU_LIBRARIES ${lib})
      add_library(${lib} STATIC IMPORTED GLOBAL)
      if(CMAKE_CONFIGURATION_TYPES)
        foreach(c ${CMAKE_CONFIGURATION_TYPES})
          string(TOUPPER "${c}" C)
          set(debug_suffix)
          if("${C}" STREQUAL "DEBUG")
            set(lib_path ${ITKDCMTK_PREREQS}/${c}/lib/${lib_prefix}icu${lib_name}d${CMAKE_STATIC_LIBRARY_SUFFIX})
          else()  # For all non-debug, use release
            set(lib_path ${ITKDCMTK_PREREQS}/Release/lib/${lib_prefix}icu${lib_name}${CMAKE_STATIC_LIBRARY_SUFFIX})
          endif()
          set_property(TARGET ${lib} PROPERTY IMPORTED_LOCATION_${C} ${lib_path})
        endforeach()
      else()
        set(lib_path ${ITKDCMTK_PREREQS}/lib/${lib_prefix}icu${lib_name}${CMAKE_STATIC_LIBRARY_SUFFIX})
        set_property(TARGET ${lib} PROPERTY IMPORTED_LOCATION ${lib_path})
      endif()
      list(APPEND ICU_BYPRODUCTS "${lib_path}")
    endforeach()

    set(ICU_URL "http://download.icu-project.org/files/icu4c/58.2/icu4c-58_2-src.tgz")
    set(ICU_MD5 fac212b32b7ec7ab007a12dff1f3aea1)
    # If not Debug, use Release (for Release, MinSizeRel,...)
    if(CMAKE_CONFIGURATION_TYPES)
      set(ICU_ROOT_DIR ${ITKDCMTK_PREREQS}/$<$<CONFIG:Debug>:Debug>$<$<NOT:$<CONFIG:Debug>>:Release>)
    else()
      set(ICU_ROOT_DIR ${ITKDCMTK_PREREQS})
    endif()
    set(CHARSET_CONVERSION_ARGS
      -DDCMTK_WITH_STDLIBC_ICONV:BOOL=OFF
      -DDCMTK_WITH_ICU:BOOL=ON
      -DDCMTK_ENABLE_CHARSET_CONVERSION:STRING=ICU
      -DICU_ROOT:PATH=${ICU_ROOT_DIR}
      )
    itk_download_attempt_check(icu)
    ExternalProject_Add(icu
    URL ${ICU_URL}
    URL_MD5 ${ICU_MD5}
    PREFIX ${ITKDCMTK_BINARY_DIR}
    UPDATE_COMMAND ""
    BUILD_BYPRODUCTS ${ICU_BYPRODUCTS}
    CONFIGURE_COMMAND <SOURCE_DIR>/source/configure --prefix=${ITKDCMTK_PREREQS}
      --with-data-packaging=static
      --enable-static=yes
      --enable-shared=no
      "CC=${CMAKE_C_COMPILER_LAUNCHER} ${CMAKE_C_COMPILER}"
      "CXX=${CMAKE_CXX_COMPILER_LAUNCHER} ${CMAKE_CXX_COMPILER}"
      "CFLAGS=-fPIC"
      "CXXFLAGS=-fPIC"
    )
    set(ICU_DEPENDENCY icu)
  endif()
else(DCMTK_USE_ICU)
  set(CHARSET_CONVERSION_ARGS
     -DDCMTK_WITH_ICU:BOOL=OFF
     )
endif()

if(APPLE)
  # Character set conversion creates linking errors on MacOS. We disable
  # `iconv` which is likely going to be found on the system by DCMTK, and
  # that is going to create linking errors.
  list(APPEND
      CHARSET_CONVERSION_ARGS
        -DDCMTK_WITH_ICONV:BOOL=OFF
      )
endif()

set(_ITKDCMTK_LIB_NAMES dcmdata dcmimage dcmimgle dcmjpeg dcmjpls
    dcmnet dcmpstat dcmqrdb dcmsr dcmtls ijg12 ijg16 ijg8 oflog ofstd)
if(WIN32)
  set(ITKDCMTK_LIBDEP_WIN iphlpapi ws2_32 netapi32 wsock32)
endif()

#
# if DCMTK is built elsewhere
if(ITK_USE_SYSTEM_DCMTK)
  # assign includes to std module var
  set(ITKDCMTK_SYSTEM_INCLUDE_DIRS ${DCMTK_INCLUDE_DIRS})

  # Module standard library var
  set(ITKDCMTK_LIBRARIES ${DCMTK_LIBRARIES})

  # When this module is loaded by an app, load DCMTK too.
  set(ITKDCMTK_EXPORT_CODE_INSTALL "
set(DCMTK_DIR \"${DCMTK_DIR}\")
find_package(DCMTK REQUIRED NO_MODULE)
")
  set(ITKDCMTK_EXPORT_CODE_BUILD "
if(NOT ITK_BINARY_DIR)
  set(CMAKE_MODULE_PATH
    \"${CMAKE_CURRENT_SOURCE_DIR}/CMake\" \${CMAKE_MODULE_PATH})
  set(DCMTK_DIR \"${DCMTK_DIR}\")
  find_package(DCMTK REQUIRED NO_MODULE)
endif()
")

  itk_module_impl()

else(ITK_USE_SYSTEM_DCMTK)
  set(DCMTK_EPNAME ITKDCMTK_ExtProject)
  set(lib_dir ${CMAKE_CURRENT_BINARY_DIR}/${DCMTK_EPNAME}-build/lib)

  if(BUILD_SHARED_LIBS)
    set(_ITKDCMTK_LIB_LINKAGE SHARED)
  else()
    set(_ITKDCMTK_LIB_LINKAGE STATIC)
  endif()
  foreach(lib ${_ITKDCMTK_LIB_NAMES})
    # add it as an imported  library target
    add_library(${lib} ${_ITKDCMTK_LIB_LINKAGE} IMPORTED GLOBAL)
  endforeach()

  # Use DCMTK include files in place in the build directory.
  set(ITKDCMTK_INCLUDE ${CMAKE_CURRENT_BINARY_DIR}/${DCMTK_EPNAME})

  # 'stringize' the libraries. Brad King addition
  set(ITKDCMTK_LIBRARIES "${_ITKDCMTK_LIB_NAMES}")
  #
  # add all the embedded include directories to include dirs
  foreach(lib ${_ITKDCMTK_LIB_NAMES})
    # add to include list
    list(APPEND ITKDCMTK_INCLUDE_DIRS
      ${ITKDCMTK_INCLUDE}/${lib}/include)
  endforeach()

  #
  # need the base include dir as well.
  list(APPEND ITKDCMTK_INCLUDE_DIRS
    ${CMAKE_CURRENT_BINARY_DIR}/${DCMTK_EPNAME}-build/config/include)

  set(ITKDCMTK_EXPORT_CODE_BUILD "
set(CMAKE_MODULE_PATH
  \"${CMAKE_CURRENT_SOURCE_DIR}/CMake\" \${CMAKE_MODULE_PATH})
")

set(ITKDCMTK_LINK_DEPENDENCIES ${ITKDCMTK_LIBDEP_WIN})
  # create imported targets when module is loaded from build tree
  if(CMAKE_CONFIGURATION_TYPES)
    set(ITKDCMTK_EXPORT_CODE_BUILD "
${ITKDCMTK_EXPORT_CODE_BUILD}
foreach(lib ${_ITKDCMTK_LIB_NAMES})
  if(NOT TARGET \${lib})
    add_library(\${lib} ${_ITKDCMTK_LIB_LINKAGE} IMPORTED)
    foreach(c ${CMAKE_CONFIGURATION_TYPES})
      string(TOUPPER \"\${c}\" C)
      set_property(TARGET \${lib} PROPERTY IMPORTED_LOCATION_\${C}
        \"${lib_dir}/\${c}/${lib_prefix}\${lib}${lib_suffix}\")
    set_property(TARGET \${lib} PROPERTY
      IMPORTED_LINK_INTERFACE_LIBRARIES
      ${ITKDCMTK_LINK_DEPENDENCIES})
    endforeach()
  endif()
endforeach()
")
  else()
    set(ITKDCMTK_EXPORT_CODE_BUILD "
${ITKDCMTK_EXPORT_CODE_BUILD}
foreach(lib ${_ITKDCMTK_LIB_NAMES})
  if(NOT TARGET \${lib})
    add_library(\${lib} ${_ITKDCMTK_LIB_LINKAGE} IMPORTED)
    set_property(TARGET \${lib} PROPERTY IMPORTED_LOCATION
      \"${lib_dir}/${lib_prefix}\${lib}${lib_suffix}\")
    set_property(TARGET \${lib} PROPERTY
      IMPORTED_LINK_INTERFACE_LIBRARIES
      ${ITKDCMTK_LINK_DEPENDENCIES})
  endif()
endforeach()
")
  endif()
  # create imported targets when module is loaded from install tree
  set(ITKDCMTK_EXPORT_CODE_INSTALL "
foreach(lib ${_ITKDCMTK_LIB_NAMES})
  if(NOT TARGET \${lib})
    add_library(\${lib} ${_ITKDCMTK_LIB_LINKAGE} IMPORTED)
    set_property(TARGET \${lib} PROPERTY IMPORTED_LOCATION
            \"\${ITK_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/${lib_prefix}\${lib}${lib_suffix}\")
    set_property(TARGET \${lib} PROPERTY
      IMPORTED_LINK_INTERFACE_LIBRARIES
      ${ITKDCMTK_LINK_DEPENDENCIES})
  endif()
endforeach()
")

  # implement module before the ExternalProject, to process
  # dependencies
  itk_module_impl()

  foreach(libdep JPEG TIFF ZLIB PNG)
    #
    # if we're using ITK-built versions of libraries, then
    # have to find the actual library name, instead of the cmake logical name
    set(DCMTK${libdep}_LIBRARIES "")

    foreach(_lib ${ITK${libdep}_LIBRARIES})
      if(TARGET ${_lib})
        set(_lib $<TARGET_FILE:${_lib}>)
      endif()
      list(APPEND DCMTK${libdep}_LIBRARIES ${_lib})
    endforeach()

    # have to replace ; with another separator in order to pass lists into
    # the external project without them getting messed up.
    string(REPLACE ";" ":::" DCMTK${libdep}_LIBRARIES "${DCMTK${libdep}_LIBRARIES}")
    string(REPLACE ";" ":::" DCMTK${libdep}_INCLUDE_DIRS "${ITK${libdep}_INCLUDE_DIRS}")
  endforeach()

  include(${CMAKE_CURRENT_LIST_DIR}/DCMTKGitTag.cmake)

  set(DCMTK_EP_FLAGS
    -DCMAKE_CXX_STANDARD:STRING=${CMAKE_CXX_STANDARD}
    -DCMAKE_CXX_STANDARD_REQUIRED:BOOL=${CMAKE_CXX_STANDARD_REQUIRED}
    -DCMAKE_CXX_EXTENSIONS:BOOL=${CMAKE_CXX_EXTENSIONS}
    -DDCMTK_ENABLE_CXX11:BOOL=ON
    -DDCMTK_FORCE_FPIC_ON_UNIX:BOOL=ON
    -DBUILD_APPS:BOOL=OFF        # Only DCMTK libraries are needed
    -DDCMTK_WITH_OPENSSL:BOOL=OFF
    -DDCMTK_WITH_PNG:BOOL=ON
    -DDCMTK_WITH_XML:BOOL=OFF
    -DDCMTK_WITH_TIFF:BOOL=ON
    -DDCMTK_WITH_ZLIB:BOOL=ON)

  if(MSVC)
    list(APPEND DCMTK_EP_FLAGS -DDCMTK_OVERWRITE_WIN32_COMPILER_FLAGS:BOOL=OFF)
  endif()

  if("${CMAKE_GENERATOR}" MATCHES "Unix Makefiles")
    list(APPEND DCMTK_EP_FLAGS -DCMAKE_VERBOSE_MAKEFILE:BOOL=${CMAKE_VERBOSE_MAKEFILE})
  endif()


  # build the list of libraries upon which DCMTK depends
  set(ITKDCMTK_LIBDEP "")
  list(APPEND ITKDCMTK_LIBDEP ${ITKDCMTK_LIBDEP_WIN})
  foreach(lib ITKJPEG_LIBRARIES ITKTIFF_LIBRARIES ITKPNG_LIBRARIES
      ITKZLIB_LIBRARIES ITKDCMTK_ICU_LIBRARIES)
    foreach(_lib ${${lib}})
      if(TARGET ${_lib})
        list(APPEND ITKDCMTK_LIBDEP ${_lib})
      endif()
    endforeach()
  endforeach()
  # attach actual filenames to the
  # imported libraries from the ExternalProject
  foreach(lib ${_ITKDCMTK_LIB_NAMES})
    # tell the imported library where it's file lives
    if(CMAKE_CONFIGURATION_TYPES)
      foreach(c ${CMAKE_CONFIGURATION_TYPES})
        string(TOUPPER "${c}" C)
        set_property(TARGET ${lib} PROPERTY IMPORTED_LOCATION_${C}
          ${lib_dir}/${c}/${lib_prefix}${lib}${lib_suffix})
        list(APPEND DCMTK_BYPRODUCTS "${lib_dir}/${c}/${lib_prefix}${lib}${lib_suffix}")
      endforeach()
    else()
      set_property(TARGET ${lib} PROPERTY IMPORTED_LOCATION
        ${lib_dir}/${lib_prefix}${lib}${lib_suffix})
      list(APPEND DCMTK_BYPRODUCTS "${lib_dir}/${lib_prefix}${lib}${lib_suffix}")
    endif()

    # make the imported library depend on its prerequisite
    # libraries
    set_property(TARGET ${lib} PROPERTY
      IMPORTED_LINK_INTERFACE_LIBRARIES
      ${ITKDCMTK_LIBDEP} )
  endforeach()

  if(CMAKE_VERSION VERSION_LESS 3.4)
    set(CMAKE_CXX_COMPILER_LAUNCHER_FLAG )
    set(CMAKE_C_COMPILER_LAUNCHER_FLAG )
  else()
    set(CMAKE_CXX_COMPILER_LAUNCHER_FLAG -DCMAKE_CXX_COMPILER_LAUNCHER:FILEPATH=${CMAKE_CXX_COMPILER_LAUNCHER})
    set(CMAKE_C_COMPILER_LAUNCHER_FLAG -DCMAKE_C_COMPILER_LAUNCHER:FILEPATH=${CMAKE_C_COMPILER_LAUNCHER})
  endif()
  set(_cross_compiling_flags)
  if(CMAKE_TOOLCHAIN_FILE)
    list(APPEND _cross_compiling_flags -DCMAKE_TOOLCHAIN_FILE:FILEPATH=${CMAKE_TOOLCHAIN_FILE})
  endif()
  if(CMAKE_CROSSCOMPILING_EMULATOR)
    list(APPEND _cross_compiling_flags -DCMAKE_CROSSCOMPILING_EMULATOR:FILEPATH=${CMAKE_CROSSCOMPILING_EMULATOR})
  endif()
  itk_download_attempt_check(${DCMTK_EPNAME})
  ExternalProject_Add(${DCMTK_EPNAME}
    GIT_REPOSITORY ${DCMTK_GIT_REPOSITORY}
    GIT_TAG ${DCMTK_GIT_TAG}
    SOURCE_DIR ${DCMTK_EPNAME}
    BINARY_DIR ${DCMTK_EPNAME}-build
    LIST_SEPARATOR ":::"
    INSTALL_COMMAND ""
   CMAKE_ARGS
    -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
    -DBUILD_SHARED_LIBS:BOOL=${BUILD_SHARED_LIBS}
    -DCMAKE_MSVC_RUNTIME_LIBRARY:STRING=${CMAKE_MSVC_RUNTIME_LIBRARY}
    -DDCMTK_DEFAULT_DICT:STRING=builtin
    -DDCMTK_WITH_DOXYGEN:BOOL=OFF
    -DDCMTK_WITH_SNDFILE:BOOL=OFF
    -DDCMTK_WITH_WRAP:BOOL=OFF
    -DDCMTK_ENABLE_PRIVATE_TAGS:BOOL=ON
    -DJPEG_LIBRARY:PATH=${DCMTKJPEG_LIBRARIES}
    -DJPEG_INCLUDE_DIR:PATH=${DCMTKJPEG_INCLUDE_DIRS}
    -DTIFF_INCLUDE_DIR:STRING=${DCMTKTIFF_INCLUDE_DIRS}
    -DTIFF_LIBRARY:PATH=${DCMTKTIFF_LIBRARIES}
    -DZLIB_INCLUDE_DIRS:STRING=${DCMTKZLIB_INCLUDE_DIRS}
    -DZLIB_LIBS:PATH=${DCMTKZLIB_LIBRARIES}
    -DPNG_INCLUDE_DIR:STRING=${DCMTKPNG_INCLUDE_DIRS}
    -DPNG_LIBRARY:PATH=${DCMTKPNG_LIBRARIES}
    ${DCMTK_EP_FLAGS}
    ${_cross_compiling_flags}
    -DCMAKE_CXX_COMPILER:FILEPATH=${CMAKE_CXX_COMPILER}
    ${CMAKE_CXX_COMPILER_LAUNCHER_FLAG}
    -DCMAKE_C_COMPILER:FILEPATH=${CMAKE_C_COMPILER}
    ${CMAKE_C_COMPILER_LAUNCHER_FLAG}
    -DCMAKE_LIBRARY_OUTPUT_DIRECTORY:PATH=${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
    -DCMAKE_ARCHIVE_OUTPUT_DIRECTORY:PATH=${CMAKE_ARCHIVE_OUTPUT_DIRECTORY}
    -DCMAKE_RUNTIME_OUTPUT_DIRECTORY:PATH=${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
    -DCMAKE_INSTALL_PREFIX:PATH=${CMAKE_INSTALL_PREFIX}
    -DCMAKE_INSTALL_LIBDIR:PATH=${CMAKE_INSTALL_LIBDIR}
    -DCMAKE_INSTALL_BINDIR:PATH=${CMAKE_INSTALL_BINDIR}
    ${CHARSET_CONVERSION_ARGS}
    DEPENDS ${JPEG_DEPENDENCY} ${PNG_DEPENDENCY} ${TIFF_DEPENDENCY} ${ICU_DEPENDENCY}
    BUILD_BYPRODUCTS ${DCMTK_BYPRODUCTS}
    )
  foreach(lib ${_ITKDCMTK_LIB_NAMES})
    # make imported library target depend on external project target
    add_dependencies(${lib} ${DCMTK_EPNAME})
  endforeach()

  # Tell CPack to install DCMTK stuff
  list(APPEND CPACK_INSTALL_CMAKE_PROJECTS
    "${CMAKE_CURRENT_BINARY_DIR}/${DCMTK_EPNAME}-build;DCMTK;ALL;/")

  #
  # run DCMTK's cmake install script
  install(SCRIPT
    ${CMAKE_CURRENT_BINARY_DIR}/${DCMTK_EPNAME}-build/cmake_install.cmake)
endif(ITK_USE_SYSTEM_DCMTK)
