project(ITKDCMTK)

set(ITKDCMTK_THIRD_PARTY 1)

# This depends on the external project, nothing locally built
set(ITKDCMTK_NO_SRC 1)

# add to module path to find FindLIBICONV.cmake
set(CMAKE_MODULE_PATH
  ${CMAKE_CURRENT_SOURCE_DIR}/CMake ${CMAKE_MODULE_PATH})

set(ITKDCMTK_PREREQS
  ${ITKDCMTK_BINARY_DIR}/DCMTK_Prereqs)

if(DCMTK_USE_LIBICONV)
  find_package(LIBICONV QUIET)
  if(LIBICONV_FOUND)
    #message("ICONV LIBRARY FOUND: ${LIBICONV_LIBRARIES}")
    get_filename_component(LIBICONV_LIBDIR ${LIBICONV_LIBRARY} PATH)
    set(ICONV_ARGS
      -DLIBICONV_LIBDIR:PATH=${LIBICONV_LIBDIR}
      -DLIBICONV_INCLUDE_DIR:PATH=${LIBICONV_INCLUDE_DIRS}
      -DLIBICONV_LIBRARY:PATH=${LIBICONV_LIBRARY}
      )
  else(LIBICONV_FOUND)
    itk_download_attempt_check(libiconv)
    ExternalProject_Add(libiconv
      URL http://ftp.gnu.org/pub/gnu/libiconv/libiconv-1.14.tar.gz
      URL_MD5 e34509b1623cec449dfeb73d7ce9c6c6
      #  GIT_REPOSITORY git://git.savannah.gnu.org/libiconv.git
      PREFIX ${ITKDCMTK_BINARY_DIR}
      UPDATE_COMMAND ""
      CONFIGURE_COMMAND <SOURCE_DIR>/configure --prefix=${ITKDCMTK_PREREQS}
      --enable-static=yes
      --enable-shared=no
      "CC=${CMAKE_C_COMPILER_LAUNCHER} ${CMAKE_C_COMPILER}"
      "CXX=${CMAKE_CXX_COMPILER_LAUNCHER} ${CMAKE_CXX_COMPILER}"
      )
    set(ICONV_ARGS
      -DDCMTK_WITH_ICONV:BOOL=ON
      -DLIBICONV_LIBDIR:PATH=${ITKDCMTK_PREREQS}/lib
      -DLIBICONV_INCLUDE_DIR:PATH=${ITKDCMTK_PREREQS}/include
      -DLIBICONV_LIBRARY:FILEPATH=${LIBICONV_LIBRARY})

    set(LIBICONV_LIBRARY ${ITKDCMTK_PREREQS}/lib/${lib_prefix}iconv${lib_suffix})
    set(ICONV_DEPENDENCY libiconv)
  endif(LIBICONV_FOUND)

  add_library(ITKDCMTK_iconv STATIC IMPORTED)
  set_property(TARGET ITKDCMTK_iconv PROPERTY
    IMPORTED_LOCATION ${LIBICONV_LIBRARY})
else()
  set(ICONV_ARGS -DDCMTK_WITH_ICONV:BOOL=OFF)
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
  if(NOT WIN32)
    set(lib_prefix lib)
    if(BUILD_SHARED_LIBS)
      set(lib_suffix "${CMAKE_SHARED_LIBRARY_SUFFIX}")
      set(lib_prefix "${CMAKE_SHARED_LIBRARY_PREFIX}")
    else()
      set(lib_suffix "${CMAKE_STATIC_LIBRARY_SUFFIX}")
      set(lib_prefix "${CMAKE_STATIC_LIBRARY_PREFIX}")
    endif()
  else()
    set(lib_prefix "")
    if(BUILD_SHARED_LIBS)
      set(lib_suffix "${CMAKE_IMPORT_LIBRARY_SUFFIX}")
      set(lib_prefix "${CMAKE_IMPORT_LIBRARY_PREFIX}")
    else()
      set(lib_suffix "${CMAKE_STATIC_LIBRARY_SUFFIX}")
      set(lib_prefix "${CMAKE_IMPORT_LIBRARY_PREFIX}")
    endif()
  endif()

  set(DCMTK_EPNAME ITKDCMTK_ExtProject)
  set(lib_dir ${CMAKE_CURRENT_BINARY_DIR}/${DCMTK_EPNAME}-build/lib)

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
      \"\${ITK_INSTALL_PREFIX}/lib/${lib_prefix}\${lib}${lib_suffix}\")
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

  # build the list of libraries upon which DCMTK depends
  set(ITKDCMTK_LIBDEP "")
  foreach(lib ITKJPEG_LIBRARIES ITKTIFF_LIBRARIES ITKPNG_LIBRARIES
      ITKZLIB_LIBRARIES ITKDCMTK_iconv)
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
        list(APPEND BYPRODUCTS "${lib_dir}/${c}/${lib_prefix}${lib}${lib_suffix}")
      endforeach()
    else()
      set_property(TARGET ${lib} PROPERTY IMPORTED_LOCATION
        ${lib_dir}/${lib_prefix}${lib}${lib_suffix})
      list(APPEND BYPRODUCTS "${lib_dir}/${lib_prefix}${lib}${lib_suffix}")
    endif()

    # make the imported library depend on its prerequisite
    # libraries
    set_property(TARGET ${lib} PROPERTY
      IMPORTED_LINK_INTERFACE_LIBRARIES
      ${ITKDCMTK_LIBDEP} )
    # make imported library target depend on external project target
    add_dependencies(${lib} ${DCMTK_EPNAME})
  endforeach()

  if(CMAKE_VERSION VERSION_LESS 3.2)
    if("${CMAKE_GENERATOR}" STREQUAL "Ninja")
      message(FATAL_ERROR "CMake >= 3.2 required with Ninja to compile DCMTK.")
    endif()
    set(build_byproducts)
  else()
    set(build_byproducts  BUILD_BYPRODUCTS ${BYPRODUCTS})
  endif()

  if(CMAKE_VERSION VERSION_LESS 3.4)
    set(CMAKE_CXX_COMPILER_LAUNCHER_FLAG )
    set(CMAKE_C_COMPILER_LAUNCHER_FLAG )
  else()
    set(CMAKE_CXX_COMPILER_LAUNCHER_FLAG -DCMAKE_CXX_COMPILER_LAUNCHER:FILEPATH=${CMAKE_CXX_COMPILER_LAUNCHER})
    set(CMAKE_C_COMPILER_LAUNCHER_FLAG -DCMAKE_C_COMPILER_LAUNCHER:FILEPATH=${CMAKE_C_COMPILER_LAUNCHER})
  endif()
  itk_download_attempt_check(${DCMTK_EPNAME})
  ExternalProject_Add(${DCMTK_EPNAME}
    GIT_REPOSITORY ${DCMTK_GIT_REPOSITORY}
    GIT_TAG ${DCMTK_GIT_TAG}
    SOURCE_DIR ${DCMTK_EPNAME}
    BINARY_DIR ${DCMTK_EPNAME}-build
    LIST_SEPARATOR ":::"
    # PATCH_COMMAND ${CMAKE_COMMAND} -E
    # copy ${CMAKE_CURRENT_LIST_DIR}/CMake/FindLIBICONV.cmake
    # ${CMAKE_CURRENT_BINARY_DIR}/${DCMTK_EPNAME}/CMake/FindICONV.cmake
    INSTALL_COMMAND ""
   CMAKE_ARGS
    -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
    -DBUILD_SHARED_LIBS:BOOL=${BUILD_SHARED_LIBS}
    -DDCMTK_ENABLE_BUILTIN_DICTIONARY:BOOL=ON
    -DDCMTK_WITH_DOXYGEN:BOOL=OFF
    -DDCMTK_WITH_SNDFILE:BOOL=OFF
    -DDCMTK_WITH_WRAP:BOOL=OFF
    -DDCMTK_WITH_XML:BOOL=ON
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
    -DCMAKE_CXX_COMPILER:FILEPATH=${CMAKE_CXX_COMPILER}
    ${CMAKE_CXX_COMPILER_LAUNCHER_FLAG}
    -DCMAKE_C_COMPILER:FILEPATH=${CMAKE_C_COMPILER}
    ${CMAKE_C_COMPILER_LAUNCHER_FLAG}
    -DCMAKE_LIBRARY_OUTPUT_DIRECTORY:PATH=${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
    -DCMAKE_ARCHIVE_OUTPUT_DIRECTORY:PATH=${CMAKE_ARCHIVE_OUTPUT_DIRECTORY}
    -DCMAKE_RUNTIME_OUTPUT_DIRECTORY:PATH=${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
    -DCMAKE_INSTALL_PREFIX:PATH=${CMAKE_INSTALL_PREFIX}
    DEPENDS ${JPEG_DEPENDENCY} ${PNG_DEPENDENCY} ${TIFF_DEPENDENCY} ${ICONV_DEPENDENCY}
    ${build_byproducts}
    )

  # Tell CPack to install DCMTK stuff
  list(APPEND CPACK_INSTALL_CMAKE_PROJECTS
    "${CMAKE_CURRENT_BINARY_DIR}/${DCMTK_EPNAME}-build;DCMTK;ALL;/")

  #
  # run DCMTK's cmake install script
  install(SCRIPT
    ${CMAKE_CURRENT_BINARY_DIR}/${DCMTK_EPNAME}-build/cmake_install.cmake)
endif(ITK_USE_SYSTEM_DCMTK)
