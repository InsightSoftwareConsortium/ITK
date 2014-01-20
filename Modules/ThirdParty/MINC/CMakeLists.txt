project(ITKMINC)
set(ITKMINC_THIRD_PARTY 1)

# This depends on the external project, nothing locally built
set(ITKMINC_NO_SRC 1)

# For itk_minc2.h
set(ITKMINC_INCLUDE_DIRS ${CMAKE_CURRENT_BINARY_DIR}/src)
if(ITK_USE_SYSTEM_MINC)

  find_package(LIBMINC REQUIRED)

  set(ITKMINC_SYSTEM_INCLUDE_DIRS
    ${LIBMINC_INCLUDE_DIRS}
    )

  set(ITKMINC_SYSTEM_LIBRARY_DIRS
    ${LIBMINC_LIBRARY_DIRS}
  )

  set(ITKMINC_LIBRARIES "${VOLUME_IO_LIBRARIES}" "${LIBMINC_LIBRARIES}")
  itk_module_impl()
else()
  set(MINC_EPNAME ITKMINC2_ExternalProject)
  set(lib_dir ${CMAKE_CURRENT_BINARY_DIR}/${MINC_EPNAME}-build)

  set(MINC_LIBRARY_PREFIX itk_)
  set(MINC_LIBRARY_NAME ${MINC_LIBRARY_PREFIX}minc2)
  set(ITKMINC_LIBRARIES "${MINC_LIBRARY_NAME}")

  set(ITKMINC_INCLUDE_DIRS
    ${ITKMINC_INCLUDE_DIRS}
    ${CMAKE_CURRENT_BINARY_DIR}/${MINC_EPNAME}
    ${CMAKE_CURRENT_BINARY_DIR}/${MINC_EPNAME}/volume_io/Include
    )

  if(NOT ITK_USE_SYSTEM_HDF5)
    set(ITKMINC_INCLUDE_DIRS ${ITKMINC_INCLUDE_DIRS}
        "${ITKHDF5_SOURCE_DIR}/src/itkhdf5"
        "${ITKHDF5_BINARY_DIR}/src/itkhdf5"
        )
  endif()

  set(MINC_CMAKE_ARGS )

  if(NOT WIN32)
    set(lib_prefix lib)
    set(lib_suffix "${CMAKE_STATIC_LIBRARY_SUFFIX}")
    set(lib_prefix "${CMAKE_STATIC_LIBRARY_PREFIX}")
  else()
    set(MINC_CMAKE_ARGS "-DCMAKE_CXX_FLAGS:STRING=-D_CRT_SECURE_NO_WARNINGS" "-DCMAKE_C_FLAGS:STRING=-D_CRT_SECURE_NO_WARNINGS")
    set(lib_prefix "")
    set(lib_suffix "${CMAKE_STATIC_LIBRARY_SUFFIX}")
    set(lib_prefix "${CMAKE_IMPORT_LIBRARY_PREFIX}")
  endif()

  # create imported targets when module is loaded from build tree
  if(CMAKE_CONFIGURATION_TYPES)
    set(ITKMINC_EXPORT_CODE_BUILD "
if(NOT TARGET ${MINC_LIBRARY_NAME})
  add_library(${MINC_LIBRARY_NAME} ${_ITKMINC_LIB_LINKAGE} IMPORTED)
  foreach(c ${CMAKE_CONFIGURATION_TYPES})
    string(TOUPPER \"\${c}\" C)
    set_property(TARGET ${MINC_LIBRARY_NAME} PROPERTY IMPORTED_LOCATION_\${C}
      \"${lib_dir}/\${c}/${lib_prefix}\${MINC_LIBRARY_NAME}${lib_suffix}\")
  endforeach()
endif()
")
  else()
    set(ITKMINC_EXPORT_CODE_BUILD "
if(NOT TARGET ${MINC_LIBRARY_NAME})
  add_library(${MINC_LIBRARY_NAME} ${_ITKMINC_LIB_LINKAGE} IMPORTED)
  set_property(TARGET ${MINC_LIBRARY_NAME} PROPERTY IMPORTED_LOCATION
    \"${lib_dir}/${lib_prefix}${MINC_LIBRARY_NAME}${lib_suffix}\")
endif()
")
  endif()

  # create imported targets when module is loaded from install tree
  set(ITKMINC_EXPORT_CODE_INSTALL "
if(NOT TARGET ${MINC_LIBRARY_NAME})
  add_library(${MINC_LIBRARY_NAME} ${_ITKMINC_LIB_LINKAGE} IMPORTED)
  set_property(TARGET ${MINC_LIBRARY_NAME} PROPERTY IMPORTED_LOCATION
    \"\${ITK_INSTALL_PREFIX}/lib/${lib_prefix}${MINC_LIBRARY_NAME}${lib_suffix}\")
endif()
")

  itk_module_impl()

  foreach(libdep HDF5 ZLIB)
    #
    # if we're using ITK-built versions of libraries, then
    # have to find the actual library name, instead of the cmake logical name
    set(MINC${libdep}_LIBRARIES "")

    foreach(_lib ${ITK${libdep}_LIBRARIES})
      if(TARGET ${_lib})
        set(_lib $<TARGET_FILE:${_lib}>)
      endif()
      list(APPEND MINC${libdep}_LIBRARIES ${_lib})
    endforeach()

    # have to replace ; with another separator in order to pass lists into
    # the external project without them getting messed up.
    string(REPLACE ";" ":::" MINC${libdep}_LIBRARIES "${MINC${libdep}_LIBRARIES}")
    string(REPLACE ";" ":::" MINC${libdep}_INCLUDE_DIRS "${ITK${libdep}_INCLUDE_DIRS}")
  endforeach()

  set(MINC_GIT_REPOSITORY "https://github.com/BIC-MNI/libminc.git")
  set(MINC_GIT_TAG "0cb35c353217a38f647f65afaf53e2af3253d912")
  set(MINC_HDF5_ARGS )

  if(NOT ITK_USE_SYSTEM_HDF5)
    set(MINC_HDF5_ARGS  "-DHDF5_INCLUDE_DIR:PATH=${ITKHDF5_SOURCE_DIR}/src/itkhdf5:::${ITKHDF5_BINARY_DIR}/src")
    set(MINC_CMAKE_ARGS ${MINC_CMAKE_ARGS} "-DHDF5_LIBRARY:FILEPATH=${MINCHDF5_LIBRARIES}")
  endif()
  if(NOT ITK_USE_SYSTEM_ZLIB)
    set(MINC_CMAKE_ARGS ${MINC_CMAKE_ARGS} "-DZLIB_LIBRARY:FILEPATH=${MINCZLIB_LIBRARIES}")
  endif()

  ExternalProject_Add(${MINC_EPNAME}
    GIT_REPOSITORY ${MINC_GIT_REPOSITORY}
    GIT_TAG ${MINC_GIT_TAG}
#    URL ${CMAKE_CURRENT_SOURCE_DIR}/../../../../libminc
    SOURCE_DIR ${MINC_EPNAME}
    BINARY_DIR ${MINC_EPNAME}-build
    INSTALL_COMMAND ""
    LIST_SEPARATOR ":::"
    CMAKE_ARGS
      ${MINC_CMAKE_ARGS}
      ${MINC_HDF5_ARGS}
      -DLIBMINC_EXTERNALLY_CONFIGURED:BOOL=ON
      -DLIBMINC_EXTERNAL_LIB_PREFIX:STRING=${MINC_LIBRARY_PREFIX}
      -DLIBMINC_BUILD_SHARED_LIBS:BOOL=OFF # ${BUILD_SHARED_LIBS} VF: Avoid conflict with potential system minc library
      -DLIBMINC_EXPORTED_TARGETS:STRING=${ITKMINC-targets}
      -DCMAKE_CXX_COMPILER:FILEPATH=${CMAKE_CXX_COMPILER}
      -DCMAKE_C_COMPILER:FILEPATH=${CMAKE_C_COMPILER}
      -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
      -DLIBMINC_INSTALL_LIB_DIR:PATH=${ITKMINC_INSTALL_LIBRARY_DIR}
      #-DCMAKE_ARCHIVE_OUTPUT_DIRECTORY:PATH=${CMAKE_ARCHIVE_OUTPUT_DIRECTORY}
      #-DCMAKE_RUNTIME_OUTPUT_DIRECTORY:PATH=${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
      -DCMAKE_INSTALL_PREFIX:PATH=${CMAKE_INSTALL_PREFIX}
    )

  # build the list of libraries upon which MINC depends
  set(ITKMINC_LIBDEP "")
  foreach(lib ITKHDF5_LIBRARIES ITKZLIB_LIBRARIES)
    foreach(_lib ${${lib}})
      if(TARGET ${_lib})
        list(APPEND ITKMINC_LIBDEP ${_lib})
      endif()
    endforeach()
  endforeach()

  if(CMAKE_CONFIGURATION_TYPES)
    foreach(c ${CMAKE_CONFIGURATION_TYPES})
      string(TOUPPER "${c}" C)
      set_property(TARGET ${MINC_LIBRARY_NAME} PROPERTY IMPORTED_LOCATION_${C}
        ${lib_dir}/${c}/${lib_prefix}${MINC_LIBRARY_NAME}${lib_suffix})
    endforeach()
  else()
    set_property(TARGET ${MINC_LIBRARY_NAME} PROPERTY IMPORTED_LOCATION
      ${lib_dir}/${lib_prefix}${MINC_LIBRARY_NAME}${lib_suffix})
  endif()
  get_target_property(debug_location ${MINC_LIBRARY_NAME} IMPORTED_LOCATION)

  # make the imported library depend on its prerequisite
  # libraries
  set_property(TARGET ${MINC_LIBRARY_NAME} PROPERTY
    IMPORTED_LINK_INTERFACE_LIBRARIES
    ${ITKMINC_LIBDEP} )
  # make imported library target depend on external project target
  add_dependencies(${MINC_LIBRARY_NAME} ${MINC_EPNAME})

  # Tell CPack to install DCMTK stuff
  list(APPEND CPACK_INSTALL_CMAKE_PROJECTS
    "${CMAKE_CURRENT_BINARY_DIR}/${MINC_EPNAME}-build;MINC;ALL;/")

  #
  # run MINC's cmake install script
  install(SCRIPT
    ${CMAKE_CURRENT_BINARY_DIR}/${MINC_EPNAME}-build/cmake_install.cmake)

endif()

configure_file(src/itk_minc2.h.in src/itk_minc2.h)

if(NOT ITK_INSTALL_NO_DEVELOPMENT)
  install(FILES ${ITKMINC_BINARY_DIR}/src/itk_minc2.h
    DESTINATION ${ITKMINC_INSTALL_INCLUDE_DIR})
endif(NOT ITK_INSTALL_NO_DEVELOPMENT)
