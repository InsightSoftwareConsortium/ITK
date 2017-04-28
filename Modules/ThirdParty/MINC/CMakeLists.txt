project(ITKMINC)
set(ITKMINC_THIRD_PARTY 1)

# For itk_minc2.h
set(ITKMINC_INCLUDE_DIRS ${CMAKE_CURRENT_BINARY_DIR}/src)
if(ITK_USE_SYSTEM_MINC)

  find_package(LIBMINC REQUIRED)

  set(ITKMINC_SYSTEM_INCLUDE_DIRS  ${LIBMINC_INCLUDE_DIRS} )
  set(ITKMINC_SYSTEM_LIBRARY_DIRS  ${LIBMINC_LIBRARY_DIRS} )
  set(ITKMINC_LIBRARIES            "${LIBMINC_LIBRARIES}"  )
  set(ITKMINC_NO_SRC                                    1  )
  # When this module is loaded by an app, load LIBMINC too
  set(ITKMINC_EXPORT_CODE_INSTALL "
set(LIBMINC_DIR \"${LIBMINC_DIR}\")
find_package(LIBMINC REQUIRED)
")
  set(ITKMINC_EXPORT_CODE_BUILD "
if(NOT ITK_BINARY_DIR)
  set(CMAKE_MODULE_PATH \"${CMAKE_CURRENT_SOURCE_DIR}/CMake\" \${CMAKE_MODULE_PATH})

  set(LIBMINC_DIR \"${LIBMINC_DIR}\")
  find_package(LIBMINC REQUIRED)
endif()
")

  set(ITKMINC_INCLUDE_DIRS
    ${ITKMINC_BINARY_DIR}
    ${LIBMINC_INCLUDE_DIRS}
    )

   set(ITKMINC_LIBRARY_DIRS
    ${LIBMINC_LIBRARY_DIRS}
    )

else()

  set(MINC_LIBRARY_PREFIX itk)
  set(MINC_LIBRARY_NAME  ${MINC_LIBRARY_PREFIX}minc2)
  set(ITKMINC_LIBRARIES "${MINC_LIBRARY_NAME}")

  set(ITKMINC_INCLUDE_DIRS
    ${ITKMINC_BINARY_DIR}
    ${ITKMINC_SOURCE_DIR}
    ${ITKMINC_SOURCE_DIR}/src/libminc
    ${ITKMINC_SOURCE_DIR}/src/libminc/libcommon
    ${ITKMINC_SOURCE_DIR}/src/libminc/libsrc2
    ${ITKMINC_SOURCE_DIR}/src/libminc/volume_io/Include
    )

  if(NOT ITK_USE_SYSTEM_HDF5)
    set(ITKMINC_INCLUDE_DIRS ${ITKMINC_INCLUDE_DIRS}
        "${ITKHDF5_SOURCE_DIR}/src/itkhdf5"
        "${ITKHDF5_BINARY_DIR}/src/itkhdf5"
        "${ITKHDF5_SOURCE_DIR}/src/itkhdf5/src"
        )
  endif()
endif()

configure_file(src/itk_minc2.h.in ${ITKMINC_BINARY_DIR}/itk_minc2.h)

itk_module_impl()

install(FILES ${ITKMINC_BINARY_DIR}/itk_minc2.h
  DESTINATION ${ITKMINC_INSTALL_INCLUDE_DIR}
  COMPONENT Development
  )
