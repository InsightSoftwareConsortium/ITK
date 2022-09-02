project(ITKHDF5)
set(ITKHDF5_THIRD_PARTY 1)

# Reset BUILD_SHARED_LIBS to ITK_BUILD_SHARED_LIBS
# that keeps track of the initial state of the root
# project BUILD_SHARED_LIBS option (which could be
# modified before reaching this point)
set(BUILD_SHARED_LIBS ${ITK_BUILD_SHARED_LIBS})

if (BUILD_SHARED_LIBS)
   add_definitions(-DH5_BUILT_AS_DYNAMIC_LIB=1)
   set(ONLY_SHARED_LIBS ON CACHE INTERNAL "Only build shared")
else()
   set(ONLY_SHARED_LIBS OFF CACHE INTERNAL "Only build static")
endif()
if(CMAKE_LINKER MATCHES "link.exe$")
  # Suppress undefined public symbols warning
  set(CMAKE_STATIC_LINKER_FLAGS "${CMAKE_STATIC_LINKER_FLAGS} /IGNORE:4221")
endif()

if(ITK_USE_SYSTEM_HDF5)

  if(HDF5_DIR)
    set(_HDF5_DIR_CODE "set(HDF5_DIR \"${HDF5_DIR}\")")
  endif()
  if(HDF5_NO_MODULE)
    set(_HDF5_NO_MODULE_ARG "NO_MODULE")
  endif()

  # When ITK's config is loaded, load HDF5 too.
  set(ITKHDF5_EXPORT_CODE_INSTALL "
${_HDF5_DIR_CODE}
find_package(HDF5 ${_HDF5_NO_MODULE_ARG} REQUIRED COMPONENTS CXX C HL)
")
    set(ITKHDF5_EXPORT_CODE_BUILD "
if(NOT ITK_BINARY_DIR)
  ${_HDF5_DIR_CODE}
  find_package(HDF5 ${_HDF5_NO_MODULE_ARG} REQUIRED COMPONENTS CXX C HL)
endif()
")

  if(BUILD_SHARED_LIBS)
    if (TARGET hdf5-shared)
      set(ITKHDF5_LIBRARIES hdf5_cpp-shared hdf5-shared hdf5_hl-shared)
    elseif(TARGET hdf5::hdf5-shared)
      set(ITKHDF5_LIBRARIES hdf5::hdf5_cpp-shared hdf5::hdf5-shared hdf5::hdf5_hl-shared)
    elseif(TARGET hdf5::hdf5)
      set(ITKHDF5_LIBRARIES hdf5::hdf5_cpp hdf5::hdf5 hdf5::hdf5_hl)
    else()
      set(ITKHDF5_LIBRARIES ${HDF5_C_SHARED_LIBRARY} ${HDF5_CXX_SHARED_LIBRARY} ${HDF5_CXX_LIBRARY_NAMES} ${HDF5_HL_SHARED_LIBRARY} ${HDF5_LIBRARIES})
    endif()
  else()
    if (TARGET hdf5-static)
      set(ITKHDF5_LIBRARIES hdf5_cpp-static hdf5-static hdf5_hl-static)
    elseif(TARGET hdf5::hdf5-static)
      set(ITKHDF5_LIBRARIES hdf5::hdf5_cpp-static hdf5::hdf5-static hdf5::hdf5_hl-static)
    elseif(TARGET hdf5::hdf5)
      set(ITKHDF5_LIBRARIES hdf5::hdf5_cpp hdf5::hdf5 hdf5::hdf5_hl)
    else()
      set(ITKHDF5_LIBRARIES ${HDF5_C_STATIC_LIBRARY} ${HDF5_CXX_STATIC_LIBRARY} ${HDF5_CXX_LIBRARY_NAMES} ${HDF5_HL_STATIC_LIBRARY} ${HDF5_LIBRARIES})
    endif()
  endif()

  set(ITKHDF5_INCLUDE_DIRS
    ${ITKHDF5_BINARY_DIR}/src # itk_hdf5.h and itk_H5Cpp.h
  )
  set(ITKHDF5_SYSTEM_INCLUDE_DIRS
    ${HDF5_INCLUDE_DIR}
    ${HDF5_INCLUDE_DIR_HL}
    ${HDF5_INCLUDE_DIRS}
    ${HDF5_INCLUDE_DIR_CPP}
    )

  set(ITKHDF5_NO_SRC 1)
else()
  set(ITKHDF5_INCLUDE_DIRS
    ${ITKHDF5_SOURCE_DIR}/src
    ${ITKHDF5_SOURCE_DIR}/src/itkhdf5/src
    ${ITKHDF5_SOURCE_DIR}/src/itkhdf5/hl/src
    ${ITKHDF5_BINARY_DIR}/src
    ${ITKHDF5_BINARY_DIR}/src/itkhdf5/src
    ${ITKHDF5_BINARY_DIR}/src/itkhdf5/hl/src
    )
  if(BUILD_SHARED_LIBS)
    set(ITKHDF5_LIBRARIES hdf5_cpp-shared hdf5-shared hdf5_hl-shared)
  else()
    set(ITKHDF5_LIBRARIES hdf5_cpp-static hdf5-static hdf5_hl-static)
  endif()
endif()

configure_file(src/itk_hdf5.h.in src/itk_hdf5.h)
configure_file(src/itk_H5Cpp.h.in src/itk_H5Cpp.h)
list(APPEND ITK_LIBRARY_PROPERTIES WINDOWS_EXPORT_ALL_SYMBOLS FALSE)
itk_module_impl()

install(FILES ${ITKHDF5_BINARY_DIR}/src/itk_hdf5.h
              ${ITKHDF5_BINARY_DIR}/src/itk_H5Cpp.h
  DESTINATION ${ITKHDF5_INSTALL_INCLUDE_DIR}
  COMPONENT Development
  )
