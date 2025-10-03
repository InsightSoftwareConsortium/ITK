set(ITK3P_INSTALL_EXPORT_NAME "${ITKNrrdIO-targets}")
set(ITK3P_INSTALL_INCLUDE_DIR "${ITKNrrdIO_INSTALL_INCLUDE_DIR}")
set(ITK3P_INSTALL_RUNTIME_DIR "${ITKNrrdIO_INSTALL_RUNTIME_DIR}")
set(ITK3P_INSTALL_LIBRARY_DIR "${ITKNrrdIO_INSTALL_LIBRARY_DIR}")
set(ITK3P_INSTALL_ARCHIVE_DIR "${ITKNrrdIO_INSTALL_ARCHIVE_DIR}")

set(NRRDIO_PREFIX ITK)
# ZLIB:
set(NRRDIO_USE_ZLIB  OFF)
set(TEEM_ITK_MANGLE 1) # Needed to generate public facing ITKNrrdIO.h from ITKNrrdIO.h.in with TEEM_ITK_MANGLE=1 always
add_definitions(-DTEEM_ITK_MANGLE=${TEEM_ITK_MANGLE}) # Needed for privateNrrd.h (only used when building library, no public api).
add_definitions(-DTEEM_ZLIB=1) # Force setting zlib support here
set(ZLIB_INCLUDE_DIR ${ITKZLIB_INCLUDE_DIRS})
set(ZLIB_LIBRARY ${ITKZLIB_LIBRARIES})
list(APPEND NRRDIO_COMPRESSION_LIBRARIES ${ITKZLIB_LIBRARIES})
if(UNIX)
    list(APPEND NRRDIO_COMPRESSION_LIBRARIES m)
    list(REMOVE_DUPLICATES NRRDIO_COMPRESSION_LIBRARIES)
endif()

# BZIP2
set(NRRDIO_USE_BZIP2 OFF) # Not yet supported in ITK
add_definitions(-UTEEM_BZIP2) # Force unsetting bzip2 support here

add_subdirectory(NrrdIO)
itk_module_target(ITKNrrdIO NO_INSTALL)
