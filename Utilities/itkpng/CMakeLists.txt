project(ITKPNG)
include_regular_expression("^(png|itk|z).*$")

include_directories(${ITKPNG_SOURCE_DIR})

# source files for png
set(PNG_SRCS
pngget.c    pngrio.c    pngwrite.c
png.c       pngmem.c    pngrtran.c  pngtrans.c  pngwtran.c
pngerror.c  pngpread.c  pngrutil.c  pngwutil.c
pngread.c   pngset.c   pngwio.c
)

if(WIN32)
  if(BUILD_SHARED_LIBS)
    add_definitions(-DPNG_BUILD_DLL)
    set(PNG_NO_MODULEDEF 1)
  else(BUILD_SHARED_LIBS)
    set(PNG_STATIC 1)
  endif(BUILD_SHARED_LIBS)
endif(WIN32)

configure_file(${ITKPNG_SOURCE_DIR}/.NoDartCoverage
               ${ITKPNG_BINARY_DIR}/.NoDartCoverage)
configure_file(${ITKPNG_SOURCE_DIR}/pngDllConfig.h.in
               ${ITKPNG_BINARY_DIR}/pngDllConfig.h)

add_library(itkpng ${PNG_SRCS})
target_link_libraries(itkpng ${ITK_ZLIB_LIBRARIES})

# Apply user-defined properties to the library target.
if(ITK_LIBRARY_PROPERTIES)
  set_target_properties(itkpng PROPERTIES ${ITK_LIBRARY_PROPERTIES})
endif(ITK_LIBRARY_PROPERTIES)

if(NOT ITK_INSTALL_NO_LIBRARIES)
  install(TARGETS itkpng
    RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT RuntimeLibraries
    LIBRARY DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT RuntimeLibraries
    ARCHIVE DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT Development)
endif(NOT ITK_INSTALL_NO_LIBRARIES)

if(ITK_USE_FLAT_DIRECTORY_INSTALL)
  set( ITK_INSTALL_INCLUDE_DIR_CM24_UTILITIES_ITKPNG ${ITK_INSTALL_INCLUDE_DIR_CM24})
else(ITK_USE_FLAT_DIRECTORY_INSTALL)
  set( ITK_INSTALL_INCLUDE_DIR_CM24_UTILITIES_ITKPNG ${ITK_INSTALL_INCLUDE_DIR_CM24}/Utilities/itkpng)
endif(ITK_USE_FLAT_DIRECTORY_INSTALL)

if(NOT ITK_INSTALL_NO_DEVELOPMENT)
  install(FILES
    ${ITKPNG_BINARY_DIR}/pngDllConfig.h
    ${ITKPNG_SOURCE_DIR}/png.h
    ${ITKPNG_SOURCE_DIR}/pngconf.h
    ${ITKPNG_SOURCE_DIR}/itk_png_mangle.h
    DESTINATION ${ITK_INSTALL_INCLUDE_DIR_CM24_UTILITIES_ITKPNG}
    COMPONENT Development)
endif(NOT ITK_INSTALL_NO_DEVELOPMENT)

if(CMAKE_COMPILER_IS_GNUCC)
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wno-cast-qual")
endif(CMAKE_COMPILER_IS_GNUCC)
