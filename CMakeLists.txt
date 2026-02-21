# CMakeFiles.txt for the libminc library
#
# Andrew Janke - a.janke@gmail.com
# Vladimir S. FONOV - vladimir.fonov@gmail.com

cmake_minimum_required(VERSION 3.10...4.2)

project(LIBMINC)

set(LIBMINC_PACKAGE_VERSION_MAJOR 2)
set(LIBMINC_PACKAGE_VERSION_MINOR 4)
set(LIBMINC_PACKAGE_VERSION_PATCH 06)

set(LIBMINC_SOVERSION "5.3.0")

set(LIBMINC_PACKAGE "libminc")
set(LIBMINC_PACKAGE_BUGREPORT "vladimir.fonov@gmail.com")

set(LIBMINC_PACKAGE_NAME    "libminc")
set(LIBMINC_PACKAGE_VERSION "${LIBMINC_PACKAGE_VERSION_MAJOR}.${LIBMINC_PACKAGE_VERSION_MINOR}.${LIBMINC_PACKAGE_VERSION_PATCH}")
set(LIBMINC_PACKAGE_STRING  "${LIBMINC_PACKAGE_NAME} ${LIBMINC_PACKAGE_VERSION}")

include(CTest)

enable_testing()

include(GNUInstallDirs)

if(MINC_TOOLKIT_BUILD)
  set(LIBMINC_EXTERNALLY_CONFIGURED ON)
endif()

set(LIBMINC_BUILD_V2 ON)

if(NOT LIBMINC_EXTERNALLY_CONFIGURED)
  # Allowing User to configure parameters
  set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH}" "${CMAKE_CURRENT_SOURCE_DIR}/cmake-modules")

  set(CPACK_GENERATOR TGZ)
  set(CPACK_PACKAGE_VERSION_MAJOR ${LIBMINC_PACKAGE_VERSION_MAJOR})
  set(CPACK_PACKAGE_VERSION_MINOR ${LIBMINC_PACKAGE_VERSION_MINOR})
  set(CPACK_PACKAGE_VERSION_PATCH ${LIBMINC_PACKAGE_VERSION_PATCH})
  set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/COPYING")
  set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Medical Imaging library core IO")

  option(LIBMINC_BUILD_SHARED_LIBS   "Build libminc with shared libraries."        OFF)


  option(LIBMINC_MINC1_SUPPORT           "Support minc1 file format, requires NETCDF" OFF)
  option(LIBMINC_BUILD_EZMINC            "Build C++ EMINC library based on libminc1" ON)
  option(LIBMINC_BUILD_EZMINC_EXAMPLES   "Build EZminc examples" OFF)
  option(LIBMINC_USE_NIFTI               "Build with NIfTI support" OFF)
  option(LIBMINC_USE_SYSTEM_NIFTI        "Use system NIfTI-1 library" OFF)

  option(LIBMINC_USE_ASAN                "Build with Address Sanitizer" OFF)

  if(DEFINED LIB_SUFFIX)
    message(WARNING "LIB_SUFFIX is deprecated, use the standard CMAKE_INSTALL_LIBDIR instead")
  endif()

  set (LIBMINC_EXPORTED_TARGETS "LIBMINC-targets")
  set (LIBMINC_INSTALL_BIN_DIR ${CMAKE_INSTALL_BINDIR})
  set (LIBMINC_INSTALL_LIB_DIR ${CMAKE_INSTALL_LIBDIR})
  set (LIBMINC_INSTALL_INCLUDE_DIR ${CMAKE_INSTALL_INCLUDEDIR})
  set (LIBMINC_INSTALL_DATA_DIR ${CMAKE_INSTALL_DATADIR})

  if(LIBMINC_MINC1_SUPPORT)
    find_package(NETCDF REQUIRED)
  endif()

  # external packages
  find_package(ZLIB REQUIRED)
  set(HDF5_NO_FIND_PACKAGE_CONFIG_FILE ON)
  find_package(HDF5 REQUIRED COMPONENTS C )

  if(LIBMINC_USE_NIFTI)
  if (LIBMINC_USE_SYSTEM_NIFTI)
    find_package(NIFTI)
  else()
    include(ExternalProject)
    include(BuildNIFTI)
    build_nifti(${CMAKE_INSTALL_PREFIX} ${CMAKE_BINARY_DIR}/external)
  endif()
  else()
  set(NIFTI_FOUND OFF)
  endif()

  set(HAVE_ZLIB ON)
else()
  #TODO: set paths for HDF5 etc
endif()

if(NOT DEFINED CMAKE_MACOSX_RPATH)
  set(CMAKE_MACOSX_RPATH ON)
endif()

# Configure libminc
if(LIBMINC_BUILD_SHARED_LIBS)
  set(LIBRARY_TYPE SHARED)
  set(LIBRARY_INSTALL LIBRARY)
else()
  set(LIBRARY_TYPE STATIC)
  set(LIBRARY_INSTALL ARCHIVE)
endif()

if(CMAKE_BUILD_TYPE MATCHES Debug)
  set(DEBUG "1")
endif()

# add for building relocatable library
if(UNIX)
  set(CMAKE_C_FLAGS   "${CMAKE_C_FLAGS}   -fPIC")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fPIC")
endif()


if(LIBMINC_USE_ASAN)
  add_compile_options(-fno-omit-frame-pointer -fsanitize=address)
  add_link_options(-fno-omit-frame-pointer -fsanitize=address)
endif()


# check for prereqs
include(CheckFunctionExists)
include(CheckSymbolExists)
include(CheckLibraryExists)

CHECK_FUNCTION_EXISTS(mkstemp  HAVE_MKSTEMP)
CHECK_FUNCTION_EXISTS(tmpnam   HAVE_TMPNAM)
CHECK_FUNCTION_EXISTS(tempnam  HAVE_TEMPNAM)
CHECK_FUNCTION_EXISTS(strerror HAVE_STRERROR)
CHECK_FUNCTION_EXISTS(popen    HAVE_POPEN)
CHECK_FUNCTION_EXISTS(fork     HAVE_WORKING_FORK)
CHECK_FUNCTION_EXISTS(vfork    HAVE_WORKING_VFORK)
CHECK_FUNCTION_EXISTS(fdopen   HAVE_FDOPEN)
CHECK_FUNCTION_EXISTS(strdup   HAVE_STRDUP)
CHECK_FUNCTION_EXISTS(getpwnam HAVE_GETPWNAM)
CHECK_FUNCTION_EXISTS(select   HAVE_SELECT)
CHECK_FUNCTION_EXISTS(strerror HAVE_STRERROR)
CHECK_FUNCTION_EXISTS(sysconf  HAVE_SYSCONF)
CHECK_FUNCTION_EXISTS(system   HAVE_SYSTEM)

if(NOT MSVC)
  set(CMAKE_REQUIRED_LIBRARIES m)
endif()

set(CMAKE_REQUIRED_DEFINITIONS -D_GNU_SOURCE=1 )

CHECK_SYMBOL_EXISTS(rint     "math.h" HAVE_RINT)
CHECK_SYMBOL_EXISTS(copysign "math.h" HAVE_COPYSIGN)
CHECK_SYMBOL_EXISTS(round    "math.h" HAVE_ROUND)
CHECK_SYMBOL_EXISTS(fabsf    "math.h" HAVE_FABSF)
unset(CMAKE_REQUIRED_LIBRARIES)
unset(CMAKE_REQUIRED_DEFINITIONS)


CHECK_FUNCTION_EXISTS(srand48   HAVE_SRAND48)
CHECK_FUNCTION_EXISTS(drand48   HAVE_DRAND48)
CHECK_FUNCTION_EXISTS(sleep     HAVE_SLEEP)

CHECK_SYMBOL_EXISTS(gettimeofday "sys/time.h" HAVE_GETTIMEOFDAY)

CHECK_LIBRARY_EXISTS(rt clock_gettime "time.h" HAVE_CLOCK_GETTIME_RT)

if(HAVE_CLOCK_GETTIME_RT)
  set(RT_LIBRARY "rt")
  set(HAVE_CLOCK_GETTIME ON)
else()
  # first check if the function is defined in header
  CHECK_SYMBOL_EXISTS(clock_gettime "time.h" HAVE_CLOCK_GETTIME_SYMBOL)
  # then check that it's available
  if(HAVE_CLOCK_GETTIME_SYMBOL)
    try_run(HAVE_CLOCK_GETTIME_RUN HAVE_CLOCK_GETTIME_COMP
            ${CMAKE_CURRENT_BINARY_DIR}
            ${CMAKE_CURRENT_SOURCE_DIR}/check_clock_gettime.c
            )
    if(HAVE_CLOCK_GETTIME_RUN EQUAL 0 AND HAVE_CLOCK_GETTIME_COMP)
      set(HAVE_CLOCK_GETTIME ON)
    else()
      set(HAVE_CLOCK_GETTIME OFF)
    endif()
  endif()


endif()

include(CheckIncludeFiles)
CHECK_INCLUDE_FILES(sys/dir.h   HAVE_SYS_DIR_H)
CHECK_INCLUDE_FILES(sys/ndir.h  HAVE_SYS_NDIR_H)
CHECK_INCLUDE_FILES(sys/stat.h  HAVE_SYS_STAT_H)
CHECK_INCLUDE_FILES(sys/types.h HAVE_SYS_TYPES_H)
CHECK_INCLUDE_FILES(sys/wait.h  HAVE_SYS_WAIT_H)
CHECK_INCLUDE_FILES(sys/time.h  HAVE_SYS_TIME_H)
CHECK_INCLUDE_FILES(unistd.h    HAVE_UNISTD_H)
CHECK_INCLUDE_FILES(dirent.h    HAVE_DIRENT_H)
CHECK_INCLUDE_FILES(fcntl.h     HAVE_FCNTL_H)
CHECK_INCLUDE_FILES(dlfcn.h     HAVE_DLFCN_H)
CHECK_INCLUDE_FILES(vfork.h     HAVE_VFORK_H)
CHECK_INCLUDE_FILES(inttypes.h  HAVE_INTTYPES_H)
CHECK_INCLUDE_FILES(strings.h   HAVE_STRINGS_H)
CHECK_INCLUDE_FILES(pwd.h       HAVE_PWD_H)
CHECK_INCLUDE_FILES(sys/select.h    HAVE_SYS_SELECT_H)


add_definitions(-DHAVE_CONFIG_H)
add_definitions(-DMINC2=1)
if(NOT APPLE)
add_definitions(-D_XOPEN_SOURCE=500)
endif()
# aliases
set(VERSION "${LIBMINC_EXTERNAL_LIB_PREFIX}${LIBMINC_PACKAGE_VERSION}")
set(HAVE_MINC1 ${LIBMINC_MINC1_SUPPORT})
set(HAVE_MINC2 ON)
set(MINC2 "1")

if(LIBMINC_MINC1_SUPPORT)
 add_definitions(-DHAVE_MINC1=1)
endif()

if(LIBMINC_BUILD_EZMINC)
  set(LIBMINC_INCLUDE_DIRS_CONFIG
    ${CMAKE_CURRENT_SOURCE_DIR}/ezminc
  )
  include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR}/ezminc
   )
endif()

if(NIFTI_FOUND)
    set(LIBMINC_NIFTI_SUPPORT ON)
    set(LIBMINC_NIFTI_SOURCES
      volume_io/Volumes/input_nifti.c
      volume_io/Volumes/input_mgh.c
      volume_io/Volumes/input_nrrd.c
    )
endif()

# config files for build
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/config.h.cmake ${CMAKE_CURRENT_BINARY_DIR}/config.h @ONLY)

# others
#configure_file(${CMAKE_CURRENT_SOURCE_DIR}/epm-header.in ${CMAKE_CURRENT_BINARY_DIR}/epm-header)

# set the master INCLUDE directories
include_directories(
   ${CMAKE_CURRENT_SOURCE_DIR}
   ${CMAKE_CURRENT_BINARY_DIR}
   ${CMAKE_CURRENT_SOURCE_DIR}/libcommon
   ${CMAKE_CURRENT_SOURCE_DIR}/libsrc2
   ${CMAKE_CURRENT_SOURCE_DIR}/libsrc
   ${CMAKE_CURRENT_SOURCE_DIR}/volume_io/Include
   ${HDF5_INCLUDE_DIRS}
   )

if(LIBMINC_BUILD_EZMINC AND LIBMINC_MINC1_SUPPORT)
  include_directories( ${CMAKE_CURRENT_SOURCE_DIR}/ezminc )
endif()

if(LIBMINC_NIFTI_SUPPORT)
    include_directories(
        ${NIFTI_INCLUDE_DIR}
    )
endif()

set(minc_common_SRCS
  libcommon/minc2_error.c
  libcommon/minc_config.c
  libcommon/minc_error.c
  libcommon/ParseArgv.c
  libcommon/read_file_names.c
  libcommon/restructure.c
  libcommon/time_stamp.c
)

set(minc_common_HEADERS
  libcommon/minc2_error.h
  libcommon/minc_config.h
  libcommon/minc_error.h
  libcommon/ParseArgv.h
  libcommon/read_file_names.h
  libcommon/restructure.h
  libcommon/time_stamp.h
  libcommon/minc_common_defs.h
  volume_io/Include/volume_io.h # VF: WTF?
)

set(minc1_LIB_SRCS
   libsrc/dim_conversion.c
   libsrc/image_conversion.c
   libsrc/minc_convenience.c
   libsrc/nd_loop.c
   libsrc/netcdf_convenience.c
   libsrc/value_conversion.c
   libsrc/voxel_loop.c
   libsrc/hdf_convenience.c
   libsrc/minc_compat.c
   libsrc/minc_simple.c
   libsrc/minc_format_convert.c
   )

set(minc1_HEADERS
  libsrc/minc.h
  libsrc/voxel_loop.h
  libsrc/nd_loop.h
  libsrc/minc_compat.h
  libsrc/minc_simple.h
  libsrc/minc_format_convert.h
)

set(minc2_LIB_SRCS
   libsrc2/convert.c
   libsrc2/datatype.c
   libsrc2/dimension.c
   libsrc2/free.c
   libsrc2/grpattr.c
   libsrc2/hyper.c
   libsrc2/label.c
   libsrc2/m2util.c
   libsrc2/record.c
   libsrc2/slice.c
   libsrc2/valid.c
   libsrc2/volprops.c
   libsrc2/volume.c
   )

set(minc2_HEADERS
  libsrc2/minc2.h
  libsrc2/minc2_defs.h
  libsrc2/minc2_structs.h
  libsrc2/minc2_api.h
)

# volume_io2
set(volume_io_LIB_SRCS
   volume_io/Geometry/colour.c
   volume_io/Geometry/colour_def.c
   volume_io/Geometry/gaussian.c
   volume_io/Geometry/inverse.c
   volume_io/Geometry/newton.c
   volume_io/Geometry/points.c
   volume_io/Geometry/splines.c
   volume_io/Geometry/tensors.c
   volume_io/Geometry/transforms.c
   volume_io/MNI_formats/gen_xf_io.c
   volume_io/MNI_formats/gen_xfs.c
   volume_io/MNI_formats/grid_transforms.c
   volume_io/MNI_formats/mni_io.c
   volume_io/MNI_formats/tag_points.c
   volume_io/MNI_formats/thin_plate_spline.c
   volume_io/Prog_utils/alloc.c
   volume_io/Prog_utils/alloc_check.c
   volume_io/Prog_utils/arrays.c
   volume_io/Prog_utils/files.c
   volume_io/Prog_utils/print.c
   volume_io/Prog_utils/progress.c
   volume_io/Prog_utils/string.c
   volume_io/Prog_utils/time.c
   volume_io/Volumes/evaluate.c
   volume_io/Volumes/get_hyperslab.c
   volume_io/Volumes/input_free.c
   volume_io/Volumes/input_mnc.c
   volume_io/Volumes/input_volume.c
   volume_io/Volumes/multidim_arrays.c
   volume_io/Volumes/output_mnc.c
   volume_io/Volumes/output_volume.c
   volume_io/Volumes/set_hyperslab.c
   volume_io/Volumes/volume_cache.c
   volume_io/Volumes/volumes.c
   volume_io/Volumes/input_mnc2.c
   volume_io/Volumes/output_mnc2.c
)

if(LIBMINC_NIFTI_SUPPORT)
    set(volume_io_LIB_SRCS
        ${volume_io_LIB_SRCS}
        ${LIBMINC_NIFTI_SOURCES}
    )
endif()


set(volume_io_HEADERS
    volume_io/Include/volume_io/alloc.h
    volume_io/Include/volume_io/arrays.h
    volume_io/Include/volume_io/basic.h
    volume_io/Include/volume_io/def_math.h
    volume_io/Include/volume_io/files.h
    volume_io/Include/volume_io/geom_structs.h
    volume_io/Include/volume_io/geometry.h
    volume_io/Include/volume_io/multidim.h
    volume_io/Include/volume_io/progress.h
    volume_io/Include/volume_io/string_funcs.h
    volume_io/Include/volume_io/system_dependent.h
    volume_io/Include/volume_io/transforms.h
    volume_io/Include/volume_io/vol_io_prototypes.h
    volume_io/Include/volume_io/volume.h
    volume_io/Include/volume_io/volume_cache.h
)

set(LIBMINC_LIBRARY   minc2)

# a hack for APPLE
if(LIBMINC_BUILD_SHARED_LIBS)
  set(LIBMINC_LIBRARY_STATIC minc2_static)
else()
  set(LIBMINC_LIBRARY_STATIC ${LIBMINC_LIBRARY})
endif()

if(LIBMINC_NIFTI_SUPPORT)
    set(NIFTI_LIBRARIES
        ${NIFTI_LIBRARY} ${ZNZ_LIBRARY}
    )
    get_filename_component(NIFTI_LIBRARY_NAME "${NIFTI_LIBRARY}" NAME)
    get_filename_component(ZNZ_LIBRARY_NAME "${ZNZ_LIBRARY}" NAME)
else()
    set(NIFTI_LIBRARIES )
    set(NIFTI_LIBRARY )
    set(ZNZ_LIBRARY )
    set(NIFTI_LIBRARY_NAME )
    set(ZNZ_LIBRARY_NAME )
endif()

if(NOT HDF5_LIBRARY AND HDF5_LIBRARIES)
    set(HDF5_LIBRARY ${HDF5_LIBRARIES})
endif()

get_filename_component(HDF5_LIBRARY_NAME "${HDF5_LIBRARY}" NAME)
get_filename_component(ZLIB_LIBRARY_NAME "${ZLIB_LIBRARY}" NAME)

set(LIBMINC_LIBRARIES        ${LIBMINC_LIBRARY} ${HDF5_LIBRARY} ${NIFTI_LIBRARIES} ${ZLIB_LIBRARY})
set(LIBMINC_LIBRARIES_CONFIG ${LIBMINC_LIBRARY} ${HDF5_LIBRARY_NAME} ${NIFTI_LIBRARY_NAME} ${ZNZ_LIBRARY_NAME} ${ZLIB_LIBRARY_NAME})
message("LIBMINC_LIBRARIES_CONFIG=${LIBMINC_LIBRARIES_CONFIG}")

set(LIBMINC_STATIC_LIBRARIES ${LIBMINC_LIBRARY_STATIC} ${HDF5_LIBRARY} ${NIFTI_LIBRARIES} ${ZLIB_LIBRARY})
set(LIBMINC_STATIC_LIBRARIES_CONFIG ${LIBMINC_LIBRARY_STATIC} ${HDF5_LIBRARY_NAME} ${NIFTI_LIBRARIES} ${ZLIB_LIBRARY_NAME})

if(UNIX)
  set(LIBMINC_LIBRARIES ${LIBMINC_LIBRARIES} m ${CMAKE_DL_LIBS} ${RT_LIBRARY})
  set(LIBMINC_STATIC_LIBRARIES ${LIBMINC_STATIC_LIBRARIES} m ${CMAKE_DL_LIBS} ${RT_LIBRARY})

  set(LIBMINC_LIBRARIES_CONFIG ${LIBMINC_LIBRARIES_CONFIG} m ${CMAKE_DL_LIBS} ${RT_LIBRARY_NAME})
  set(LIBMINC_STATIC_LIBRARIES_CONFIG ${LIBMINC_STATIC_LIBRARIES_CONFIG} m ${CMAKE_DL_LIBS} ${RT_LIBRARY_NAME})
endif()

set(minc_LIB_SRCS ${minc2_LIB_SRCS} ${minc_common_SRCS})
set(minc_HEADERS  ${minc2_HEADERS} ${minc_common_HEADERS})

if(LIBMINC_MINC1_SUPPORT)
  set(minc_HEADERS  ${minc_HEADERS}  ${minc1_HEADERS})
  set(minc_LIB_SRCS ${minc_LIB_SRCS} ${minc1_LIB_SRCS})
endif()


# Keep this variable for compatibility
set(VOLUME_IO_LIBRARY  minc2)
set(CMAKE_INSTALL_RPATH ${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR})

add_library(${LIBMINC_LIBRARY} ${LIBRARY_TYPE} ${minc_LIB_SRCS} ${minc_HEADERS} ${volume_io_LIB_SRCS} ${volume_io_HEADERS} )

if(NIFTI_FOUND AND NOT LIBMINC_USE_SYSTEM_NIFTI)
    add_dependencies(${LIBMINC_LIBRARY} NIFTI)
endif()


target_link_libraries(${LIBMINC_LIBRARY} ${HDF5_LIBRARY} ${NIFTI_LIBRARIES} ${ZLIB_LIBRARY} ${RT_LIBRARY}) #

if(LIBMINC_MINC1_SUPPORT)
  include_directories(${NETCDF_INCLUDE_DIR})
  target_link_libraries(${LIBMINC_LIBRARY} ${NETCDF_LIBRARY})
  get_filename_component(NETCDF_LIBRARY_NAME "${NETCDF_LIBRARY}" NAME)
else()
  set(NETCDF_LIBRARY_NAME )
endif()

if(NOT LIBMINC_USE_SYSTEM_NIFTI AND NOT LIBMINC_EXTERNALLY_CONFIGURED AND NIFTI_FOUND)
  add_dependencies(minc2 NIFTI)
endif()

export(TARGETS ${LIBMINC_LIBRARY} FILE "${LIBMINC_EXPORTED_TARGETS}.cmake")

if(UNIX)
  target_link_libraries(${LIBMINC_LIBRARY} m ${CMAKE_DL_LIBS} )

  if(LIBMINC_BUILD_SHARED_LIBS)
    add_library(${LIBMINC_LIBRARY_STATIC} STATIC ${minc_LIB_SRCS} ${minc_HEADERS} ${volume_io_LIB_SRCS} ${volume_io_HEADERS} )
    target_link_libraries(${LIBMINC_LIBRARY_STATIC} ${HDF5_LIBRARY} ${NIFTI_LIBRARIES} ${ZLIB_LIBRARY} ${RT_LIBRARY} m ${CMAKE_DL_LIBS} )
    if(LIBMINC_MINC1_SUPPORT)
      target_link_libraries(${LIBMINC_LIBRARY} ${NETCDF_LIBRARY})
    endif()
  endif()
endif()

set_target_properties(${LIBMINC_LIBRARY}
 PROPERTIES
  SOVERSION ${LIBMINC_SOVERSION})

if(LIBMINC_MINC1_SUPPORT)
  set(LIBMINC_LIBRARIES        ${LIBMINC_LIBRARIES} ${NETCDF_LIBRARY} )
  set(LIBMINC_STATIC_LIBRARIES ${LIBMINC_STATIC_LIBRARIES} ${NETCDF_LIBRARY} )

  set(LIBMINC_LIBRARIES_CONFIG        ${LIBMINC_LIBRARIES_CONFIG} ${NETCDF_LIBRARY_NAME} )
  set(LIBMINC_STATIC_LIBRARIES_CONFIG ${LIBMINC_STATIC_LIBRARIES_CONFIG} ${NETCDF_LIBRARY_NAME} )
endif()

if( LIBMINC_INSTALL_LIB_DIR )
  install(
    TARGETS
      ${LIBMINC_LIBRARY}
    EXPORT
      ${LIBMINC_EXPORTED_TARGETS}
    LIBRARY DESTINATION ${LIBMINC_INSTALL_LIB_DIR} COMPONENT libraries
    ARCHIVE DESTINATION ${LIBMINC_INSTALL_LIB_DIR} COMPONENT libraries
    RUNTIME DESTINATION ${LIBMINC_INSTALL_LIB_DIR} COMPONENT libraries
  )
endif()

if(LIBMINC_INSTALL_INCLUDE_DIR AND NOT LIBMINC_INSTALL_NO_DEVELOPMENT)
  install(FILES   ${minc_HEADERS}      DESTINATION  ${LIBMINC_INSTALL_INCLUDE_DIR} COMPONENT headers )
  install(FILES   ${volume_io_HEADERS} DESTINATION  ${LIBMINC_INSTALL_INCLUDE_DIR}/volume_io COMPONENT headers)
endif()

if(LIBMINC_BUILD_EZMINC AND LIBMINC_MINC1_SUPPORT)
  add_subdirectory( ezminc )
  set(EZMINC_LIBRARIES                   minc_io ${LIBMINC_LIBRARIES})
  set(EZMINC_LIBRARIES_CONFIG            minc_io ${LIBMINC_LIBRARIES_CONFIG})
else()
  set(EZMINC_LIBRARIES                   "")
  set(EZMINC_LIBRARIES_CONFIG            "")
endif()


# config for install dir
set(LIBMINC_USE_FILE_CONFIG     "\${LIBMINC_INSTALL_PREFIX}/lib/cmake/Use${LIBMINC_EXTERNAL_LIB_PREFIX}LIBMINC.cmake" )
set(LIBMINC_INCLUDE_DIRS_CONFIG "\${LIBMINC_INSTALL_PREFIX}/include" )
set(LIBMINC_LIBRARY_DIRS_CONFIG "\${LIBMINC_INSTALL_PREFIX}/lib" )
set(LIBMINC_STATIC_LIBRARIES_CONFIG   "" )
set(VOLUME_IO_LIBRARY_STATIC "")

configure_file(LIBMINCConfig.cmake.in
  ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/${LIBMINC_EXTERNAL_LIB_PREFIX}LIBMINCConfig.cmake @ONLY )

configure_file(UseLIBMINC.cmake.in
  ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/Use${LIBMINC_EXTERNAL_LIB_PREFIX}LIBMINC.cmake @ONLY)

# configure for build dir
set(LIBMINC_USE_FILE_CONFIG  ${CMAKE_CURRENT_BINARY_DIR}/UseLIBMINC.cmake)

set(LIBMINC_INCLUDE_DIRS_CONFIG
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/libcommon
  ${CMAKE_CURRENT_SOURCE_DIR}/libsrc2
  ${CMAKE_CURRENT_SOURCE_DIR}/volume_io/Include
)
if(LIBMINC_MINC1_SUPPORT)
  set(LIBMINC_INCLUDE_DIRS_CONFIG
    ${LIBMINC_INCLUDE_DIRS_CONFIG}
    ${CMAKE_CURRENT_SOURCE_DIR}/libsrc
  )

  if(LIBMINC_BUILD_EZMINC)
    set(LIBMINC_INCLUDE_DIRS_CONFIG ${LIBMINC_INCLUDE_DIRS_CONFIG}
      ${CMAKE_CURRENT_SOURCE_DIR}/ezminc )
  endif()
endif()

set(LIBMINC_LIBRARY_DIRS_CONFIG        ${CMAKE_CURRENT_BINARY_DIR})
set(LIBMINC_LIBRARIES_CONFIG           ${LIBMINC_LIBRARIES})
set(LIBMINC_STATIC_LIBRARIES_CONFIG    ${LIBMINC_STATIC_LIBRARIES})
set(EZMINC_LIBRARIES_CONFIG            ${EZMINC_LIBRARIES})

if(LIBMINC_MINC1_SUPPORT)
  set(LIBMINC_LIBRARY_DIRS_CONFIG  ${LIBMINC_LIBRARY_DIRS_CONFIG} ${CMAKE_CURRENT_BINARY_DIR}/ezminc)
endif()

configure_file(LIBMINCConfig.cmake.in
  ${CMAKE_CURRENT_BINARY_DIR}/LIBMINCConfig.cmake @ONLY )

configure_file(UseLIBMINC.cmake.in
  ${CMAKE_CURRENT_BINARY_DIR}/UseLIBMINC.cmake @ONLY)


if(LIBMINC_INSTALL_LIB_DIR AND NOT LIBMINC_INSTALL_NO_DEVELOPMENT)
  install(
    FILES
     ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/Use${LIBMINC_EXTERNAL_LIB_PREFIX}LIBMINC.cmake
     ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/${LIBMINC_EXTERNAL_LIB_PREFIX}LIBMINCConfig.cmake
    DESTINATION
     ${LIBMINC_INSTALL_LIB_DIR}/cmake
    COMPONENT Development)
endif()

# testing
if(BUILD_TESTING)
  add_subdirectory(testdir)
endif()

if(NOT LIBMINC_EXTERNALLY_CONFIGURED)
  include(CPack)
endif()
