set(HDF5_EXTERNALLY_CONFIGURED ON)
# Modern CMake install COMPONENTS
# Setting HDF5_INSTALL_NO_DEVELOPMENT & HDF5_INSTALL_NO_LIBRARIES to
# FALSE to make explicit that the development and libraries
# can not be skipped for installation.  Use the install COMPONENTS
# features instead.
set(HDF5_INSTALL_NO_DEVELOPMENT FALSE)
set(HDF5_EXPORTED_TARGETS ${ITKHDF5-targets})
set(HDF5_INSTALL_LIB_DIR ${ITKHDF5_INSTALL_LIBRARY_DIR})
set(HDF5_INSTALL_BIN_DIR ${ITKHDF5_INSTALL_RUNTIME_DIR})
set(HDF5_INSTALL_INCLUDE_DIR ${ITKHDF5_INSTALL_INCLUDE_DIR}/itkhdf5)

# Configure HDF5 privately so its options do not appear to the user.
set(HDF5_ALLOW_EXTERNAL_SUPPORT OFF CACHE INTERNAL "Allow External Library Building")
set(HDF5_BUILD_CPP_LIB ON CACHE INTERNAL "Build HDF5 C++ Library")
set(HDF5_BUILD_HL_LIB ON CACHE INTERNAL "Build High Level HDF5 Library")
set(HDF5_DISABLE_COMPILER_WARNINGS OFF CACHE INTERNAL "Disable HDF5 warnings")
set(HDF5_ENABLE_COVERAGE OFF CACHE INTERNAL "Enable code coverage for Libraries and Programs")
set(HDF5_ENABLE_DEPRECATED_SYMBOLS ON CACHE INTERNAL "Enable deprecated public API symbols")
set(HDF5_ENABLE_PARALLEL OFF CACHE INTERNAL "Enable parallel build (requires MPI)")
set(HDF5_ENABLE_SZIP_SUPPORT OFF CACHE INTERNAL "Use SZip Filter")
set(HDF5_ENABLE_THREADSAFE OFF CACHE INTERNAL "Enable Threadsafety")
set(HDF5_ENABLE_TRACE OFF CACHE INTERNAL "Enable API tracing capability")
set(HDF5_ENABLE_USING_MEMCHECKER OFF CACHE INTERNAL "Indicate that a memory checker is used")
set(HDF5_ENABLE_Z_LIB_SUPPORT ON CACHE INTERNAL "Enable HDF5 Zlib Filters")
set(HDF5_Enable_Instrument OFF CACHE INTERNAL "Instrument The library")
set(HDF5_PACKAGE_EXTLIBS OFF CACHE INTERNAL "CPACK - include external libraries")
set(HDF5_STRICT_FORMAT_CHECKS OFF CACHE INTERNAL "Whether to perform strict file format checks")
set(HDF5_WANT_DATA_ACCURACY ON CACHE INTERNAL "IF data accuracy is guaranteed during data conversions")
set(HDF5_WANT_DCONV_EXCEPTION ON CACHE INTERNAL "exception handling functions is checked during data conversions")
set(HDF_LEGACY_NAMING ON CACHE INTERNAL "Use Legacy Names for Libraries and Programs")
set(HDF5_BUILD_FRAMEWORKS OFF CACHE INTERNAL "TRUE to build as frameworks libraries, FALSE to build according to BUILD_SHARED_LIBS")
set(HDF5_BUILD_WITH_INSTALL_NAME  OFF CACHE INTERNAL "Build with library install_name set to the installation path")
set(HDF5_ENABLE_ALL_WARNINGS OFF CACHE INTERNAL "Enable all warnings")
set(HDF5_ENABLE_DEBUG_APIS OFF CACHE INTERNAL "Turn on debugging in all packages")
set(HDF5_ENABLE_DIRECT_VFD OFF CACHE INTERNAL "Build the Direct I/O Virtual File Driver")
set(HDF5_ENABLE_EMBEDDED_LIBINFO ON CACHE INTERNAL "embed library info into executables")
set(HDF5_GENERATE_HEADERS OFF CACHE INTERNAL "Rebuild Generated Files")
set(HDF5_PACK_EXAMPLES OFF CACHE INTERNAL "Package the HDF5 Library Examples Compressed File")
set(HDF5_NO_PACKAGES OFF CACHE INTERNAL "CPACK - Disable packaging")
set(HDF5_TEST_VFD OFF CACHE INTERNAL "Execute tests with different VFDs")
set(HDF5_USE_FOLDERS  ON CACHE INTERNAL "Enable folder grouping of projects in IDEs.")
set(HDF_TEST_EXPRESS OFF CACHE INTERNAL "Control testing framework (0-3)")
set(BUILD_STATIC_EXECS OFF CACHE INTERNAL "Build Static Executables")
set(BUILD_USER_DEFINED_LIBS OFF CACHE INTERNAL "Build user defined libs")

set (SZIP_USE_EXTERNAL OFF CACHE INTERNAL "Use External Library Building for SZIP" )
set (ZLIB_USE_EXTERNAL OFF CACHE INTERNAL "Use External Library Building for ZLIB" )
set (HDF5_DIMENSION_SCALES_NEW_REF OFF CACHE INTERNAL "Use new-style references with dimension scale APIs" )
set (HDF5_EXTERNAL_LIB_PREFIX "itk" CACHE INTERNAL "Use prefix for custom library naming.")
set (HDF5_LIB_INFIX "" CACHE INTERNAL "Use infix for custom library naming.")
set (HDF5_USE_GNU_DIRS OFF CACHE INTERNAL "TRUE to use GNU Coding Standard install directory variables, FALSE to use historical settings")
set (HDF5_TEST_SERIAL OFF CACHE INTERNAL "Execute non-parallel tests" )

set (HDF5_BUILD_DOC                   OFF CACHE INTERNAL "Build documentation")
set (HDF5_ENABLE_MAP_API              OFF CACHE INTERNAL "Build the map API")
set (HDF5_ENABLE_WARNINGS_AS_ERRORS   OFF CACHE INTERNAL "Interpret some warnings as errors")
set (HDF5_MINGW_STATIC_GCC_LIBS       OFF CACHE INTERNAL "")
set (HDF5_MSVC_NAMING_CONVENTION      OFF CACHE INTERNAL "Use MSVC Naming conventions for Shared Libraries")
set (HDF5_ENABLE_PLUGIN_SUPPORT       OFF CACHE INTERNAL "Enable PLUGIN Filters")

set (HDF5_BUILD_STATIC_TOOLS OFF CACHE INTERNAL "Build Static Tools NOT Shared Tools")
set (HDF5_ENABLE_NONSTANDARD_FEATURES ON CACHE INTERNAL "Enable support for non-standard programming language features")
set (HDF5_ENABLE_NONSTANDARD_FEATURE_FLOAT16 ON CACHE INTERNAL "Enable support for _Float16 C datatype")
set (HDF5_ENABLE_SUBFILING_VFD OFF CACHE INTERNAL "Build Parallel HDF5 Subfiling VFD")
set (HDF5_USE_LIBAEC_STATIC OFF CACHE INTERNAL "Use static AEC library" )
set (HDF5_USE_ZLIB_NG OFF CACHE INTERNAL "Use zlib-ng library as zlib library" )
set (HDF5_USE_ZLIB_STATIC OFF CACHE INTERNAL "Find static zlib library" )
set (ALLOW_UNSUPPORTED OFF CACHE INTERNAL "Allow unsupported combinations of configure options")
set (HDF5_ENABLE_HDFS OFF CACHE INTERNAL "Enable HDFS")
set (HDF5_ENABLE_ROS3_VFD OFF CACHE INTERNAL  "Build the ROS3 Virtual File Driver")
set (DEFAULT_API_VERSION "v114" CACHE INTERNAL "Enable v1.14 API (v16, v18, v110, v112, v114)")


if (NOT BUILD_SHARED_LIBS)
  set (BUILD_STATIC_LIBS ON CACHE INTERNAL "Build Static Libraries")
endif()

# Setup all necessary overrides for zlib so that HDF5 uses our
# internally compiled zlib rather than any other version
if(HDF5_ENABLE_Z_LIB_SUPPORT)
  # We must tell the main HDF5 library that it depends on our zlib
  # see src/itkhdf5/CMakeLists.txt for detauls
  set(H5_ZLIB_INCLUDE_DIRS "${ITKZLIB_INCLUDE_DIRS}")
  set(H5_ZLIB_LIBRARIES "${ITKZLIB_LIBRARIES}")
  set(HDF5_LIB_DEPENDENCIES ${ITKZLIB_LIBRARIES})
  set(H5_ZLIB_HEADER "itk_zlib.h")

  set (LINK_COMP_LIBS ${LINK_COMP_LIBS} ${H5_ZLIB_LIBRARIES})
endif()

add_subdirectory(itkhdf5)

mark_as_advanced(HDF_ENABLE_LARGE_FILE)
mark_as_advanced(CTEST_TEST_TIMEOUT)
mark_as_advanced(HDF5_BUILD_GENERATORS)
mark_as_advanced(HDF5_ENABLE_DEV_WARNINGS)
mark_as_advanced(HDF5_ENABLE_MIRROR_VFD)
mark_as_advanced(HDF5_IGNORE_DISABLED_FILE_LOCKS)
mark_as_advanced(HDF5_MEMORY_ALLOC_SANITY_CHECK)
mark_as_advanced(HDF5_USE_110_API_DEFAULT)
mark_as_advanced(HDF5_USE_18_API_DEFAULT)
mark_as_advanced(HDF5_USE_FILE_LOCKING)
mark_as_advanced(HDF5_USE_PREGEN)
mark_as_advanced(USE_LIBAEC)

if(BUILD_SHARED_LIBS)
  set(target_extension -shared)
else()
  set(target_extension -static)
endif()
foreach(lib
    hdf5
    hdf5_cpp
    hdf5_hl
    hdf5_hl_cpp
    )
  itk_module_target(${lib}${target_extension} NO_INSTALL)
endforeach()
if(ITK_LIBRARY_PROPERTIES)
  set_target_properties(hdf5${target_extension} PROPERTIES ${ITK_LIBRARY_PROPERTIES})
endif()
