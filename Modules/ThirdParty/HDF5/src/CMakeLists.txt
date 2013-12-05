set(HDF5_EXTERNALLY_CONFIGURED ON)
set(HDF5_INSTALL_NO_DEVELOPMENT ${ITK_INSTALL_NO_DEVELOPMENT})
set(HDF5_EXTERNAL_LIB_PREFIX "itk")
set(HDF5_EXPORTED_TARGETS ${ITKHDF5-targets})
set(HDF5_INSTALL_LIBRARY_DIR ${ITKHDF5_INSTALL_LIBRARY_DIR})
set(HDF5_INSTALL_ARCHIVE_DIR ${ITKHDF5_INSTALL_ARCHIVE_DIR})
set(HDF5_INSTALL_RUNTIME_DIR ${ITKHDF5_INSTALL_RUNTIME_DIR})
set(HDF5_INSTALL_INCLUDE_DIR ${ITKHDF5_INSTALL_INCLUDE_DIR}/itkhdf5)

# Configure HDF5 privately so its options do not appear to the user.
set(HDF5_ALLOW_EXTERNAL_SUPPORT OFF CACHE INTERNAL "Allow External Library Building")
set(HDF5_BUILD_CPP_LIB ON CACHE INTERNAL "Build HDF5 C++ Library")
set(HDF5_BUILD_HL_LIB OFF CACHE INTERNAL "Build HIGH Level HDF5 Library")
set(HDF5_DISABLE_COMPILER_WARNINGS OFF CACHE INTERNAL "Disable HDF5 warnings")
set(HDF5_ENABLE_CODESTACK OFF CACHE INTERNAL "Enable the function stack tracing (for developer debugging).")
set(HDF5_ENABLE_COVERAGE OFF CACHE INTERNAL "Enable code coverage for Libraries and Programs")
set(HDF5_ENABLE_DEPRECATED_SYMBOLS ON CACHE INTERNAL "Enable deprecated public API symbols")
set(HDF5_ENABLE_GPFS OFF CACHE INTERNAL "Enable GPFS hints for the MPI/POSIX file driver")
set(HDF5_ENABLE_HSIZET ON CACHE INTERNAL "Enable datasets larger than memory")
set(HDF5_ENABLE_LARGE_FILE ON CACHE INTERNAL "Enable support for large (64-bit) files on Linux.")
set(HDF5_ENABLE_PARALLEL OFF CACHE INTERNAL "Enable parallel build (requires MPI)")
set(HDF5_ENABLE_SZIP_SUPPORT OFF CACHE INTERNAL "Use SZip Filter")
set(HDF5_ENABLE_THREADSAFE OFF CACHE INTERNAL "Enable Threadsafety")
set(HDF5_ENABLE_TRACE OFF CACHE INTERNAL "Enable API tracing capability")
set(HDF5_ENABLE_USING_MEMCHECKER OFF CACHE INTERNAL "Indicate that a memory checker is used")
set(HDF5_ENABLE_Z_LIB_SUPPORT ON CACHE INTERNAL "Enable HDF5 Zlib Filters")
set(HDF5_Enable_Clear_File_Buffers ON CACHE INTERNAL "Securely clear file buffers before writing to file")
set(HDF5_Enable_Instrument OFF CACHE INTERNAL "Instrument The library")
set(HDF5_METADATA_TRACE_FILE OFF CACHE INTERNAL "Enable metadata trace file collection")
set(HDF5_PACKAGE_EXTLIBS OFF CACHE INTERNAL "CPACK - include external libraries")
set(HDF5_STRICT_FORMAT_CHECKS OFF CACHE INTERNAL "Whether to perform strict file format checks")
set(HDF5_USE_16_API_DEFAULT OFF CACHE INTERNAL "Use the HDF5 1.6.x API by default")
set(HDF5_USE_FILTER_FLETCHER32 ON CACHE INTERNAL "Use the FLETCHER32 Filter")
set(HDF5_USE_FILTER_NBIT ON CACHE INTERNAL "Use the NBIT Filter")
set(HDF5_USE_FILTER_SCALEOFFSET ON CACHE INTERNAL "Use the SCALEOFFSET Filter")
set(HDF5_USE_FILTER_SHUFFLE ON CACHE INTERNAL "Use the SHUFFLE Filter")
set(HDF5_USE_H5DUMP_PACKED_BITS ON CACHE INTERNAL "Use the PACKED BITS feature in h5dump")
set(HDF5_WANT_DATA_ACCURACY ON CACHE INTERNAL "IF data accuracy is guaranteed during data conversions")
set(HDF5_WANT_DCONV_EXCEPTION ON CACHE INTERNAL "exception handling functions is checked during data conversions")
set(HDF_LEGACY_NAMING ON CACHE INTERNAL "Use Legacy Names for Libraries and Programs")
set(BUILD_STATIC_EXECS OFF CACHE INTERNAL "Build Static Executables")

# Setup all necessary overrides for zlib so that HDF5 uses our
# internally compiled zlib rather than any other version
if(HDF5_ENABLE_Z_LIB_SUPPORT)
  # We must tell the main HDF5 library that it depends on our zlib
  set(ZLIB_INCLUDE_DIRS "${ITKZLIB_INCLUDE_DIRS}")
  set(ZLIB_LIBRARIES "${ITKZLIB_LIBRARIES}")
  set(H5_ZLIB_HEADER "itk_zlib.h")
endif(HDF5_ENABLE_Z_LIB_SUPPORT)

add_subdirectory(itkhdf5)

foreach(lib
    itkhdf5
    itkhdf5_cpp
    )
  itk_module_target(${lib} NO_INSTALL)
endforeach()
