# Define the srcs for Media Storage And FileFormat
# MSFF
set(MSFF_SRCS
  gdcmEmptyMaskGenerator.cxx
  gdcmFileStreamer.cxx
  gdcmJSON.cxx
  gdcmFileChangeTransferSyntax.cxx
  gdcmAnonymizer.cxx
  gdcmFileAnonymizer.cxx
  gdcmIconImageFilter.cxx
  gdcmIconImageGenerator.cxx
  gdcmDICOMDIRGenerator.cxx
  gdcmSpacing.cxx
  gdcmFileExplicitFilter.cxx
  gdcmFileDerivation.cxx
  gdcmImageFragmentSplitter.cxx
  gdcmTagPath.cxx
  gdcmSimpleSubjectWatcher.cxx
  gdcmAnonymizeEvent.cxx
  gdcmPixmap.cxx
  gdcmBitmap.cxx
  gdcmRescaler.cxx
  gdcmImageToImageFilter.cxx
  gdcmBitmapToBitmapFilter.cxx
  gdcmPixmapToPixmapFilter.cxx
  gdcmImageChangeTransferSyntax.cxx
  gdcmImageApplyLookupTable.cxx
  gdcmFileDecompressLookupTable.cxx
  gdcmOrientation.cxx
  gdcmDataSetHelper.cxx
  gdcmImageChangePlanarConfiguration.cxx
  gdcmImageChangePhotometricInterpretation.cxx
  gdcmDirectionCosines.cxx
  gdcmSorter.cxx
  gdcmSerieHelper.cxx
  gdcmIPPSorter.cxx
  gdcmApplicationEntity.cxx
  gdcmDICOMDIR.cxx
  gdcmSpectroscopy.cxx
  gdcmEncapsulatedDocument.cxx
  gdcmSplitMosaicFilter.cxx
  gdcmFiducials.cxx
  gdcmWaveform.cxx
  gdcmPersonName.cxx
  gdcmIconImage.cxx
  gdcmUIDGenerator.cxx
  gdcmUUIDGenerator.cxx
  gdcmPrinter.cxx
  gdcmDictPrinter.cxx
  gdcmXMLPrinter.cxx
  gdcmScanner.cxx
  gdcmStrictScanner.cxx
  gdcmPixmapReader.cxx
  gdcmImageReader.cxx
  gdcmPixmapWriter.cxx
  gdcmImageWriter.cxx
  gdcmStringFilter.cxx
  gdcmImageHelper.cxx
  gdcmValidate.cxx
  gdcmDumper.cxx
  gdcmImage.cxx
  gdcmImageConverter.cxx
  gdcmImageCodec.cxx
  gdcmJPEG12Codec.cxx
  gdcmRLECodec.cxx
  gdcmPDFCodec.cxx
  gdcmAudioCodec.cxx
  gdcmJPEG16Codec.cxx
  gdcmJPEGLSCodec.cxx
  gdcmJPEG8Codec.cxx
  gdcmJPEGCodec.cxx
  gdcmPVRGCodec.cxx
  gdcmKAKADUCodec.cxx
  gdcmPNMCodec.cxx
  gdcmPGXCodec.cxx
  gdcmRAWCodec.cxx
  gdcmLookupTable.cxx
  gdcmOverlay.cxx
  gdcmCurve.cxx
  gdcmPhotometricInterpretation.cxx
  gdcmPixelFormat.cxx
  gdcmSegmentedPaletteColorLookupTable.cxx
  gdcmStreamImageReader.cxx
  gdcmImageRegionReader.cxx
  #gdcmStreamImageWriter.cxx
  gdcmDirectoryHelper.cxx
  gdcmSegment.cxx
  gdcmSurface.cxx
  gdcmMeshPrimitive.cxx
  gdcmSegmentWriter.cxx
  gdcmSurfaceWriter.cxx
  gdcmSegmentReader.cxx
  gdcmSurfaceReader.cxx
  gdcmSurfaceHelper.cxx
  gdcmSegmentHelper.cxx
  gdcmJPEG2000Codec.cxx
  )

  list(APPEND MSFF_SRCS
    ${GDCM_SOURCE_DIR}/Utilities/gdcmrle/rle.cxx
    ${GDCM_SOURCE_DIR}/Utilities/gdcmrle/info.cxx
    ${GDCM_SOURCE_DIR}/Utilities/gdcmrle/io.cxx
  )

# Do the proper thing when building static...if only there was configured
# headers or def files instead
if(NOT BUILD_SHARED_LIBS)
  set_source_files_properties(gdcmJPEG2000Codec.cxx
                              PROPERTIES
                              COMPILE_FLAGS -DOPJ_STATIC
                              )
  set_source_files_properties(gdcmJPEGLSCodec.cxx
                              PROPERTIES
                              COMPILE_DEFINITIONS CHARLS_STATIC
                              )
else()
  set_source_files_properties(gdcmJPEGLSCodec.cxx
                              PROPERTIES
                              COMPILE_FLAGS -DCHARLS_DLL
                              )
endif()


# Add the include paths
include_directories(
  "${GDCM_SOURCE_DIR}/Source/Common"
  "${GDCM_BINARY_DIR}/Source/Common"
  "${GDCM_SOURCE_DIR}/Source/DataStructureAndEncodingDefinition"
  "${GDCM_SOURCE_DIR}/Source/DataDictionary"
  "${GDCM_SOURCE_DIR}/Source/InformationObjectDefinition"
  ${CMAKE_CURRENT_SOURCE_DIR}

  # FIXME:
  "${GDCM_SOURCE_DIR}/Utilities"
  "${GDCM_BINARY_DIR}/Utilities"
  )

# CharLS
if(GDCM_USE_JPEGLS)
  #include_directories("${GDCM_BINARY_DIR}/Utilities/gdcmcharls")
endif()

if(GDCM_USE_SYSTEM_CHARLS)
  include_directories(${CHARLS_INCLUDE_DIRS} )
else()
  include_directories(
  "${GDCM_BINARY_DIR}/Utilities/gdcmcharls"
    )
endif()
if(GDCM_USE_SYSTEM_OPENJPEG)
  include_directories(${OPENJPEG_INCLUDE_DIRS} )
else()
  include_directories(
  "${GDCM_BINARY_DIR}/Utilities/gdcmopenjpeg"
  "${GDCM_BINARY_DIR}/Utilities/gdcmopenjpeg/src/lib/openjp2"
    )
endif()
if(GDCM_USE_SYSTEM_LJPEG)
  include_directories(${LJPEG_INCLUDE_DIRS} )
endif()
if(NOT GDCM_USE_SYSTEM_ZLIB)
  include_directories(
    "${GDCM_BINARY_DIR}/Utilities/gdcmzlib"
  )
endif()
if(GDCM_USE_SYSTEM_UUID)
  include_directories(
    ${UUID_INCLUDE_DIR}
  )
  set(GDCMUUID ${UUID_LIBRARIES})
else()
  include_directories(
    "${GDCM_BINARY_DIR}/Utilities/gdcmuuid" # uuid_mangle.h
  )
  set(GDCMUUID gdcmuuid)
endif()
if(GDCM_USE_SYSTEM_JSON)
  include_directories(
    ${JSON_INCLUDE_DIRS}
  )
endif()

add_library(gdcmMSFF ${MSFF_SRCS})
# gdcmPVRGCodec calls gdcmjpeg
if(GDCM_USE_PVRG)
  if(NOT GDCM_USE_SYSTEM_PVRG)
    add_dependencies(gdcmMSFF gdcmjpeg)
  endif()
endif()

# main libs:
target_link_libraries(gdcmMSFF LINK_PUBLIC gdcmIOD gdcmDSED gdcmDICT)
target_link_libraries(gdcmMSFF LINK_PRIVATE ${GDCM_LJPEG_LIBRARIES} ${GDCM_OPENJPEG_LIBRARIES})
if(GDCM_USE_JPEGLS)
  target_link_libraries(gdcmMSFF LINK_PRIVATE ${GDCM_CHARLS_LIBRARIES})
endif()

if(CMAKE_COMPILER_IS_GNUCXX AND MINGW)
  # I am getting:
# CMakeFiles/gdcmMSFF.dir/gdcmScanner.obj(.text$_ZN4gdcm6ReaderC1Ev[gdcm::Reader::Reader()]+0x3a):gdcmScanner.cxx: variable 'vtable for gdcm::Reader' can't be auto-imported. Please read the documentation for ld's --enable-auto-import for details.
# CMakeFiles/gdcmMSFF.dir/gdcmImageReader.obj(.text$_ZN4gdcm6ReaderC2Ev[gdcm::Reader::Reader()]+0x3a):gdcmImageReader.cxx: variable 'vtable for gdcm::Reader' can't be auto-imported. Please read the documentation for ld's --enable-auto-import for details.
# CMakeFiles/gdcmMSFF.dir/gdcmImageWriter.obj(.text$_ZN4gdcm6WriterC2Ev[gdcm::Writer::Writer()]+0x3a):gdcmImageWriter.cxx: variable 'vtable for gdcm::Writer' can't be auto-imported. Please read the documentation for ld's --enable-auto-import for details.

  set_target_properties(gdcmMSFF PROPERTIES LINK_FLAGS "-Wl,--enable-runtime-pseudo-reloc")
endif()
#if(HAVE_UUIDCREATE)
if(WIN32)
  # For UuidCreate
  # http://msdn.microsoft.com/en-us/library/aa379205(VS.85).aspx
  target_link_libraries(gdcmMSFF LINK_PRIVATE rpcrt4)
#endif()
else()
  target_link_libraries(gdcmMSFF LINK_PRIVATE ${GDCMUUID})
endif()
if(GDCM_USE_SYSTEM_JSON)
  target_link_libraries(gdcmMSFF LINK_PRIVATE ${JSON_LIBRARIES})
endif()
# handling of static lib within shared is a mess:
#target_link_libraries(gdcmMSFF gdcmrle)
set_target_properties(gdcmMSFF PROPERTIES ${GDCM_LIBRARY_PROPERTIES})
if(BUILD_SHARED_LIBS)
  #set(_ill "gdcmDSED;gdcmDICT;gdcmIOD")
  #set_target_properties(gdcmMSFF PROPERTIES INTERFACE_LINK_LIBRARIES "${_ill}" LINK_INTERFACE_LIBRARIES "${_ill}")
endif()

# libs
install_library(gdcmMSFF)
# PDB
install_pdb(gdcmMSFF)
# include files
install_includes("*.h" "*.txx")
