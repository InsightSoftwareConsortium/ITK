#ifndef itk_tiff_mangle_h
#define itk_tiff_mangle_h

/*


This header file mangles all symbols exported from the tiff library.
It is included in all files while building the tiff library.  Due to
namespace pollution, no tiff headers should be included in .h files in
ITK.

The following commands were used to obtain the symbol list:

nm libitktiff.a |grep " T "
nm libitktiff.a |grep " D "

*/

#define __TIFFmalloc itk___TIFFmalloc
#define __TIFFfree itk___TIFFfree
#define _TIFFerrorHandler itk__TIFFerrorHandler
#define _TIFFwarningHandler itk__TIFFwarningHandler
#define tiffDataWidth itk_tiffDataWidth
#define LogL10fromY itk_LogL10fromY
#define LogL10toY itk_LogL10toY
#define LogL16fromY itk_LogL16fromY
#define LogL16toY itk_LogL16toY
#define LogLuv24fromXYZ itk_LogLuv24fromXYZ
#define LogLuv24toXYZ itk_LogLuv24toXYZ
#define LogLuv32fromXYZ itk_LogLuv32fromXYZ
#define LogLuv32toXYZ itk_LogLuv32toXYZ
#define TIFFCheckTile itk_TIFFCheckTile
#define TIFFClientOpen itk_TIFFClientOpen
#define TIFFClose itk_TIFFClose
#define TIFFComputeStrip itk_TIFFComputeStrip
#define TIFFComputeTile itk_TIFFComputeTile
#define TIFFCreateDirectory itk_TIFFCreateDirectory
#define TIFFCurrentDirOffset itk_TIFFCurrentDirOffset
#define TIFFCurrentDirectory itk_TIFFCurrentDirectory
#define TIFFCurrentRow itk_TIFFCurrentRow
#define TIFFCurrentStrip itk_TIFFCurrentStrip
#define TIFFCurrentTile itk_TIFFCurrentTile
#define TIFFDefaultDirectory itk_TIFFDefaultDirectory
#define TIFFDefaultStripSize itk_TIFFDefaultStripSize
#define TIFFDefaultTileSize itk_TIFFDefaultTileSize
#define TIFFError itk_TIFFError
#define TIFFFdOpen itk_TIFFFdOpen
#define TIFFFileName itk_TIFFFileName
#define TIFFFileno itk_TIFFFileno
#define TIFFFindCODEC itk_TIFFFindCODEC
#define TIFFFlush itk_TIFFFlush
#define TIFFFlushData itk_TIFFFlushData
#define TIFFFlushData1 itk_TIFFFlushData1
#define TIFFFreeDirectory itk_TIFFFreeDirectory
#define TIFFGetBitRevTable itk_TIFFGetBitRevTable
#define TIFFGetField itk_TIFFGetField
#define TIFFGetFieldDefaulted itk_TIFFGetFieldDefaulted
#define TIFFGetMode itk_TIFFGetMode
#define TIFFGetVersion itk_TIFFGetVersion
#define TIFFInitDumpMode itk_TIFFInitDumpMode

#ifdef  LZW_SUPPORT
 #define  TIFFInitLZW    itk_TIFFInitLZW
#endif
#ifdef  PACKBITS_SUPPORT
#define  TIFFInitPackBits  itk_TIFFInitPackBits
#endif
#ifdef  THUNDER_SUPPORT
#define TIFFInitThunderScan itk_TIFFInitThunderScan
#endif
#ifdef  NEXT_SUPPORT
#define  TIFFInitNeXT    itk_TIFFInitNeXT
#endif
#ifdef  JPEG_SUPPORT
#define  TIFFInitJPEG    itk_TIFFInitJPEG
#endif
#ifdef  OJPEG_SUPPORT
#define  TIFFInitOJPEG    itk_TIFFInitOJPEG
#endif
#ifdef  CCITT_SUPPORT
#define  TIFFInitCCITTRLE  itk_TIFFInitCCITTRLE
#define  TIFFInitCCITTRLEW  itk_TIFFInitCCITTRLEW
#define  TIFFInitCCITTFax3  itk_TIFFInitCCITTFax3
#define  TIFFInitCCITTFax4  itk_TIFFInitCCITTFax4
#endif
#ifdef JBIG_SUPPORT
#define  TIFFInitJBIG    itk_TIFFInitJBIG
#endif
#ifdef  ZIP_SUPPORT
#define  TIFFInitZIP    itk_TIFFInitZIP
#endif
#ifdef  PIXARLOG_SUPPORT
#define  TIFFInitPixarLog  itk_TIFFInitPixarLog
#endif
#ifdef LOGLUV_SUPPORT
#define TIFFInitSGILog    itk_TIFFInitSGILog
#endif

#define TIFFIsByteSwapped itk_TIFFIsByteSwapped
#define TIFFIsMSB2LSB itk_TIFFIsMSB2LSB
#define TIFFIsTiled itk_TIFFIsTiled
#define TIFFIsUpSampled itk_TIFFIsUpSampled
#define TIFFLastDirectory itk_TIFFLastDirectory
#define TIFFNumberOfDirectories itk_TIFFNumberOfDirectories
#define TIFFNumberOfStrips itk_TIFFNumberOfStrips
#define TIFFNumberOfTiles itk_TIFFNumberOfTiles
#define TIFFOpen itk_TIFFOpen
#define TIFFPredictorInit itk_TIFFPredictorInit
#define TIFFPrintDirectory itk_TIFFPrintDirectory
#define TIFFRGBAImageBegin itk_TIFFRGBAImageBegin
#define TIFFRGBAImageEnd itk_TIFFRGBAImageEnd
#define TIFFRGBAImageGet itk_TIFFRGBAImageGet
#define TIFFRGBAImageOK itk_TIFFRGBAImageOK
#define TIFFRasterScanlineSize itk_TIFFRasterScanlineSize
#define TIFFReadBufferSetup itk_TIFFReadBufferSetup
#define TIFFReadDirectory itk_TIFFReadDirectory
#define TIFFReadEncodedStrip itk_TIFFReadEncodedStrip
#define TIFFReadEncodedTile itk_TIFFReadEncodedTile
#define TIFFReadRGBAImage itk_TIFFReadRGBAImage
#define TIFFReadRGBAStrip itk_TIFFReadRGBAStrip
#define TIFFReadRGBATile itk_TIFFReadRGBATile
#define TIFFReadRawStrip itk_TIFFReadRawStrip
#define TIFFReadRawTile itk_TIFFReadRawTile
#define TIFFReadScanline itk_TIFFReadScanline
#define TIFFReadTile itk_TIFFReadTile
#define TIFFReassignTagToIgnore itk_TIFFReassignTagToIgnore
#define TIFFRegisterCODEC itk_TIFFRegisterCODEC
#define TIFFReverseBits itk_TIFFReverseBits
#define TIFFRewriteDirectory itk_TIFFRewriteDirectory
#define TIFFScanlineSize itk_TIFFScanlineSize
#define TIFFSetCompressionScheme itk_TIFFSetCompressionScheme
#define TIFFSetDirectory itk_TIFFSetDirectory
#define TIFFSetErrorHandler itk_TIFFSetErrorHandler
#define TIFFSetField itk_TIFFSetField
#define TIFFSetSubDirectory itk_TIFFSetSubDirectory
#define TIFFSetTagExtender itk_TIFFSetTagExtender
#define TIFFSetWarningHandler itk_TIFFSetWarningHandler
#define TIFFSetWriteOffset itk_TIFFSetWriteOffset
#define TIFFStripSize itk_TIFFStripSize
#define TIFFSwabArrayOfDouble itk_TIFFSwabArrayOfDouble
#define TIFFSwabArrayOfLong itk_TIFFSwabArrayOfLong
#define TIFFSwabArrayOfShort itk_TIFFSwabArrayOfShort
#define TIFFSwabDouble itk_TIFFSwabDouble
#define TIFFSwabLong itk_TIFFSwabLong
#define TIFFSwabShort itk_TIFFSwabShort
#define TIFFTileRowSize itk_TIFFTileRowSize
#define TIFFTileSize itk_TIFFTileSize
#define TIFFUnRegisterCODEC itk_TIFFUnRegisterCODEC
#define TIFFUnlinkDirectory itk_TIFFUnlinkDirectory
#define TIFFVGetField itk_TIFFVGetField
#define TIFFVGetFieldDefaulted itk_TIFFVGetFieldDefaulted
#define TIFFVSetField itk_TIFFVSetField
#define TIFFVStripSize itk_TIFFVStripSize
#define TIFFVTileSize itk_TIFFVTileSize
#define TIFFWarning itk_TIFFWarning
#define TIFFWriteBufferSetup itk_TIFFWriteBufferSetup
#define TIFFWriteCheck itk_TIFFWriteCheck
#define TIFFWriteDirectory itk_TIFFWriteDirectory
#define TIFFWriteEncodedStrip itk_TIFFWriteEncodedStrip
#define TIFFWriteEncodedTile itk_TIFFWriteEncodedTile
#define TIFFWriteRawStrip itk_TIFFWriteRawStrip
#define TIFFWriteRawTile itk_TIFFWriteRawTile
#define TIFFWriteScanline itk_TIFFWriteScanline
#define TIFFWriteTile itk_TIFFWriteTile
#define XYZtoRGB24 itk_XYZtoRGB24
#define _TIFFDefaultStripSize itk__TIFFDefaultStripSize
#define _TIFFDefaultTileSize itk__TIFFDefaultTileSize
#define _TIFFFax3fillruns itk__TIFFFax3fillruns
#define TIFFFieldWithTag itk_TIFFFieldWithTag
#define TIFFFindFieldInfo itk_TIFFFindFieldInfo
#define TIFFMergeFieldInfo itk_TIFFMergeFieldInfo

#define _TIFFNoPostDecode itk__TIFFNoPostDecode
#define _TIFFNoPreCode itk__TIFFNoPreCode
#define _TIFFNoRowDecode itk__TIFFNoRowDecode
#define _TIFFNoRowEncode itk__TIFFNoRowEncode
#define _TIFFNoSeek itk__TIFFNoSeek
#define _TIFFNoStripDecode itk__TIFFNoStripDecode
#define _TIFFNoStripEncode itk__TIFFNoStripEncode
#define _TIFFNoTileDecode itk__TIFFNoTileDecode
#define _TIFFNoTileEncode itk__TIFFNoTileEncode
#define _TIFFPrintFieldInfo itk__TIFFPrintFieldInfo
#define _TIFFSampleToTagType itk__TIFFSampleToTagType
#define _TIFFSetDefaultCompressionState itk__TIFFSetDefaultCompressionState
#define _TIFFSetupFieldInfo itk__TIFFSetupFieldInfo
#define _TIFFSwab16BitData itk__TIFFSwab16BitData
#define _TIFFSwab32BitData itk__TIFFSwab32BitData
#define _TIFFSwab64BitData itk__TIFFSwab64BitData
#define _TIFFfree itk__TIFFfree
#define _TIFFgetMode itk__TIFFgetMode
#define _TIFFmalloc itk__TIFFmalloc
#define _TIFFmemcmp itk__TIFFmemcmp
#define _TIFFmemcpy itk__TIFFmemcpy
#define _TIFFmemset itk__TIFFmemset
#define _TIFFprintAscii itk__TIFFprintAscii
#define _TIFFprintAsciiTag itk__TIFFprintAsciiTag
#define _TIFFrealloc itk__TIFFrealloc
#define _TIFFsetByteArray itk__TIFFsetByteArray
#define _TIFFsetDoubleArray itk__TIFFsetDoubleArray
#define _TIFFsetFloatArray itk__TIFFsetFloatArray
#define _TIFFsetLongArray itk__TIFFsetLongArray
#define _TIFFsetNString itk__TIFFsetNString
#define _TIFFsetShortArray itk__TIFFsetShortArray
#define _TIFFsetString itk__TIFFsetString
#define uv_decode itk_uv_decode
#define uv_encode itk_uv_encode
#define TIFFCvtIEEEFloatToNative itk_TIFFCvtIEEEFloatToNative
#define TIFFCvtIEEEDoubleToNative itk_TIFFCvtIEEEDoubleToNative
#define TIFFYCbCrToRGBInit itk_TIFFYCbCrToRGBInit
#define TIFFYCbCrtoRGB itk_TIFFYCbCrtoRGB
#define TIFFCIELabToRGBInit itk_TIFFCIELabToRGBInit
#define TIFFXYZToRGB itk_TIFFXYZToRGB
#define TIFFCIELabToXYZ itk_TIFFCIELabToXYZ
#define TIFFCvtNativeToIEEEFloat itk_TIFFCvtNativeToIEEEFloat
#define _TIFFFaxBlackTable itk__TIFFFaxBlackTable
#define _TIFFFaxWhiteTable itk__TIFFFaxWhiteTable
#define _TIFFFaxMainTable itk__TIFFFaxMainTable
#define TIFFDataWidth itk_TIFFDataWidth
#define _TIFFCreateAnonFieldInfo itk__TIFFCreateAnonFieldInfo
#define _TIFFFindOrRegisterFieldInfo itk__TIFFFindOrRegisterFieldInfo
#define TIFFRawStripSize itk__TIFFRawStripSize
#define TIFFFillStrip itk_TIFFFillStrip
#define TIFFFillTile itk_TIFFFillTile
#define TIFFSetupStrips itk_TIFFSetupStrips
#define TIFFCheckpointDirectory itk_TIFFCheckpointDirectory

#define TIFFAccessTagMethods itk_TIFFAccessTagMethods
#define TIFFCleanup itk_TIFFCleanup
#define TIFFClientdata itk_TIFFClientdata
#define TIFFGetClientInfo itk_TIFFGetClientInfo
#define TIFFGetCloseProc itk_TIFFGetCloseProc
#define TIFFGetConfiguredCODECs itk_TIFFGetConfiguredCODECs
#define TIFFGetMapFileProc itk_TIFFGetMapFileProc
#define TIFFGetReadProc itk_TIFFGetReadProc
#define TIFFGetSeekProc itk_TIFFGetSeekProc
#define TIFFGetSizeProc itk_TIFFGetSizeProc
#define TIFFGetTagListCount itk_TIFFGetTagListCount
#define TIFFGetTagListEntry itk_TIFFGetTagListEntry
#define TIFFGetUnmapFileProc itk_TIFFGetUnmapFileProc
#define TIFFGetWriteProc itk_TIFFGetWriteProc
#define TIFFIsBigEndian itk_TIFFIsBigEndian
#define TIFFIsCODECConfigured itk_TIFFIsCODECConfigured
#define TIFFReadRGBAImageOriented itk_TIFFReadRGBAImageOriented
#define TIFFSetClientdata itk_TIFFSetClientdata
#define TIFFSetClientInfo itk_TIFFSetClientInfo
#define TIFFSetFileName itk_TIFFSetFileName
#define TIFFSetFileno itk_TIFFSetFileno
#define TIFFSetMode itk_TIFFSetMode

#define display_sRGB itk_display_sRGB
#define _TIFFBuiltinCODECS itk_TIFFBuiltinCODECS
#endif
