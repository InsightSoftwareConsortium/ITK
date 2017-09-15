/* 
 (C) Jan de Vaan 2007-2010, all rights reserved. See the accompanying "License.txt" for licensed use. 
*/ 


#ifndef JLS_INTERFACE
#define JLS_INTERFACE

#include "publictypes.h"

/* non-windows (static linking) */
#if !defined(CHARLS_IMEXPORT) && !defined(_WIN32)
#  define CHARLS_IMEXPORT(returntype) returntype
#endif

/* windows static linking */
#if !defined(CHARLS_IMEXPORT) && defined(CHARLS_STATIC)
#  define CHARLS_IMEXPORT(returntype) returntype
#endif

/* windows dll */
#if !defined(CHARLS_IMEXPORT) && defined(CHARLS_DLL)
#  define CHARLS_IMEXPORT(returntype) __declspec(dllimport) returntype __stdcall
#endif

#if !defined(CHARLS_IMEXPORT)
#error Please #define CHARLS_STATIC or CHARLS_DLL before including "interface.h" to indicate if CharLS is built as a static library or as a dll. 
#endif


#ifdef __cplusplus

#include <iostream>

extern "C" 
{

#endif
  CHARLS_IMEXPORT(enum JLS_ERROR) JpegLsEncode(void* compressedData, size_t compressedLength, size_t* byteCountWritten, 
	    const void* uncompressedData, size_t uncompressedLength, struct JlsParameters* info);

  CHARLS_IMEXPORT(enum JLS_ERROR) JpegLsDecode(void* uncompressedData, size_t uncompressedLength, 
		const void* compressedData, size_t compressedLength, 
		struct JlsParameters* info);

  CHARLS_IMEXPORT(enum JLS_ERROR) JpegLsDecodeRect(void* uncompressedData, size_t uncompressedLength, 
		const void* compressedData, size_t compressedLength, 
		struct JlsRect rect, struct JlsParameters* info);

  CHARLS_IMEXPORT(enum JLS_ERROR) JpegLsReadHeader(const void* compressedData, size_t compressedLength, 
		struct JlsParameters* info);

  CHARLS_IMEXPORT(enum JLS_ERROR) JpegLsVerifyEncode(const void* uncompressedData, size_t uncompressedLength, 
		const void* compressedData, size_t compressedLength);

  
#ifdef __cplusplus

}
	CHARLS_IMEXPORT(enum JLS_ERROR) JpegLsEncodeStream(ByteStreamInfo rawStream, size_t* bytesWritten, ByteStreamInfo inputStream, struct JlsParameters* info);
	CHARLS_IMEXPORT(enum JLS_ERROR) JpegLsDecodeStream(ByteStreamInfo output, ByteStreamInfo input, struct JlsParameters* info);
	CHARLS_IMEXPORT(enum JLS_ERROR) JpegLsReadHeaderStream(ByteStreamInfo input, struct JlsParameters* info);

#endif

#endif
