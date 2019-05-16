/* 
 (C) Jan de Vaan 2007-2010, all rights reserved. See the accompanying "License.txt" for licensed use. 
*/ 


#ifndef CHARLS_CHARLS
#define CHARLS_CHARLS

#include "publictypes.h"

// Windows and building CharLS DLL itself.
#if defined(_WIN32) && defined(CHARLS_DLL_BUILD)
#define CHARLS_IMEXPORT(returntype) __declspec(dllexport) returntype __stdcall
#endif

// Non-windows (static linking)
#if !defined(CHARLS_IMEXPORT) && !defined(_WIN32)
#  define CHARLS_IMEXPORT(returntype) returntype
#endif

// Windows static linking
#if !defined(CHARLS_IMEXPORT) && defined(CHARLS_STATIC)
#  define CHARLS_IMEXPORT(returntype) returntype
#endif

// Windows dll
#if !defined(CHARLS_IMEXPORT) && defined(CHARLS_DLL)
#  define CHARLS_IMEXPORT(returntype) __declspec(dllimport) returntype __stdcall
#endif

#if !defined(CHARLS_IMEXPORT)
#error Please #define CHARLS_STATIC or CHARLS_DLL before including "charls.h" to indicate if CharLS is built as a static library or as a dll.
#endif


#ifdef __cplusplus

extern "C"
{

#else

#include <stddef.h>

#endif
    /// <summary>
    /// Encodes a byte array with pixel data to a JPEG-LS encoded (compressed) byte array.
    /// </summary>
    /// <param name="destination">Byte array that holds the encoded bytes when the funtion returns.</param>
    /// <param name="destinationLength">Length of the array in bytes. If the array is too small the function will return an error.</param>
    /// <param name="bytesWritten">This parameter will hold the number of bytes written to the destination byte array. Cannot be NULL.</param>
    /// <param name="source">Byte array that holds the pixels that should be encoded.</param>
    /// <param name="sourceLength">Length of the array in bytes.</param>
    /// <param name="params">Parameter object that describes the pixel data and how to encode it.</param>
    /// <param name="errorMessage">Character array of at least 256 characters or NULL. Hold the error message when a failure occurs, empty otherwise.</param>
    CHARLS_IMEXPORT(CharlsApiResultType) JpegLsEncode(void* destination, size_t destinationLength, size_t* bytesWritten,
        const void* source, size_t sourceLength, const struct JlsParameters* params, char* errorMessage);

    /// <summary>
    /// Retrieves the JPEG-LS header. This info can be used to pre-allocate the uncompressed output buffer.
    /// </summary>
    /// <param name="compressedData">Byte array that holds the JPEG-LS encoded data of which the header should be extracted.</param>
    /// <param name="compressedLength">Length of the array in bytes.</param>
    /// <param name="params">Parameter object that describes how the pixel data is encoded.</param>
    /// <param name="errorMessage">Character array of at least 256 characters or NULL. Hold the error message when a failure occurs, empty otherwise.</param>
    CHARLS_IMEXPORT(CharlsApiResultType) JpegLsReadHeader(const void* compressedData, size_t compressedLength,
        struct JlsParameters* params, char* errorMessage);

    /// <summary>
    /// Encodes a JPEG-LS encoded byte array to uncompressed pixel data byte array.
    /// </summary>
    /// <param name="destination">Byte array that holds the uncompressed pixel data bytes when the funtion returns.</param>
    /// <param name="destinationLength">Length of the array in bytes. If the array is too small the function will return an error.</param>
    /// <param name="source">Byte array that holds the JPEG-LS encoded data that should be decoded.</param>
    /// <param name="sourceLength">Length of the array in bytes.</param>
    /// <param name="params">Parameter object that describes the pixel data and how to decode it.</param>
    /// <param name="errorMessage">Character array of at least 256 characters or NULL. Hold the error message when a failure occurs, empty otherwise.</param>
    CHARLS_IMEXPORT(CharlsApiResultType) JpegLsDecode(void* destination, size_t destinationLength,
        const void* source, size_t sourceLength, const struct JlsParameters* params, char* errorMessage);

    CHARLS_IMEXPORT(CharlsApiResultType) JpegLsDecodeRect(void* uncompressedData, size_t uncompressedLength,
        const void* compressedData, size_t compressedLength,
        struct JlsRect rect, struct JlsParameters* params, char* errorMessage);

    CHARLS_IMEXPORT(CharlsApiResultType) JpegLsVerifyEncode(const void* uncompressedData, size_t uncompressedLength,
        const void* compressedData, size_t compressedLength, char* errorMessage);

#ifdef __cplusplus

}
    CHARLS_IMEXPORT(CharlsApiResultType) JpegLsEncodeStream(ByteStreamInfo rawStream, size_t& bytesWritten, ByteStreamInfo inputStream, const JlsParameters& info, char* errorMessage);
    CHARLS_IMEXPORT(CharlsApiResultType) JpegLsDecodeStream(ByteStreamInfo output, ByteStreamInfo input, const JlsParameters* info, char* errorMessage);
    CHARLS_IMEXPORT(CharlsApiResultType) JpegLsReadHeaderStream(ByteStreamInfo input, JlsParameters* params, char* errorMessage);

#endif

#endif
