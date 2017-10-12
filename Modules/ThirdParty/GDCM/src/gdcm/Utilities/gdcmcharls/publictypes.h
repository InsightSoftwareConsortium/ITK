/* 
  (C) Jan de Vaan 2007-2010, all rights reserved. See the accompanying "License.txt" for licensed use. 
*/ 
#ifndef CHARLS_PUBLICTYPES
#define CHARLS_PUBLICTYPES

#include "config.h"

#ifdef __cplusplus
#include <iostream>
#include <cstddef>
#endif

enum JLS_ERROR
{
	OK = 0,
	InvalidJlsParameters,
	ParameterValueNotSupported,
	UncompressedBufferTooSmall,
	CompressedBufferTooSmall,
	InvalidCompressedData,           // This error is returned when the encoded bit stream contains a general structural problem.
	TooMuchCompressedData,
	ImageTypeNotSupported,           // This error is returned when the bit stream is encoded with an option that is not supported by this implementation.
	UnsupportedBitDepthForTransform,
	UnsupportedColorTransform,
	UnsupportedEncoding,             // This error is returned when an encoded frame is found that is not encoded with the JPEG-LS algorithm.
	UnknownJpegMarker,               // This error is returned when an unknown JPEG marker code is detected in the encoded bit stream.
	MissingJpegMarkerStart           // This error is returned when the algorithm expect a 0xFF code (indicates start of a JPEG marker) but none was found.
};


enum interleavemode
{
	ILV_NONE = 0,
	ILV_LINE = 1,
	ILV_SAMPLE = 2
};



struct JlsCustomParameters
{
	int MAXVAL;
	int T1;
	int T2;
	int T3;
	int RESET;
};


struct JlsRect
{
	int X, Y;
	int Width, Height;
};


struct JfifParameters
{
	int   Ver;
	char  units;
	int   XDensity;
	int   YDensity;
	short Xthumb;
	short Ythumb;
	void* pdataThumbnail; /* user must set buffer which size is Xthumb*Ythumb*3(RGB) before JpegLsDecode() */
};


struct JlsParameters
{
	int width;
	int height;
	int bitspersample;
	int bytesperline;	/* for [source (at encoding)][decoded (at decoding)] pixel image in user buffer */
	int components;
	int allowedlossyerror;
	enum interleavemode ilv;
	int colorTransform;
	char outputBgr;
	struct JlsCustomParameters custom;
	struct JfifParameters jfif;
};



enum JPEGLS_ColorXForm
{
	// default (RGB)
	COLORXFORM_NONE = 0,

	// Color transforms as defined by HP
	// Not part of the JPEG-LS standard in any way, provided for compatibility with existing streams.	
	COLORXFORM_HP1,
	COLORXFORM_HP2,
	COLORXFORM_HP3,

	// Defined by HP but not supported by CharLS
	COLORXFORM_RGB_AS_YUV_LOSSY,
	COLORXFORM_MATRIX,
	XFORM_BIGENDIAN = 1 << 29,
	XFORM_LITTLEENDIAN = 1 << 30
};


#ifdef __cplusplus

// 
// ByteStreamInfo & FromByteArray helper function
//
// ByteStreamInfo describes the stream: either set rawStream to a valid stream, or rawData/count, not both.
// it's possible to decode to memorystreams, but using rawData will always be faster.
//
// Example use: 
//     ByteStreamInfo streamInfo = { fileStream.rdbuf() };
// or 
//     ByteStreamInfo streamInfo = FromByteArray( bytePtr, byteCount);
//
struct ByteStreamInfo
{
	std::basic_streambuf<char>* rawStream;
	BYTE* rawData;
	std::size_t count;
};


inline ByteStreamInfo FromByteArray(const void* bytes, std::size_t count)
{
	ByteStreamInfo info = ByteStreamInfo();
	info.rawData = (BYTE*)bytes;
	info.count = count;
	return info;
}

#endif

#endif
