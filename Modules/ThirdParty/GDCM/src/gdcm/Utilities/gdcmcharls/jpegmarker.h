// 
// (C) Jan de Vaan 2007-2010, all rights reserved. See the accompanying "License.txt" for licensed use. 
// 
#ifndef CHARLS_JPEGMARKER
#define CHARLS_JPEGMARKER

#include <memory>
#include <vector>
#include "util.h"


// This file defines JPEG-LS markers: The header and the actual pixel data. Header markers have fixed length, the pixeldata not.


class JpegSegment;



ByteStreamInfo FromByteArray(const void* bytes, size_t count);
ByteStreamInfo FromStream(std::basic_streambuf<char>* stream);
void SkipBytes(ByteStreamInfo* streamInfo, size_t count);



//
// JpegMarkerReader: minimal implementation to read JPEG markers
//
class JpegMarkerReader
{
public:
	JpegMarkerReader(ByteStreamInfo byteStreamInfo);

	const JlsParameters& GetMetadata() const
		{ return _info; } 

	const JlsCustomParameters& GetCustomPreset() const
	{ return _info.custom; } 

	void Read(ByteStreamInfo info);
	void ReadHeader();

	void EnableCompare(bool bCompare)
		{ _bCompare = bCompare;	}

	void SetInfo(JlsParameters* info) { _info = *info; }

	void SetRect(JlsRect rect) { _rect = rect; }

	void ReadStartOfScan(bool firstComponent);
	BYTE ReadByte();

private:
	void ReadScan(ByteStreamInfo rawPixels);	
	int ReadPresetParameters();
	int ReadComment();
	int ReadStartOfFrame();
	int ReadWord();
	void ReadNBytes(std::vector<char>& dst, int byteCount);
	int ReadMarker(BYTE marker);

	// JFIF
	void ReadJfif();
	// Color Transform Application Markers & Code Stream (HP extension)
	int ReadColorSpace();
	int ReadColorXForm();

private:
	ByteStreamInfo _byteStream;
	bool _bCompare;
	JlsParameters _info;
	JlsRect _rect;
};


#endif
