//
// (C) CharLS Team 2014, all rights reserved. See the accompanying "License.txt" for licensed use. 
//

#ifndef CHARLS_JPEGSTREAMWRITER
#define CHARLS_JPEGSTREAMWRITER

#include "util.h"
#include "jpegsegment.h"
#include <vector>


//
// Purpose: 'Writer'class that can generate JPEG-LS file streams.
//
class JpegStreamWriter
{
	friend class JpegMarkerSegment;
	friend class JpegImageDataSegment;

public:
	JpegStreamWriter(const JfifParameters& jfifParameters, Size size, LONG bitsPerSample, LONG ccomp);
	virtual ~JpegStreamWriter();

	void AddSegment(JpegSegment* segment)
	{
		_segments.push_back(segment);
	}

	void AddScan(ByteStreamInfo info, const JlsParameters* pparams);

	void AddLSE(const JlsCustomParameters* pcustom);

	void AddColorTransform(int i);

	size_t GetBytesWritten()
	{
		return _byteOffset;
	}

	size_t GetLength()
	{
		return _data.count - _byteOffset;
	}

	size_t Write(ByteStreamInfo info);

	void EnableCompare(bool bCompare)
	{
		_bCompare = bCompare;
	}

private:
	BYTE* GetPos() const
	{
		return _data.rawData + _byteOffset;
	}

	ByteStreamInfo OutputStream() const
	{
		ByteStreamInfo data = _data;
		data.count -= _byteOffset;
		data.rawData += _byteOffset;
		return data;
	}

	void WriteByte(BYTE val)
	{
		ASSERT(!_bCompare || _data.rawData[_byteOffset] == val);

		if (_data.rawStream != NULL)
		{
			_data.rawStream->sputc(val);
		}
		else
		{
			if (_byteOffset >= _data.count)
				throw JlsException(CompressedBufferTooSmall);

			_data.rawData[_byteOffset++] = val;
		}
	}

	void WriteBytes(const std::vector<BYTE>& rgbyte)
	{
		for (size_t i = 0; i < rgbyte.size(); ++i)
		{
			WriteByte(rgbyte[i]);
		}
	}

	void WriteWord(USHORT val)
	{
		WriteByte(BYTE(val / 0x100));
		WriteByte(BYTE(val % 0x100));
	}

	void Seek(size_t byteCount)
	{
		if (_data.rawStream != NULL)
			return;

		_byteOffset += byteCount;
	}

private:
	bool _bCompare;
	ByteStreamInfo _data;
	size_t _byteOffset;
	LONG _lastCompenentIndex;
	std::vector<JpegSegment*> _segments;
};

#endif
