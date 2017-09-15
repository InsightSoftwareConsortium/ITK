// 
// (C) Jan de Vaan 2007-2010, all rights reserved. See the accompanying "License.txt" for licensed use. 
// 
#ifndef CHARLS_PROCESSLINE
#define CHARLS_PROCESSLINE

#include "colortransform.h"
#include <iostream>
#include <vector>

#ifdef _MSC_VER 
#pragma warning(disable: 4996) // disable 'function': was declared deprecated also 'std::<function name>': Function call with parameters that may be unsafe
#endif

//
// This file defines the ProcessLine base class, its derivitives and helper functions.
// During coding/decoding, CharLS process one line at a time. The different Processline implementations
// convert the uncompressed format to and from the internal format for encoding.
// Conversions include color transforms, line interleaved vs sample interleaved, masking out unused bits,
// accounting for line padding etc.
// This mechanism could be used to encode/decode images as they are received.
//

class ProcessLine
{
public:
	virtual ~ProcessLine() {}
	virtual void NewLineDecoded(const void* pSrc, int pixelCount, int sourceStride) = 0;
	virtual void NewLineRequested(void* pDest, int pixelCount, int destStride) = 0;
};


class PostProcesSingleComponent : public ProcessLine
{
public:
	PostProcesSingleComponent(void* rawData, const JlsParameters& info, int bytesPerPixel) :
		_rawData((BYTE*)rawData), 
		_bytesPerPixel(bytesPerPixel),
		_bytesPerLine(info.bytesperline)
	{
	}

	void NewLineRequested(void* dest, int pixelCount, int /*byteStride*/)
	{
		::memcpy(dest, _rawData, pixelCount * _bytesPerPixel);
		_rawData += _bytesPerLine;
	}

	void NewLineDecoded(const void* pSrc, int pixelCount, int /*sourceStride*/)
	{
		::memcpy(_rawData, pSrc, pixelCount * _bytesPerPixel);
		_rawData += _bytesPerLine;
	}

private:
	BYTE* _rawData;
	int _bytesPerPixel;
	int _bytesPerLine;
};


inline void ByteSwap(unsigned char* data, int count)
{
	if (count & 1)
		throw JlsException(InvalidJlsParameters);

	unsigned int* data32 = (unsigned int*)data;
	for(int i = 0; i < count/4; i++)
	{
		unsigned int value = data32[i];
		data32[i] = ((value >> 8) & 0x00FF00FF) | ((value & 0x00FF00FF) << 8);
	}

	if ((count % 4) != 0)
	{
		std::swap(data[count-2], data[count-1]);
	}
}

class PostProcesSingleStream : public ProcessLine
{
public:
	PostProcesSingleStream(std::basic_streambuf<char>* rawData, const JlsParameters& info, int bytesPerPixel) :
		_rawData(rawData), 
		_bytesPerPixel(bytesPerPixel),
		_bytesPerLine(info.bytesperline)
	{
	}

	void NewLineRequested(void* dest, int pixelCount, int /*destStride*/)
	{
		size_t bytesToRead = pixelCount * _bytesPerPixel;
		while (bytesToRead != 0)
		{
			std::streamsize bytesRead = _rawData->sgetn((char*)dest, bytesToRead);
			if (bytesRead == 0)
				throw JlsException(UncompressedBufferTooSmall);

			bytesToRead = (size_t)(bytesToRead - bytesRead);
		}

		if (_bytesPerPixel == 2 )
		{
			ByteSwap((unsigned char*)dest, 2 * pixelCount);
		}

		if (_bytesPerLine - pixelCount * _bytesPerPixel > 0)
		{
			_rawData->pubseekoff(std::streamoff(_bytesPerLine - bytesToRead), std::ios_base::cur);
		}
	}

	void NewLineDecoded(const void* pSrc, int pixelCount, int /*sourceStride*/)
	{
		int bytesToWrite = pixelCount * _bytesPerPixel;
		std::streamsize bytesWritten = _rawData->sputn((const char*)pSrc, bytesToWrite);
		if (bytesWritten != bytesToWrite)
			throw JlsException(UncompressedBufferTooSmall);
	}

private:
	std::basic_streambuf<char>* _rawData;
	int _bytesPerPixel;
	int _bytesPerLine;
};


template<class TRANSFORM, class SAMPLE>
void TransformLineToQuad(const SAMPLE* ptypeInput, LONG pixelStrideIn, Quad<SAMPLE>* pbyteBuffer, LONG pixelStride, TRANSFORM& transform)
{
	int cpixel = MIN(pixelStride, pixelStrideIn);
	Quad<SAMPLE>* ptypeBuffer = (Quad<SAMPLE>*)pbyteBuffer;

	for (int x = 0; x < cpixel; ++x)
	{
		Quad<SAMPLE> pixel(transform(ptypeInput[x], ptypeInput[x + pixelStrideIn], ptypeInput[x + 2*pixelStrideIn]),ptypeInput[x + 3*pixelStrideIn]) ;
		
		ptypeBuffer[x] = pixel;
	}
}


template<class TRANSFORM, class SAMPLE> 
void TransformQuadToLine(const Quad<SAMPLE>* pbyteInput, LONG pixelStrideIn, SAMPLE* ptypeBuffer, LONG pixelStride, TRANSFORM& transform)
{
	int cpixel = MIN(pixelStride, pixelStrideIn);
	const Quad<SAMPLE>* ptypeBufferIn = (Quad<SAMPLE>*)pbyteInput;

	for (int x = 0; x < cpixel; ++x)
	{
		Quad<SAMPLE> color = ptypeBufferIn[x];
		Quad<SAMPLE> colorTranformed(transform(color.v1, color.v2, color.v3), color.v4);

		ptypeBuffer[x] = colorTranformed.v1;
		ptypeBuffer[x + pixelStride] = colorTranformed.v2;
		ptypeBuffer[x + 2 *pixelStride] = colorTranformed.v3;
		ptypeBuffer[x + 3 *pixelStride] = colorTranformed.v4;
	}
}


template<class SAMPLE>
void TransformRgbToBgr(SAMPLE* pDest, int samplesPerPixel, int pixelCount)
{
	for (int i = 0; i < pixelCount; ++i)
	{
		std::swap(pDest[0], pDest[2]);
		pDest += samplesPerPixel;
	}
}


template<class TRANSFORM, class SAMPLE>
void TransformLine(Triplet<SAMPLE>* pDest, const Triplet<SAMPLE>* pSrc, int pixelCount, TRANSFORM& transform)
{
	for (int i = 0; i < pixelCount; ++i)
	{
		pDest[i] = transform(pSrc[i].v1, pSrc[i].v2, pSrc[i].v3);
	}
}


template<class TRANSFORM, class SAMPLE> 
void TransformLineToTriplet(const SAMPLE* ptypeInput, LONG pixelStrideIn, Triplet<SAMPLE>* pbyteBuffer, LONG pixelStride, TRANSFORM& transform)
{
	int cpixel = MIN(pixelStride, pixelStrideIn);
	Triplet<SAMPLE>* ptypeBuffer = (Triplet<SAMPLE>*)pbyteBuffer;

	for (int x = 0; x < cpixel; ++x)
	{
		ptypeBuffer[x] = transform(ptypeInput[x], ptypeInput[x + pixelStrideIn], ptypeInput[x + 2*pixelStrideIn]);
	}
}


template<class TRANSFORM, class SAMPLE>
void TransformTripletToLine(const Triplet<SAMPLE>* pbyteInput, LONG pixelStrideIn, SAMPLE* ptypeBuffer, LONG pixelStride, TRANSFORM& transform)
{
	int cpixel = MIN(pixelStride, pixelStrideIn);
	const Triplet<SAMPLE>* ptypeBufferIn = (Triplet<SAMPLE>*)pbyteInput;

	for (int x = 0; x < cpixel; ++x)
	{
		Triplet<SAMPLE> color = ptypeBufferIn[x];
		Triplet<SAMPLE> colorTranformed = transform(color.v1, color.v2, color.v3);

		ptypeBuffer[x] = colorTranformed.v1;
		ptypeBuffer[x + pixelStride] = colorTranformed.v2;
		ptypeBuffer[x + 2 *pixelStride] = colorTranformed.v3;
	}
}


template<class TRANSFORM>
class ProcessTransformed : public ProcessLine
{
	typedef typename TRANSFORM::SAMPLE SAMPLE;

public:
	ProcessTransformed(ByteStreamInfo rawStream, const JlsParameters& info, TRANSFORM transform) :
		_info(info),
		_templine(info.width * info.components),
		_buffer(info.width * info.components * sizeof(SAMPLE)),
		_transform(transform),
		_inverseTransform(transform),
		_rawPixels(rawStream)
	{
	}

	void NewLineRequested(void* dest, int pixelCount, int destStride)
	{
		if (_rawPixels.rawStream == NULL)
		{
			Transform(_rawPixels.rawData, dest, pixelCount, destStride);
			_rawPixels.rawData += _info.bytesperline;
			return;
		}

		Transform(_rawPixels.rawStream, dest, pixelCount, destStride);
	}

	void Transform(std::basic_streambuf<char>* rawStream, void* dest, int pixelCount, int destStride)
	{
		std::streamsize bytesToRead = pixelCount * _info.components * sizeof(SAMPLE);
		while(bytesToRead != 0)
		{
			std::streamsize read = rawStream->sgetn((char*)&_buffer[0], bytesToRead);
			if (read == 0)
				throw new JlsException(UncompressedBufferTooSmall);

			bytesToRead -= read;
		}
		if (sizeof(SAMPLE) == 2 && _info.colorTransform == XFORM_BIGENDIAN)
		{
			ByteSwap(&_buffer[0], _info.components * sizeof(SAMPLE) * pixelCount);
		}
		Transform(&_buffer[0], dest, pixelCount, destStride);
	}

	void Transform(const void* source, void* dest, int pixelCount, int destStride)
	{
		if (_info.outputBgr)
		{
			memcpy(&_templine[0], source, sizeof(Triplet<SAMPLE>)*pixelCount);
			TransformRgbToBgr((SAMPLE*)&_templine[0], _info.components, pixelCount);
			source = &_templine[0];
		}

		if (_info.components == 3)
		{
			if (_info.ilv == ILV_SAMPLE)
			{
				TransformLine((Triplet<SAMPLE>*)dest, (const Triplet<SAMPLE>*)source, pixelCount, _transform);
			}
			else
			{
				TransformTripletToLine((const Triplet<SAMPLE>*)source, pixelCount, (SAMPLE*)dest, destStride, _transform);
			}
		}
		else if (_info.components == 4 && _info.ilv == ILV_LINE)
		{
			TransformQuadToLine((const Quad<SAMPLE>*)source, pixelCount, (SAMPLE*)dest, destStride, _transform);
		}
	}


	void DecodeTransform(const void* pSrc, void* rawData, int pixelCount, int byteStride)
	{
		if (_info.components == 3)
		{
			if (_info.ilv == ILV_SAMPLE)
			{
				TransformLine((Triplet<SAMPLE>*)rawData, (const Triplet<SAMPLE>*)pSrc, pixelCount, _inverseTransform);
			}
			else
			{
				TransformLineToTriplet((const SAMPLE*)pSrc, byteStride, (Triplet<SAMPLE>*)rawData, pixelCount, _inverseTransform);
			}
		}
		else if (_info.components == 4 && _info.ilv == ILV_LINE)
		{
			TransformLineToQuad((const SAMPLE*)pSrc, byteStride, (Quad<SAMPLE>*)rawData, pixelCount, _inverseTransform);
		}

		if (_info.outputBgr)
		{
			TransformRgbToBgr((SAMPLE*)rawData, _info.components, pixelCount);
		}
	}

	void NewLineDecoded(const void* pSrc, int pixelCount, int sourceStride)
	{
		if (_rawPixels.rawStream != NULL)
		{
			std::streamsize bytesToWrite = pixelCount * _info.components * sizeof(SAMPLE);
			DecodeTransform(pSrc, &_buffer[0], pixelCount, sourceStride);

			if (sizeof(SAMPLE) == 2 && _info.colorTransform == XFORM_BIGENDIAN)
			{
				ByteSwap(&_buffer[0], _info.components * sizeof(SAMPLE) * pixelCount);
			}

			std::streamsize bytesWritten = _rawPixels.rawStream->sputn((char*)&_buffer[0], bytesToWrite);
			if (bytesWritten != bytesToWrite)
				throw JlsException(UncompressedBufferTooSmall);
		}
		else
		{
			DecodeTransform(pSrc, _rawPixels.rawData, pixelCount, sourceStride);
			_rawPixels.rawData += _info.bytesperline;
		}
	}


private:
	const JlsParameters& _info;
	std::vector<SAMPLE> _templine;
	std::vector<BYTE> _buffer;
	TRANSFORM _transform;
	typename TRANSFORM::INVERSE _inverseTransform;
	ByteStreamInfo _rawPixels;
};



#endif
