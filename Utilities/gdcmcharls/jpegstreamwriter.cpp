//
// (C) CharLS Team 2014, all rights reserved. See the accompanying "License.txt" for licensed use. 
//


#include "jpegstreamwriter.h"
#include "jpegmarkersegment.h"
#include "jpegimagedatasegment.h"
#include "header.h"
#include "util.h"
#include <vector>


namespace
{
	bool IsDefault(const JlsCustomParameters* pcustom)
	{
		if (pcustom->MAXVAL != 0)
			return false;

		if (pcustom->T1 != 0)
			return false;

		if (pcustom->T2 != 0)
			return false;

		if (pcustom->T3 != 0)
			return false;

		if (pcustom->RESET != 0)
			return false;

		return true;
	}
}


JpegStreamWriter::JpegStreamWriter(const JfifParameters& jfifParameters, Size size, LONG bitsPerSample, LONG ccomp) :
_bCompare(false),
_data(),
_byteOffset(0),
_lastCompenentIndex(0)
{
	if (jfifParameters.Ver)
	{
		AddSegment(JpegMarkerSegment::CreateJpegFileInterchangeFormatMarker(jfifParameters));
	}

	AddSegment(JpegMarkerSegment::CreateStartOfFrameMarker(size, bitsPerSample, ccomp));
}


JpegStreamWriter::~JpegStreamWriter()
{
	for (size_t i = 0; i < _segments.size(); ++i)
	{
		delete _segments[i];
	}
}


void JpegStreamWriter::AddColorTransform(int i)
{
	AddSegment(JpegMarkerSegment::CreateColorTransformMarker(i));
}


size_t JpegStreamWriter::Write(ByteStreamInfo info)
{
	_data = info;

	WriteByte(0xFF);
	WriteByte(JPEG_SOI);

	for (size_t i = 0; i < _segments.size(); ++i)
	{
		_segments[i]->Serialize(*this);
	}

	//_bCompare = false;

	WriteByte(0xFF);
	WriteByte(JPEG_EOI);

	return _byteOffset;
}


void JpegStreamWriter::AddScan(ByteStreamInfo info, const JlsParameters* pparams)
{
	if (!IsDefault(&pparams->custom))
	{
		AddSegment(JpegMarkerSegment::CreateJpegLSExtendedParametersMarker(pparams->custom));
	}
	else if (pparams->bitspersample > 12)
	{
		JlsCustomParameters preset = ComputeDefault((1 << pparams->bitspersample) - 1, pparams->allowedlossyerror);
		AddSegment(JpegMarkerSegment::CreateJpegLSExtendedParametersMarker(preset));
	}

	_lastCompenentIndex += 1;
	AddSegment(JpegMarkerSegment::CreateStartOfScanMarker(pparams, pparams->ilv == ILV_NONE ? _lastCompenentIndex : -1));

	int ccomp = pparams->ilv == ILV_NONE ? 1 : pparams->components;

	AddSegment(new JpegImageDataSegment(info, *pparams, ccomp));
}
