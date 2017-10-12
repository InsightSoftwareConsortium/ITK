//
// (C) CharLS Team 2014, all rights reserved. See the accompanying "License.txt" for licensed use. 
//

#include "jpegmarkersegment.h"
#include "header.h"
#include "util.h"
#include <vector>


JpegMarkerSegment* JpegMarkerSegment::CreateStartOfFrameMarker(Size size, LONG bitsPerSample, LONG ccomp)
{
	std::vector<BYTE> vec;
	vec.push_back(static_cast<BYTE>(bitsPerSample));
	push_back(vec, static_cast<USHORT>(size.cy));
	push_back(vec, static_cast<USHORT>(size.cx));

	// components
	vec.push_back(static_cast<BYTE>(ccomp));
	for (BYTE component = 0; component < ccomp; component++)
	{
		// rescaling
		vec.push_back(component + 1);
		vec.push_back(0x11);
		//"Tq1" reserved, 0
		vec.push_back(0);
	}

	return new JpegMarkerSegment(JPEG_SOF_55, vec);
}


JpegMarkerSegment* JpegMarkerSegment::CreateJpegFileInterchangeFormatMarker(const JfifParameters& jfifParameters)
{
	BYTE jfifID [] = { 'J', 'F', 'I', 'F', '\0' };

	std::vector<BYTE> rgbyte;
	for (int i = 0; i < (int)sizeof(jfifID); i++)
	{
		rgbyte.push_back(jfifID[i]);
	}

	push_back(rgbyte, (USHORT) jfifParameters.Ver);

	rgbyte.push_back(jfifParameters.units);
	push_back(rgbyte, (USHORT) jfifParameters.XDensity);
	push_back(rgbyte, (USHORT) jfifParameters.YDensity);

	// thumbnail
	rgbyte.push_back((BYTE) jfifParameters.Xthumb);
	rgbyte.push_back((BYTE) jfifParameters.Ythumb);
	if (jfifParameters.Xthumb > 0)
	{
		if (jfifParameters.pdataThumbnail)
			throw JlsException(InvalidJlsParameters);

		rgbyte.insert(rgbyte.end(), (BYTE*) jfifParameters.pdataThumbnail, (BYTE*) jfifParameters.pdataThumbnail + 3 * jfifParameters.Xthumb * jfifParameters.Ythumb);
	}

	return new JpegMarkerSegment(JPEG_APP0, rgbyte);
}


JpegMarkerSegment* JpegMarkerSegment::CreateJpegLSExtendedParametersMarker(const JlsCustomParameters& customParameters)
{
	std::vector<BYTE> rgbyte;

	rgbyte.push_back(1);
	push_back(rgbyte, (USHORT) customParameters.MAXVAL);
	push_back(rgbyte, (USHORT) customParameters.T1);
	push_back(rgbyte, (USHORT) customParameters.T2);
	push_back(rgbyte, (USHORT) customParameters.T3);
	push_back(rgbyte, (USHORT) customParameters.RESET);

	return new JpegMarkerSegment(JPEG_LSE, rgbyte);
}


JpegMarkerSegment* JpegMarkerSegment::CreateColorTransformMarker(int i)
{
	std::vector<BYTE> rgbyteXform;

	rgbyteXform.push_back('m');
	rgbyteXform.push_back('r');
	rgbyteXform.push_back('f');
	rgbyteXform.push_back('x');
	rgbyteXform.push_back((BYTE) i);

	return new JpegMarkerSegment(JPEG_APP8, rgbyteXform);
}


JpegMarkerSegment* JpegMarkerSegment::CreateStartOfScanMarker(const JlsParameters* pparams, LONG icomponent)
{
	BYTE itable = 0;

	std::vector<BYTE> rgbyte;

	if (icomponent < 0)
	{
		rgbyte.push_back((BYTE) pparams->components);
		for (LONG i = 0; i < pparams->components; ++i)
		{
			rgbyte.push_back(BYTE(i + 1));
			rgbyte.push_back(itable);
		}
	}
	else
	{
		rgbyte.push_back(1);
		rgbyte.push_back((BYTE) icomponent);
		rgbyte.push_back(itable);
	}

	rgbyte.push_back(BYTE(pparams->allowedlossyerror));
	rgbyte.push_back(BYTE(pparams->ilv));
	rgbyte.push_back(0); // transform

	return new JpegMarkerSegment(JPEG_SOS, rgbyte);
}
