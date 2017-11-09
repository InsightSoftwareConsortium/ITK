// 
// (C) Jan de Vaan 2007-2010, all rights reserved. See the accompanying "License.txt" for licensed use. 
// 


#ifndef CHARLS_HEADER
#define CHARLS_HEADER

#include "jpegmarker.h"

// JPEG Marker codes have the pattern 0xFFaa. The valid 'aa' options are defined by several ITU / IEC standards.
// 0x00, 0x01, 0xFE, 0xC0-0xDF are defined in ITU T.81/IEC 10918-1
// 0xF0 - 0xF6 are defined in ITU T.84/IEC 10918-3 JPEG extensions
// 0xF7 - 0xF8 are defined in ITU T.87/IEC 14495-1 JPEG LS
// 0x4F - 0x6F, 0x90 - 0x93 are defined in JPEG 2000 IEC 15444-1

#define JPEG_SOI    0xD8 // Start Of Image
#define JPEG_EOI    0xD9 // End Of Image
#define JPEG_SOS    0xDA // Start Of Scan

// The following markers are defined in ITU T.81 | ISO IEC 10918-1.
#define JPEG_SOF_0  0xC0 // Start Of Frame (Baseline jpeg)
#define JPEG_SOF_1  0xC1 // Start Of Frame (Extended sequential, huffman)
#define JPEG_SOF_2  0xC2 // Start Of Frame (progressive, huffman)
#define JPEG_SOF_3  0xC3 // Start Of Frame (lossless, huffman)
#define JPEG_SOF_5  0xC5 // Start Of Frame (differential sequential, huffman)
#define JPEG_SOF_6  0xC6 // Start Of Frame (differential progressive, huffman)
#define JPEG_SOF_7  0xC7 // Start Of Frame (differential lossless, huffman)
#define JPEG_JPG    0xC8 // Reserved for JPEG extension
#define JPEG_SOF_9  0xC9 // Start Of Frame (extended sequential, arithmetic)
#define JPEG_SOF_10 0xCA // Start Of Frame (progressive, arithmetic)
#define JPEG_SOF_11 0xCB // Start Of Frame (lossless, arithmetic)

#define JPEG_SOF_55 0xF7 // Start Of Frame (JPEG-LS encoded)
#define JPEG_LSE    0xF8 // JPEG-LS extended parameters
#define JPEG_DNL    0xDC
#define JPEG_DRI    0xDD
#define JPEG_RSTm   0xD0
#define JPEG_COM    0xFE
#define JPEG_APP0   0xE0 // Application data 0: used for JFIF header.
#define JPEG_APP7   0xE7 // Application data 7: colorspace
#define JPEG_APP8   0xE8 // Application data 8: colorXForm



// Default bin sizes for JPEG-LS statistical modeling. Can be overriden at compression time, however this is rarely done.
const int BASIC_T1 = 3;
const int BASIC_T2 = 7;
const int BASIC_T3 = 21;

const LONG BASIC_RESET = 64;

class JpegMarkerWriter;


template<class STRATEGY>
class JlsCodecFactory 
{
public:
	std::auto_ptr<STRATEGY> GetCodec(const JlsParameters& info, const JlsCustomParameters&);
private:
	STRATEGY* GetCodecImpl(const JlsParameters& info);
};

JLS_ERROR CheckParameterCoherent(const JlsParameters* pparams);

JlsCustomParameters ComputeDefault(LONG MAXVAL, LONG NEAR);


#endif
