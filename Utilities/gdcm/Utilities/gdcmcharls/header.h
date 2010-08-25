//
// (C) Jan de Vaan 2007-2009, all rights reserved. See the accompanying "License.txt" for licensed use.
//


#ifndef CHARLS_HEADER
#define CHARLS_HEADER

#include "streams.h"

#define JPEG_SOI  0xD8
#define JPEG_EOI  0xD9
#define JPEG_SOS  0xDA

#define JPEG_SOF  0xF7
#define JPEG_LSE  0xF8
#define JPEG_DNL  0xDC
#define JPEG_DRI  0xDD
#define JPEG_RSTm  0xD0
#define JPEG_COM  0xFE
#define JPEG_APP0 0xE0 // JFIF
#define JPEG_APP7 0xE7 // colorspace
#define JPEG_APP8 0xE8 // colorXForm


class JLSOutputStream;


template<class STRATEGY>
class JlsCodecFactory
{
public:
  STRATEGY* GetCodec(const JlsParamaters& info, const JlsCustomParameters&);
private:
  STRATEGY* GetCodecImpl(const JlsParamaters& info);
};


//
// JpegSegment
//
class JpegSegment
{
protected:
  JpegSegment() {}
public:
  virtual ~JpegSegment() {}
  virtual void Write(JLSOutputStream* pstream) = 0;
};

#endif
