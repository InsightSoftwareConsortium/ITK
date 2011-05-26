//
// (C) Jan de Vaan 2007-2009, all rights reserved. See the accompanying "License.txt" for licensed use.
//


#ifndef JLS_INTERFACE
#define JLS_INTERFACE


enum JLS_ERROR
{
  OK = 0,
  InvalidJlsParameters,
  ParameterValueNotSupported,
  UncompressedBufferTooSmall,
  CompressedBufferTooSmall,
  InvalidCompressedData,
  ImageTypeNotSupported,
  UnsupportedBitDepthForTransform,
  UnsupportedColorTransform
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

struct JfifParamaters
{
  int   Ver;
  char  units;
  int   XDensity;
  int   YDensity;
  short Xthumb;
  short Ythumb;
  void* pdataThumbnail; // user must set buffer which size is Xthumb*Ythumb*3(RGB) before JpegLsDecode()
};

struct JlsParamaters
{
  int width;
  int height;
  int bitspersample;
  int bytesperline;  // for [source (at encoding)][decoded (at decoding)] pixel image in user buffer
  int components;
  int allowedlossyerror;
  interleavemode ilv;
  int colorTransform;
  bool outputBgr;
  JlsCustomParameters custom;
  JfifParamaters jfif;
};


#if defined(_WIN32)
#ifndef CHARLS_IMEXPORT
#define CHARLS_IMEXPORT __declspec(dllimport)
#pragma comment (lib,"charls.lib")
#endif
#else
#ifndef CHARLS_IMEXPORT
#define CHARLS_IMEXPORT
#endif
#endif /* _WIN32 */


extern "C"
{
  CHARLS_IMEXPORT JLS_ERROR JpegLsEncode(void* pdataCompressed, size_t cbyteBuffer, size_t* pcbyteWritten, const void* pdataUncompressed, size_t cbyteUncompressed, const JlsParamaters* pparams);
  CHARLS_IMEXPORT JLS_ERROR JpegLsDecode(void* pdataUncompressed, size_t cbyteUncompressed, const void* pdataCompressed, size_t cbyteCompressed, JlsParamaters* info = NULL);
  CHARLS_IMEXPORT JLS_ERROR JpegLsReadHeader(const void* pdataUncompressed, size_t cbyteUncompressed, JlsParamaters* pparams);
  CHARLS_IMEXPORT JLS_ERROR JpegLsVerifyEncode(const void* pdataUncompressed, size_t cbyteUncompressed, const void* pdataCompressed, size_t cbyteCompressed);
}


#endif
