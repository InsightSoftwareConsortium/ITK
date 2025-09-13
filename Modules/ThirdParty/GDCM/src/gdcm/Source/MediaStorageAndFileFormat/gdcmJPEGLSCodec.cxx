/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "gdcmJPEGLSCodec.h"
#include "gdcmTransferSyntax.h"
#include "gdcmSequenceOfFragments.h"
#include "gdcmDataElement.h"
#include "gdcmSwapper.h"

#include <numeric>
#include <cstring> // memcpy

// CharLS includes
#include "gdcm_charls.h"


#if defined(GCC_VERSION) && GCC_VERSION < 50101
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

namespace gdcm
{

JPEGLSCodec::JPEGLSCodec():BufferLength(0)/*,Lossless(true)*/,LossyError(0)
{
}

JPEGLSCodec::~JPEGLSCodec()
= default;

void JPEGLSCodec::SetLossless(bool l)
{
  LossyFlag = !l;
}

bool JPEGLSCodec::GetLossless() const
{
  return !LossyFlag;
}

bool JPEGLSCodec::GetHeaderInfo(std::istream &is, TransferSyntax &ts)
{
#ifndef GDCM_USE_JPEGLS
  return false;
#else
  using namespace charls;
  is.seekg( 0, std::ios::end);
  size_t buf_size = (size_t)is.tellg();
  //gdcm_assert(buf_size < INT_MAX);
  char *dummy_buffer = new char[(unsigned int)buf_size];
  is.seekg(0, std::ios::beg);
  is.read( dummy_buffer, buf_size);

  JlsParameters metadata = {};
  if (JpegLsReadHeader(dummy_buffer, buf_size, &metadata, nullptr) != ApiResult::OK)
    {
    return false;
    }
  delete[] dummy_buffer;

  // $1 = {width = 512, height = 512, bitspersample = 8, components = 1, allowedlossyerror = 0, ilv = ILV_NONE, colorTransform = 0, custom = {MAXVAL = 0, T1 = 0, T2 = 0, T3 = 0, RESET = 0}}

  this->Dimensions[0] = metadata.width;
  this->Dimensions[1] = metadata.height;
  if( metadata.bitsPerSample <= 8 )
    {
    this->PF = PixelFormat( PixelFormat::UINT8 );
    }
  else if( metadata.bitsPerSample <= 16 )
    {
    gdcm_assert( metadata.bitsPerSample > 8 );
    this->PF = PixelFormat( PixelFormat::UINT16 );
    }
  else
    {
    gdcm_assert(0);
    }
  this->PF.SetBitsStored( (uint16_t)metadata.bitsPerSample );
  gdcm_assert( this->PF.IsValid() );
//  switch( metadata.bitspersample )
//    {
//  case 8:
//    this->PF = PixelFormat( PixelFormat::UINT8 );
//    break;
//  case 12:
//    this->PF = PixelFormat( PixelFormat::UINT16 );
//    this->PF.SetBitsStored( 12 );
//    break;
//  case 16:
//    this->PF = PixelFormat( PixelFormat::UINT16 );
//    break;
//  default:
//    gdcm_assert(0);
//    }
  if( metadata.components == 1 )
    {
    PI = PhotometricInterpretation::MONOCHROME2;
    this->PF.SetSamplesPerPixel( 1 );
    }
  else if( metadata.components == 3 )
    {
    PI = PhotometricInterpretation::RGB;
    PlanarConfiguration = 0; // CP-1843
    this->PF.SetSamplesPerPixel( 3 );
    }
  else gdcm_assert(0);

  // allowedlossyerror == 0 => Lossless
  LossyFlag = metadata.allowedLossyError != 0;

  if( metadata.allowedLossyError == 0 )
    {
    ts = TransferSyntax::JPEGLSLossless;
    }
  else
    {
    ts = TransferSyntax::JPEGLSNearLossless;
    }

  return true;
#endif
}

bool JPEGLSCodec::CanDecode(TransferSyntax const &ts) const
{
#ifndef GDCM_USE_JPEGLS
  return false;
#else
  return ts == TransferSyntax::JPEGLSLossless
      || ts == TransferSyntax::JPEGLSNearLossless;
#endif
}

bool JPEGLSCodec::CanCode(TransferSyntax const &ts) const
{
#ifndef GDCM_USE_JPEGLS
  return false;
#else
  return ts == TransferSyntax::JPEGLSLossless
      || ts == TransferSyntax::JPEGLSNearLossless;
#endif
}

template<typename T>
static void ConvPlanar(std::vector<unsigned char> &input)
{
  size_t buf_size = input.size();
  gdcm_assert( buf_size % sizeof(T) == 0 );
  size_t npixels = buf_size / sizeof( T );
  gdcm_assert( npixels % 3 == 0 );
  size_t size = npixels / 3;
  T* buffer = (T*)input.data();

  const T *r = buffer;
  const T *g = buffer + size;
  const T *b = buffer + size + size;

  T *copy = new T[ npixels ];
  T *p = copy;
  for (size_t j = 0; j < size; ++j)
    {
    *(p++) = *(r++);
    *(p++) = *(g++);
    *(p++) = *(b++);
    }
  std::memcpy(input.data(), copy, input.size() );
  delete[] copy;
}

bool JPEGLSCodec::DecodeByStreamsCommon(const char *buffer, size_t totalLen, std::vector<unsigned char> &rgbyteOut)
{
  using namespace charls;
  const unsigned char* pbyteCompressed = (const unsigned char*)buffer;
  size_t cbyteCompressed = totalLen;

  JlsParameters params = {};
  if(JpegLsReadHeader(pbyteCompressed, cbyteCompressed, &params, nullptr) != ApiResult::OK )
    {
    gdcmDebugMacro( "Could not parse JPEG-LS header" );
    return false;
    }

  if( params.colorTransformation != charls::ColorTransformation::None )
    {
    gdcmWarningMacro( "APP8 marker found to contains a color transformation. This is an HP extension" );
    }
  // allowedlossyerror == 0 => Lossless
  LossyFlag = params.allowedLossyError!= 0;

  rgbyteOut.resize(params.height *params.width * ((params.bitsPerSample + 7) / 8) * params.components);

  ApiResult result = JpegLsDecode(rgbyteOut.data(), rgbyteOut.size(), pbyteCompressed, cbyteCompressed, &params, nullptr);

  if( params.components == 3 )
    {
    const unsigned int nBytes = (params.bitsPerSample + 7) / 8;
    if( params.interleaveMode == InterleaveMode::None )
      {
      if(nBytes == 1 )
        ConvPlanar<unsigned char>(rgbyteOut);
      else if(nBytes == 2 )
        ConvPlanar<unsigned short>(rgbyteOut);
      else
        gdcm_assert(0);
      }
    }

  if (result != ApiResult::OK)
    {
    gdcmErrorMacro( "Could not decode JPEG-LS stream" );
    return false;
    }

  return true;
}

bool JPEGLSCodec::Decode(DataElement const &in, DataElement &out)
{
#ifndef GDCM_USE_JPEGLS
  return false;
#else
  using namespace charls;
  if( NumberOfDimensions == 2 )
    {
    const SequenceOfFragments *sf = in.GetSequenceOfFragments();
    if (!sf) return false;
    unsigned long totalLen = sf->ComputeByteLength();
    char *buffer = new char[totalLen];
    sf->GetBuffer(buffer, totalLen);

    std::vector<unsigned char> rgbyteOut;
    bool b = DecodeByStreamsCommon(buffer, totalLen, rgbyteOut);
    if( !b ) return false;
    delete[] buffer;

    out = in;

    out.SetByteValue( (char*)rgbyteOut.data(), (uint32_t)rgbyteOut.size() );
    return true;
    }
  else if( NumberOfDimensions == 3 )
    {
    const SequenceOfFragments *sf = in.GetSequenceOfFragments();
    if (!sf) return false;
    if (sf->GetNumberOfFragments() != Dimensions[2]) return false;
    std::stringstream os;
    for(unsigned int i = 0; i < sf->GetNumberOfFragments(); ++i)
      {
      const Fragment &frag = sf->GetFragment(i);
      if( frag.IsEmpty() ) return false;
      const ByteValue *bv = frag.GetByteValue();
      if (!bv) return false;
      size_t totalLen = bv->GetLength();
      char *mybuffer = new char[totalLen];

      bv->GetBuffer(mybuffer, bv->GetLength());

      const unsigned char* pbyteCompressed = (const unsigned char*)mybuffer;
      while( totalLen > 0 && pbyteCompressed[totalLen-1] != 0xd9 )
        {
        totalLen--;
        }
      // what if 0xd9 is never found ?
      gdcm_assert( totalLen > 0 && pbyteCompressed[totalLen-1] == 0xd9 );

      size_t cbyteCompressed = totalLen;

      JlsParameters params = {};
      if( JpegLsReadHeader(pbyteCompressed, cbyteCompressed, &params, nullptr) != ApiResult::OK )
        {
        gdcmDebugMacro( "Could not parse JPEG-LS header" );
        return false;
        }

      // allowedlossyerror == 0 => Lossless
      LossyFlag = params.allowedLossyError!= 0;

      std::vector<unsigned char> rgbyteOut;
      rgbyteOut.resize(params.height *params.width * ((params.bitsPerSample + 7) / 8) * params.components);

      ApiResult result = JpegLsDecode(rgbyteOut.data(), rgbyteOut.size(), pbyteCompressed, cbyteCompressed, &params, nullptr);
      bool r = true;

      delete[] mybuffer;
      if (result != ApiResult::OK)
        {
        return false;
        }
      os.write( (const char*)rgbyteOut.data(), rgbyteOut.size() );

      if(!r) return false;
      gdcm_assert( r == true );
      }
    std::string str = os.str();
    gdcm_assert( !str.empty() );
    out.SetByteValue( str.data(), (uint32_t)str.size() );

    return true;
    }
  return false;

#endif
}

bool JPEGLSCodec::CodeFrameIntoBuffer(char * outdata, size_t outlen, size_t & complen, const char * indata, size_t inlen )
{
#ifndef GDCM_USE_JPEGLS
  return false;
#else
  using namespace charls;
  const unsigned int *dims = this->GetDimensions();
  int image_width = dims[0];
  int image_height = dims[1];

  const PixelFormat &pf = this->GetPixelFormat();
  unsigned int planarConf = this->GetPlanarConfiguration();
  int sample_pixel = pf.GetSamplesPerPixel();
  int bitsallocated = pf.GetBitsAllocated();
  int bitsstored = pf.GetBitsStored();

  JlsParameters params = {};
  /*
  The fields in JlsCustomParameters do not control lossy/lossless. They
  provide the possibility to tune the JPEG-LS internals for better compression
  ratios. Expect a lot of work and testing to achieve small improvements.

  Lossy/lossless is controlled by the field allowedlossyerror. If you put in
  0, encoding is lossless. If it is non-zero, then encoding is lossy. The
  value of 3 is often suggested as a default.

  The nice part about JPEG-LS encoding is that in lossy encoding, there is a
  guaranteed maximum error for each pixel. So a pixel that has value 12,
  encoded with a maximum lossy error of 3, may be decoded as a value between 9
  and 15, but never anything else. In medical imaging this could be a useful
  guarantee.

  The not so nice part is that you may see striping artifacts when decoding
  "non-natural" images. I haven't seen the effects myself on medical images,
  but I suspect screenshots may suffer the most. Also, the bandwidth saving is
  not as big as with other lossy schemes.

  As for 12 bit, I am about to commit a unit test (with the sample you gave
  me) that does a successful round trip encoding of 12 bit color. I did notice
  that for 12 bit, the encoder fails if the unused bits are non-zero, but the
  sample dit not suffer from that.
   */
  params.allowedLossyError = !LossyFlag ? 0 : LossyError;
  params.components = sample_pixel;
  // D_CLUNIE_RG3_JPLY.dcm. The famous 16bits allocated / 10 bits stored with the pixel value = 1024
  // CharLS properly encode 1024 considering it as 10bits data, so the output
  // Using bitsstored for the encoder gives a slightly better compression ratio, and is indeed the
  // right way of doing it.

  // gdcmData/PHILIPS_Gyroscan-8-MONO2-Odd_Sequence.dcm
  if( true || pf.GetPixelRepresentation() )
    {
    // gdcmData/CT_16b_signed-UsedBits13.dcm
    params.bitsPerSample = bitsallocated;
    }
  else
    {
    params.bitsPerSample = bitsstored;
    }
  params.height = image_height;
  params.width = image_width;

  if (sample_pixel == 4)
    {
    params.interleaveMode = InterleaveMode::Sample;
    }
  else if (sample_pixel == 3)
    {
    if(planarConf == 0)
      params.interleaveMode = InterleaveMode::Sample;
    else
      params.interleaveMode = InterleaveMode::None;
    params.colorTransformation = ColorTransformation::None;
    }
  else if (sample_pixel == 1)
      params.interleaveMode = InterleaveMode::None;


  ApiResult error = JpegLsEncode(outdata, outlen, &complen, indata, inlen, &params, nullptr);
  if( error != ApiResult::OK )
    {
    gdcmErrorMacro( "Error compressing: " << (int)error );
    return false;
    }

  gdcm_assert( complen < outlen );

  return true;
#endif
}

// Compress into JPEG
bool JPEGLSCodec::Code(DataElement const &in, DataElement &out)
{
#ifndef GDCM_USE_JPEGLS
  return false;
#else
  out = in;
  //
  // Create a Sequence Of Fragments:
  SmartPointer<SequenceOfFragments> sq = new SequenceOfFragments;

  const unsigned int *dims = this->GetDimensions();
  int image_width = dims[0];
  int image_height = dims[1];

  const ByteValue *bv = in.GetByteValue();
  const char *input = bv->GetPointer();
  unsigned long len = bv->GetLength();
  unsigned long image_len = len / dims[2];
  size_t inputlength = image_len;

  for(unsigned int dim = 0; dim < dims[2]; ++dim)
    {
    const char *inputdata = input + dim * image_len;

    std::vector<unsigned char> rgbyteCompressed;
    rgbyteCompressed.resize(image_width * image_height * 4 * 2); // overallocate in case of weird case

    size_t cbyteCompressed;
    const bool b = this->CodeFrameIntoBuffer((char*)rgbyteCompressed.data(), rgbyteCompressed.size(), cbyteCompressed, inputdata, inputlength );
    if( !b ) return false;

    Fragment frag;
    frag.SetByteValue( (char*)rgbyteCompressed.data(), (uint32_t)cbyteCompressed );
    sq->AddFragment( frag );
    }

  gdcm_assert( sq->GetNumberOfFragments() == dims[2] );
  out.SetValue( *sq );

  return true;

#endif
}

void JPEGLSCodec::SetLossyError(int error)
{
  LossyError = error;
}

bool JPEGLSCodec::Decode(DataElement const &, char* , size_t,
              uint32_t , uint32_t , uint32_t ,
              uint32_t , uint32_t , uint32_t )
{
 return false;
}

bool JPEGLSCodec::DecodeExtent(
    char *buffer,
    unsigned int xmin, unsigned int xmax,
    unsigned int ymin, unsigned int ymax,
    unsigned int zmin, unsigned int zmax,
    std::istream & is
  )
{
  BasicOffsetTable bot;
  bot.Read<SwapperNoOp>( is );

  const unsigned int * dimensions = this->GetDimensions();
  const PixelFormat & pf = this->GetPixelFormat();
  gdcm_assert( pf.GetBitsAllocated() % 8 == 0 );
  gdcm_assert( pf != PixelFormat::SINGLEBIT );
  gdcm_assert( pf != PixelFormat::UINT12 && pf != PixelFormat::INT12 );

  if( NumberOfDimensions == 2 )
    {
    char *dummy_buffer = nullptr;
    std::vector<char> vdummybuffer;
    size_t buf_size = 0;

    const Tag seqDelItem(0xfffe,0xe0dd);
    Fragment frag;
    while( frag.ReadPreValue<SwapperNoOp>(is) && frag.GetTag() != seqDelItem )
      {
      size_t fraglen = frag.GetVL();
      size_t oldlen = vdummybuffer.size();
      // update
      buf_size = fraglen + oldlen;
      vdummybuffer.resize( buf_size );
      dummy_buffer = vdummybuffer.data();
      // read J2K
      is.read( &vdummybuffer[oldlen], fraglen );
      }
    gdcm_assert( frag.GetTag() == seqDelItem && frag.GetVL() == 0 );
    gdcm_assert( zmin == zmax );
    gdcm_assert( zmin == 0 );

    std::vector <unsigned char> outv;
    bool b = DecodeByStreamsCommon(dummy_buffer, buf_size, outv);
    if( !b ) return false;

    unsigned char *raw = outv.data();
    const unsigned int rowsize = xmax - xmin + 1;
    const unsigned int colsize = ymax - ymin + 1;
    const unsigned int bytesPerPixel = pf.GetPixelSize();

    if( outv.size() != dimensions[0] * dimensions[1] * bytesPerPixel )
    {
       gdcmDebugMacro( "Inconsistent buffer size. Giving up" );
       return false;
    }

    const unsigned char *tmpBuffer1 = raw;
    unsigned int z = 0;
    for (unsigned int y = ymin; y <= ymax; ++y)
      {
      size_t theOffset = 0 + (z*dimensions[1]*dimensions[0] + y*dimensions[0] + xmin)*bytesPerPixel;
      tmpBuffer1 = raw + theOffset;
      memcpy(&(buffer[((z-zmin)*rowsize*colsize +
            (y-ymin)*rowsize)*bytesPerPixel]),
        tmpBuffer1, rowsize*bytesPerPixel);
      }
    }
  else if ( NumberOfDimensions == 3 )
    {
    const Tag seqDelItem(0xfffe,0xe0dd);
    Fragment frag;
    std::streamoff thestart = is.tellg();
    unsigned int numfrags = 0;
    std::vector< size_t > offsets;
    while( frag.ReadPreValue<SwapperNoOp>(is) && frag.GetTag() != seqDelItem )
      {
      //std::streamoff relstart = is.tellg();
      //gdcm_assert( relstart - thestart == 8 );
      std::streamoff off = frag.GetVL();
      offsets.push_back( (size_t)off );
      is.seekg( off, std::ios::cur );
      ++numfrags;
      }
    gdcm_assert( frag.GetTag() == seqDelItem && frag.GetVL() == 0 );
    gdcm_assert( numfrags == offsets.size() );
    if( numfrags != Dimensions[2] )
      {
      gdcmErrorMacro( "Not handled" );
      return false;
      }

    for( unsigned int z = zmin; z <= zmax; ++z )
      {
      size_t curoffset = std::accumulate( offsets.begin(), offsets.begin() + z, size_t(0) );
      is.seekg( thestart + curoffset + 8 * z, std::ios::beg );
      is.seekg( 8, std::ios::cur );

      const size_t buf_size = offsets[z];
      char *dummy_buffer = new char[ buf_size ];
      is.read( dummy_buffer, buf_size );

      std::vector <unsigned char> outv;
      bool b = DecodeByStreamsCommon(dummy_buffer, buf_size, outv);
      delete[] dummy_buffer;

      if( !b ) return false;

      unsigned char *raw = outv.data();
      const unsigned int rowsize = xmax - xmin + 1;
      const unsigned int colsize = ymax - ymin + 1;
      const unsigned int bytesPerPixel = pf.GetPixelSize();

      if( outv.size() != dimensions[0] * dimensions[1] * bytesPerPixel )
      {
         gdcmDebugMacro( "Inconsistent buffer size. Giving up" );
         return false;
      }

      const unsigned char *tmpBuffer1 = raw;
      for (unsigned int y = ymin; y <= ymax; ++y)
        {
        size_t theOffset = 0 + (0*dimensions[1]*dimensions[0] + y*dimensions[0] + xmin)*bytesPerPixel;
        tmpBuffer1 = raw + theOffset;
        memcpy(&(buffer[((z-zmin)*rowsize*colsize +
              (y-ymin)*rowsize)*bytesPerPixel]),
          tmpBuffer1, rowsize*bytesPerPixel);
        }
      }
    }
  return true;
}

ImageCodec * JPEGLSCodec::Clone() const
{
  JPEGLSCodec * copy = new JPEGLSCodec;
  return copy;
}

bool JPEGLSCodec::StartEncode( std::ostream & )
{
  return true;
}
bool JPEGLSCodec::IsRowEncoder()
{
  return false;
}

bool JPEGLSCodec::IsFrameEncoder()
{
  return true;
}

bool JPEGLSCodec::AppendRowEncode( std::ostream & , const char * , size_t )
{
  return false;
}

bool JPEGLSCodec::AppendFrameEncode( std::ostream & out, const char * data, size_t datalen )
{
  const unsigned int * dimensions = this->GetDimensions();
  const PixelFormat & pf = this->GetPixelFormat(); (void)pf;
  gdcm_assert( datalen == dimensions[0] * dimensions[1] * pf.GetPixelSize() );

  std::vector<unsigned char> rgbyteCompressed;
  rgbyteCompressed.resize(dimensions[0] * dimensions[1] * 4);

  size_t cbyteCompressed;
  const bool b = this->CodeFrameIntoBuffer((char*)rgbyteCompressed.data(), rgbyteCompressed.size(), cbyteCompressed, data, datalen );
  if( !b ) return false;

  out.write( (char*)rgbyteCompressed.data(), cbyteCompressed );

  return true;
}

bool JPEGLSCodec::StopEncode( std::ostream & )
{
  return true;
}


} // end namespace gdcm
