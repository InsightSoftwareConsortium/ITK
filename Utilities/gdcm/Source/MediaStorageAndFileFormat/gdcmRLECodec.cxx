/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library
  Module:  $URL$

  Copyright (c) 2006-2010 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "gdcmRLECodec.h"
#include "gdcmTransferSyntax.h"
#include "gdcmTrace.h"
#include "gdcmByteSwap.txx"
#include "gdcmDataElement.h"
#include "gdcmSequenceOfFragments.h"
#include "gdcmSmartPointer.h"
#include "gdcmSwapper.h"

#include <vector>

namespace gdcm
{

// TODO ideally this code should be in utilities for ease of reuse
class RLEHeader
{
public:
  uint32_t NumSegments;
  uint32_t Offset[15];

  void Print(std::ostream &os)
    {
    os << "NumSegments:" << NumSegments << "\n";
    for(int i=0; i<15; ++i)
      {
      os << i << ":" << Offset[i] << "\n";
      }
    }
};

class RLEFrame
{
public:
  void Read(std::istream &is)
    {
    // read Header (64 bytes)
    is.read((char*)(&Header), sizeof(uint32_t)*16);
    assert( sizeof(uint32_t)*16 == 64 );
    assert( sizeof(RLEHeader) == 64 );
    //ByteSwap<unsigned long>::SwapRangeFromSwapCodeIntoSystem(
    //  (unsigned long*)&Header, is.GetSwapCode(), 16);
    SwapperNoOp::SwapArray((uint32_t*)&Header,16);
    uint32_t numSegments = Header.NumSegments;
    if( numSegments >= 1 )
      {
      assert( Header.Offset[0] == 64 );
      }
    // We just check that we are indeed at the proper position start+64
    //char bytes[256*256];
    }
  void Print(std::ostream &os)
    {
    Header.Print(os);
    }
//private:
  RLEHeader Header;
  std::vector<char> Bytes;
};

class RLEInternals
{
public:
  RLEFrame Frame;
  std::vector<uint32_t> SegmentLength;
};

RLECodec::RLECodec()
{
  Internals = new RLEInternals;
  Length = 0;
  BufferLength = 0;
}

RLECodec::~RLECodec()
{
  delete Internals;
}

bool RLECodec::CanDecode(TransferSyntax const &ts) const
{
  return ts == TransferSyntax::RLELossless;
}

bool RLECodec::CanCode(TransferSyntax const &ts) const
{
  return ts == TransferSyntax::RLELossless;
}

/*
G.3 THE RLE ALGORITHM
The RLE algorithm described in this section is used to compress Byte Segments into RLE Segments.
There is a one-to-one correspondence between Byte Segments and RLE Segments. Each RLE segment
must be an even number of bytes or padded at its end with zero to make it even.
G.3.1 The RLE encoder
A sequence of identical bytes (Replicate Run) is encoded as a two-byte code:
< -count + 1 > <byte value>, where
count = the number of bytes in the run, and
2 <= count <= 128
and a non-repetitive sequence of bytes (Literal Run) is encoded as:
< count - 1 > <Iiteral sequence of bytes>, where
count = number of bytes in the sequence, and
1 <= count <= 128.
The value of -128 may not be used to prefix a byte value.
Note: It is common to encode a 2-byte repeat run as a Replicate Run except when preceded and followed by
a Literal Run, in which case it's best to merge the three runs into a Literal Run.
Three-byte repeats shall be encoded as Replicate Runs. Each row of the image shall be encoded
separately and not cross a row boundary.
*/
inline int count_identical_bytes(const char *start, unsigned int len)
{
  assert( len );
#if 0
  const char *p = start + 1;
  const unsigned int cmin = std::min(128u,len);
  const char *end = start + cmin;
  while( p < end && *p == *start )
    {
    ++p;
    }
  return p - start;
#else
  const char ref = start[0];
  int count = 1; // start at one
  const unsigned int cmin = std::min(128u,len);
  while( count < cmin && start[count] == ref )
    {
  //std::cerr << "count/len:" << count << "," << len << std::endl;
    ++count;
    }
  assert( /*2 <= count && */ count <= 128 ); // remove post condition as it will be our return error code
  assert( count >= 1 );
  return count;
#endif
}

inline int count_nonrepetitive_bytes(const char *start, unsigned int len)
{
/*
 * TODO:
 * I need a special handling when there is only a one repetition that break the Literal run...
Note: It is common to encode a 2-byte repeat run as a Replicate Run except when preceded and followed by
a Literal Run, in which case it's best to merge the three runs into a Literal Run.
*/
  assert( len );
#if 0
  const char *prev = start;
  const char *p = start + 1;
  const unsigned int cmin = std::min(128u,len);
  const char *end = start + cmin;
  while( p < end && *p != *prev )
    {
    ++prev;
    ++p;
    }
  return p - start;
#else
  int count = 1;
  const unsigned int cmin = std::min(128u,len);
#if 0
  // TODO: this version that handles the note still does not work...
  while( count < cmin )
    {
    if ( start[count] != start[count-1] )
      {
      // Special case:
      if( count + 1 < cmin && start[count] != start[count+1] )
        {
        continue;
        }
      break;
      }
    ++count;
    }
#else
#if 1
  // This version properly encode: 0 1 1 0 as: 3 0 1 1 0 ...
  for( count = 1; count < cmin; ++count )
    {
    if( start[count] == start[count-1] )
      {
      if( count + 1 < cmin && start[count] != start[count+1] )
        {
        continue;
        }
      --count;
      break;
      }
    }
#else
  // This version does not handle 0 1 1 0 as specified in the note in the DICOM standard
  while( count < cmin && start[count] != start[count-1] )
    {
    ++count;
    }
#endif
#endif
  assert( 1 <= count && count <= 128 );
  return count;
#endif
}

/* return output length */
ptrdiff_t rle_encode(char *output, unsigned int outputlength, const char *input, unsigned int inputlength)
{
  char *pout = output;
  const char *pin = input;
  unsigned int length = inputlength;
  while( pin != input + inputlength )
    {
    assert( length <= inputlength );
    assert( pin <= input + inputlength );
    int count = count_identical_bytes(pin, length);
    if( count > 1 ) /* or 2 ? */
      {
      // repeat case:
      //
      // Test first we are allowed to write two bytes:
      if( pout + 1 + 1 > output + outputlength ) return -1;
      *pout = -count + 1;
      assert( /**pout != -128 &&*/ 1 - *pout == count );
      assert( *pout <= -1 && *pout >= -127 );
      ++pout;
      *pout = *pin;
      ++pout;
      }
    else
      {
      // non repeat case:
      // ok need to compute non-repeat:
      count = count_nonrepetitive_bytes(pin, length);
      // first test we are allowed to write 1 + count bytes in the output buffer:
      if( pout + count + 1 > output + outputlength ) return -1;
      *pout = count - 1;
      assert( *pout != -128 && *pout+1 == count );
      assert( *pout >= 0 );
      ++pout;
      memcpy(pout, pin, count);
      pout += count;
      }
    // count byte where read, move pin to new position:
    pin += count;
    // compute remaining length:
    assert( count <= length );
    length -= count;
    }
  return pout - output;
}

template <typename T>
bool DoInvertPlanarConfiguration(T *output, const T *input, uint32_t inputlength)
{
  const T *r = input+0;
  const T *g = input+1;
  const T *b = input+2;
  uint32_t length = (inputlength / 3) * 3; // remove the 0 padding
  assert( length == inputlength || length == inputlength - 1 );
  assert( length % 3 == 0 );
  uint32_t plane_length = length / 3;
  T *pout = output;
  // copy red plane:
  while( pout != output + plane_length * 1 )
    {
    *pout++ = *r;
    r += 3;
    }
  assert( r == input + length );
  // copy green plane:
  assert( pout == output + plane_length );
  while( pout != output + plane_length * 2 )
    {
    *pout++ = *g;
    g += 3;
    }
  assert( g == input + length + 1);
  // copy blue plane:
  assert( pout == output + 2*plane_length );
  while( pout != output + plane_length * 3 )
    {
    *pout++ = *b;
    b += 3;
    }
  assert( b == input + length + 2);
  assert ( pout = output + length );
  return true;
}


bool RLECodec::Code(DataElement const &in, DataElement &out)
{
  const unsigned int *dims = this->GetDimensions();
  const unsigned int n = 256*256;
  char *outbuf;
  // At most we are encoding a single row at a time, so we would be very unlucky
  // if the row *after* compression would not fit in 256*256 bytes...
  char small_buffer[n];
  outbuf = small_buffer;

  // Create a Sequence Of Fragments:
  SmartPointer<SequenceOfFragments> sq = new SequenceOfFragments;
  const Tag itemStart(0xfffe, 0xe000);
  //sq->GetTable().SetTag( itemStart );
  // FIXME  ? Is this compulsary ?
  const char dummy[4] = {};
  //sq->GetTable().SetByteValue( dummy, sizeof(dummy) );

  const ByteValue *bv = in.GetByteValue();
  assert( bv );
  const char *input = bv->GetPointer();
  unsigned long bvl = bv->GetLength();
  unsigned long image_len = bvl / dims[2];

  // If 16bits, need to do the padded composite...
  char *buffer = 0;
  // if rgb (3 comp) need to the planar configuration
  char *bufferrgb = 0;
  if( GetPixelFormat().GetBitsAllocated() > 8 )
    {
    //RequestPaddedCompositePixelCode = true;
    buffer = new char [ image_len ];
    }

  if ( GetPhotometricInterpretation() == PhotometricInterpretation::RGB
    || GetPhotometricInterpretation() == PhotometricInterpretation::YBR_FULL
    || GetPhotometricInterpretation() == PhotometricInterpretation::YBR_RCT
    || GetPhotometricInterpretation() == PhotometricInterpretation::YBR_FULL_422 )
    {
    bufferrgb = new char [ image_len ];
    }

  int MaxNumSegments = 1;
  if( GetPixelFormat().GetBitsAllocated() == 8 )
    {
    MaxNumSegments *= 1;
    }
  else if( GetPixelFormat().GetBitsAllocated() == 16 )
    {
    MaxNumSegments *= 2;
    }
  else if( GetPixelFormat().GetBitsAllocated() == 32 )
    {
    MaxNumSegments *= 4;
    }
  else
    {
    return false;
    }

  if( GetPhotometricInterpretation() == PhotometricInterpretation::RGB
    || GetPhotometricInterpretation() == PhotometricInterpretation::YBR_FULL
    || GetPhotometricInterpretation() == PhotometricInterpretation::YBR_RCT
    || GetPhotometricInterpretation() == PhotometricInterpretation::YBR_FULL_422 )
    {
    MaxNumSegments *= 3;
    }

  assert( GetPixelFormat().GetBitsAllocated() == 8 || GetPixelFormat().GetBitsAllocated() == 16
    || GetPixelFormat().GetBitsAllocated() == 32 );
  if( GetPixelFormat().GetSamplesPerPixel() == 3 )
    {
    assert( MaxNumSegments % 3 == 0 );
    }

  RLEHeader header = { MaxNumSegments, { 64 } };
  // there cannot be any space in between the end of the RLE header and the start
  // of the first RLE segment
  //
  // Create a RLE Frame for each frame:
  for(unsigned int dim = 0; dim < dims[2]; ++dim)
    {
    // Within each frame, create the RLE Segments:
    // lets' try a simple scheme where each Segments is given an equal portion
    // of the input image.
    const char *ptr_img = input + dim * image_len;
    if( GetPlanarConfiguration() == 0 && GetPixelFormat().GetSamplesPerPixel() == 3 )
      {
      if( GetPixelFormat().GetBitsAllocated() == 8 )
        {
        DoInvertPlanarConfiguration<char>(bufferrgb, ptr_img, image_len / sizeof(char));
        }
      else /* ( GetPixelFormat().GetBitsAllocated() == 16 ) */
        {
        assert( GetPixelFormat().GetBitsAllocated() == 16 );
        // should not happen right ?
        DoInvertPlanarConfiguration<short>((short*)bufferrgb, (short*)ptr_img, image_len / sizeof(short));
        }
      ptr_img = bufferrgb;
      }
    if( GetPixelFormat().GetBitsAllocated() == 32 )
      {
      assert( !(image_len % 4) );
      //assert( image_len % 3 == 0 );
      unsigned int div = GetPixelFormat().GetSamplesPerPixel();
      for(unsigned int j = 0; j < div; ++j)
        {
        unsigned long iimage_len = image_len / div;
        char *ibuffer = buffer + j * iimage_len;
        const char *iptr_img = ptr_img + j * iimage_len;
        assert( iimage_len % 4 == 0 );
        for(unsigned long i = 0; i < iimage_len/4; ++i)
          {
#ifdef GDCM_WORDS_BIGENDIAN
          ibuffer[i] = iptr_img[4*i+0];
#else
          ibuffer[i] = iptr_img[4*i+3];
#endif
          }
        for(unsigned long i = 0; i < iimage_len/4; ++i)
          {
#ifdef GDCM_WORDS_BIGENDIAN
          ibuffer[i+iimage_len/4] = iptr_img[4*i+1];
#else
          ibuffer[i+iimage_len/4] = iptr_img[4*i+2];
#endif
          }
        for(unsigned long i = 0; i < iimage_len/4; ++i)
          {
#ifdef GDCM_WORDS_BIGENDIAN
          ibuffer[i+2*iimage_len/4] = iptr_img[4*i+2];
#else
          ibuffer[i+2*iimage_len/4] = iptr_img[4*i+1];
#endif
          }
        for(unsigned long i = 0; i < iimage_len/4; ++i)
          {
#ifdef GDCM_WORDS_BIGENDIAN
          ibuffer[i+3*iimage_len/4] = iptr_img[4*i+3];
#else
          ibuffer[i+3*iimage_len/4] = iptr_img[4*i+0];
#endif
          }
        }
      ptr_img = buffer;
      }
    else if( GetPixelFormat().GetBitsAllocated() == 16 )
      {
      assert( !(image_len % 2) );
      //assert( image_len % 3 == 0 );
      unsigned int div = GetPixelFormat().GetSamplesPerPixel();
      for(unsigned int j = 0; j < div; ++j)
        {
        unsigned long iimage_len = image_len / div;
        char *ibuffer = buffer + j * iimage_len;
        const char *iptr_img = ptr_img + j * iimage_len;
        assert( iimage_len % 2 == 0 );
        for(unsigned long i = 0; i < iimage_len/2; ++i)
          {
#ifdef GDCM_WORDS_BIGENDIAN
          ibuffer[i] = iptr_img[2*i];
#else
          ibuffer[i] = iptr_img[2*i+1];
#endif
          }
        for(unsigned long i = 0; i < iimage_len/2; ++i)
          {
#ifdef GDCM_WORDS_BIGENDIAN
          ibuffer[i+iimage_len/2] = iptr_img[2*i+1];
#else
          ibuffer[i+iimage_len/2] = iptr_img[2*i];
#endif
          }
        }
      ptr_img = buffer;
      }
    assert( image_len % MaxNumSegments == 0 );
    const int input_seg_length = image_len / MaxNumSegments;
    std::string datastr;
    for(int seg = 0; seg < MaxNumSegments; ++seg )
      {
      int partition = input_seg_length;
      const char *ptr = ptr_img + seg * input_seg_length;
      assert( ptr < ptr_img + image_len );
      if( seg == MaxNumSegments - 1 )
        {
        partition += image_len % MaxNumSegments;
        assert( (MaxNumSegments-1) * input_seg_length + partition == image_len );
        }
      assert( partition == input_seg_length );

      std::stringstream data;
      assert( partition % dims[1] == 0 );
      size_t length = 0;
      // Do not cross row boundary:
      for(unsigned int y = 0; y < dims[1]; ++y)
        {
        ptrdiff_t llength = rle_encode(outbuf, n, ptr + y*dims[0], partition / dims[1] /*image_len*/);
        if( llength < 0 )
          {
          std::cerr << "RLE compressor error" << std::endl;
          return false;
          }
        assert( llength );
        data.write((char*)outbuf, llength);
        length += llength;
        }
      // update header
      header.Offset[1+seg] = header.Offset[seg] + length;

      assert( data.str().size() == length );
      datastr += data.str();
      }
    header.Offset[MaxNumSegments] = 0;
    std::stringstream os;
    //header.Print( std::cout );
    os.write((char*)&header,sizeof(header));
    std::string str = os.str() + datastr;
    assert( str.size() );
    Fragment frag;
    //frag.SetTag( itemStart );
    frag.SetByteValue( &str[0], str.size() );
    sq->AddFragment( frag );
    }

  out.SetValue( *sq );

  if( buffer /*GetPixelFormat().GetBitsAllocated() > 8*/ )
    {
    //RequestPaddedCompositePixelCode = true;
    delete[] buffer;
    }
  if ( bufferrgb /*GetPhotometricInterpretation() == PhotometricInterpretation::RGB*/ )
    {
    delete[] bufferrgb;
    }

  return true;
}

// G.3.2 The RLE decoder
// Pseudo code for the RLE decoder is shown below:
// Loop until the number of output bytes equals the uncompressed segment size
// Read the next source byte into n
// If n> =0 and n <= 127 then
// output the next n+1 bytes literally
// Elseif n <= - 1 and n >= -127 then
// output the next byte -n+1 times
// Elseif n = - 128 then
// output nothing
// Endif
// Endloop

bool RLECodec::Decode(DataElement const &in, DataElement &out)
{
  if( NumberOfDimensions == 2 )
    {
    out = in;
    const SequenceOfFragments *sf = in.GetSequenceOfFragments();
    if( !sf ) return false;
    unsigned long len = GetBufferLength();
    //char *buffer = new char[len];
    std::stringstream is;
    sf->WriteBuffer( is );
    SetLength( len );
      std::stringstream os;
      bool r = Decode(is, os);
      assert( r == true );
    std::string str = os.str();
    std::string::size_type check = str.size();
    assert( check == len );
    out.SetByteValue( &str[0], check );
    return true;
    }
  else if ( NumberOfDimensions == 3 )
    {
    out = in;
    const SequenceOfFragments *sf = in.GetSequenceOfFragments();
    if( !sf ) return false;
    unsigned long len = GetBufferLength();
    char *buffer = new char[len];
    unsigned long pos = 0;
    // Each RLE Frame store a 2D frame. len is the 3d length
    unsigned long llen = len / sf->GetNumberOfFragments();
    // assert( GetNumberOfDimensions() == 2
    //      || GetDimension(2) == sf->GetNumberOfFragments() );
    for(unsigned int i = 0; i < sf->GetNumberOfFragments(); ++i)
      {
      std::stringstream is;
      const Fragment &frag = sf->GetFragment(i);
      const ByteValue &bv = dynamic_cast<const ByteValue&>(frag.GetValue());
      char *mybuffer = new char[bv.GetLength()];
      bv.GetBuffer(mybuffer, bv.GetLength());
      is.write(mybuffer, bv.GetLength());
      delete[] mybuffer;
      std::stringstream os;
      SetLength( llen );
      bool r = Decode(is, os);
      assert( r == true );
      std::streampos p = is.tellg();
      // http://groups.google.com/group/microsoft.public.vc.stl/browse_thread/thread/96740930d0e4e6b8
      if( !!is )
        {
        // Indeed the length of the RLE stream has been padded with a \0
        // which is discarded
        uint32_t check = bv.GetLength() - p;
        // check == 2 for gdcmDataExtra/gdcmSampleData/US_DataSet/GE_US/2929J686-breaker
        assert( check == 0 || check == 1 || check == 2 );
        if( check ) gdcmWarningMacro( "tiny offset detected in between RLE segments" );
        }
      else
        {
        // ALOKA_SSD-8-MONO2-RLE-SQ.dcm
        gdcmWarningMacro( "Bad RLE stream" );
        }
      std::string::size_type check = os.str().size();
      // If the following assert fail expect big troubles:
      assert( check == llen );
      memcpy(buffer+pos, os.str().c_str(), check);
      pos += check;
      }
    assert( pos == len );
    out.SetByteValue( buffer, len );
    delete[] buffer;
    return true;
    }
  return false;
}

bool RLECodec::Decode(std::istream &is, std::ostream &os)
{
  // FIXME: Do some stupid work:
  char dummy_buffer[256];
  std::stringstream tmpos;
  // DEBUG
  is.seekg( 0, std::ios::end);
  std::streampos buf_size = is.tellg();
  is.seekg(0, std::ios::beg);
  // END DEBUG

  RLEFrame &frame = Internals->Frame;
  frame.Read(is);
  //frame.Print(std::cout);
  unsigned long numSegments = frame.Header.NumSegments;

  unsigned long numberOfReadBytes = 0;

  unsigned long length = Length;
  // Special case:
  assert( GetPixelFormat().GetBitsAllocated() == 32 ||
          GetPixelFormat().GetBitsAllocated() == 16 ||
          GetPixelFormat().GetBitsAllocated() == 8 );
  if( GetPixelFormat().GetBitsAllocated() > 8 )
    {
    RequestPaddedCompositePixelCode = true;
    }

  assert( GetPixelFormat().GetSamplesPerPixel() == 3 || GetPixelFormat().GetSamplesPerPixel() == 1 );
  // A footnote:
  // RLE *by definition* with more than one component will have applied the Planar Configuration
  // because it simply does not make sense to do it otherwise. So implicitely
  // RLE is indeed PlanarConfiguration == 1. However when the image says: "hey I am PlanarConfiguration = 0
  // AND RLE", then apply the PlanarConfiguration internally so that people don't get lost
  // Because GDCM internally set PlanarConfiguration == 0 by default, even if the Attribute is not
  // sent, it will still default to 0 and we will be consistant with ourselves...
  if( GetPixelFormat().GetSamplesPerPixel() == 3 && GetPlanarConfiguration() == 0 )
    {
    RequestPlanarConfiguration = true;
    }
  length /= numSegments;
  for(unsigned long i = 0; i<numSegments; ++i)
    {
    numberOfReadBytes = 0;
    std::streampos pos = is.tellg();
    if ( frame.Header.Offset[i] - pos != 0 )
      {
      // ACUSON-24-YBR_FULL-RLE.dcm
      // D_CLUNIE_CT1_RLE.dcm
      // This should be at most the \0 padding
      //gdcmWarningMacro( "RLE Header says: " << frame.Header.Offset[i] <<
      //   " when it should says: " << pos << std::endl );
      uint32_t check = frame.Header.Offset[i] - pos;
      // check == 2 for gdcmDataExtra/gdcmSampleData/US_DataSet/GE_US/2929J686-breaker
      assert( check == 1 || check == 2);
      is.seekg( frame.Header.Offset[i], std::ios::beg );
      }

    unsigned long numOutBytes = 0;
    signed char byte;
    //std::cerr << "Length: " << Length << "\n";
    //assert( (uint32_t)is.Tellg() == frame.Header.Offset[i] );

    // FIXME: ALOKA_SSD-8-MONO2-RLE-SQ.dcm
    // I think the RLE decoder is off by one, we are reading in 128001 byte, while only 128000
    // are present
    while( numOutBytes < length )
      {
      is.read((char*)&byte, 1);
      assert( is.good() );
      numberOfReadBytes++;
      if( byte >= 0 /*&& byte <= 127*/ ) /* 2nd is always true */
        {
        is.read( dummy_buffer, byte+1 );
        numberOfReadBytes += byte+1;
        numOutBytes += byte+ 1;
        tmpos.write( dummy_buffer, byte+1 );
        }
      else if( byte <= -1 && byte >= -127 )
        {
        char nextByte;
        is.read( &nextByte, 1);
        numberOfReadBytes += 1;
        memset(dummy_buffer, nextByte, -byte + 1);
        //for( int c = 0; c < -byte + 1; ++c )
        //  {
        //  dummy_buffer[c] = nextByte;
        //  }
        numOutBytes += -byte + 1;
        tmpos.write( dummy_buffer, -byte+1 );
        }
      else /* byte == -128 */
        {
        assert( byte == -128 );
        }
        assert( is.eof()
        || numberOfReadBytes + frame.Header.Offset[i] - is.tellg() == 0);
      //std::cerr << "numOutBytes: " << numOutBytes << " / " << length << "\n";
      //std::cerr << "numberOfReadBytes: " << numberOfReadBytes << "\n";
      }
    assert( numOutBytes == length );
    }

  return ImageCodec::Decode(tmpos,os);
}

bool RLECodec::GetHeaderInfo(std::istream &is, TransferSyntax &ts)
{
  (void)is;
  ts = TransferSyntax::RLELossless;
  return true;
}

} // end namespace gdcm
