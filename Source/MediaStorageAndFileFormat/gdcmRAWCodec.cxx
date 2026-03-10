/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "gdcmRAWCodec.h"
#include "gdcmTransferSyntax.h"
#include "gdcmByteSwap.txx"
#include "gdcmDataElement.h"
#include "gdcmSequenceOfFragments.h"
#include "gdcmUnpacker12Bits.h"

#include <limits>
#include <sstream>

#include <cstring>

namespace gdcm
{

class RAWInternals
{
public:
};

RAWCodec::RAWCodec()
{
  Internals = new RAWInternals;
}

RAWCodec::~RAWCodec()
{
  delete Internals;
}

bool RAWCodec::CanCode(TransferSyntax const &ts) const
{
  return ts == TransferSyntax::ImplicitVRLittleEndian
   || ts == TransferSyntax::ExplicitVRLittleEndian
   || ts == TransferSyntax::ExplicitVRBigEndian
   || ts == TransferSyntax::ImplicitVRBigEndianPrivateGE
   || ts == TransferSyntax::DeflatedExplicitVRLittleEndian;
}

bool RAWCodec::CanDecode(TransferSyntax const &ts) const
{
  return ts == TransferSyntax::ImplicitVRLittleEndian
   || ts == TransferSyntax::ExplicitVRLittleEndian
   || ts == TransferSyntax::ExplicitVRBigEndian
   || ts == TransferSyntax::ImplicitVRBigEndianPrivateGE
   || ts == TransferSyntax::DeflatedExplicitVRLittleEndian;
}

bool RAWCodec::Code(DataElement const &in, DataElement &out)
{
  out = in;
  //gdcm_assert(0);
  return true;
}

bool RAWCodec::DecodeBytes(const char* inBytes, size_t inBufferLength,
                           char* outBytes, size_t inOutBufferLength)
{
  // First let's see if we can do a fast-path:
  if( !NeedByteSwap &&
    !RequestPaddedCompositePixelCode &&
     PI != PhotometricInterpretation::YBR_FULL_422 &&
    /*!PlanarConfiguration &&*/ !RequestPlanarConfiguration &&
    GetPixelFormat().GetBitsAllocated() != 12 &&
    !NeedOverlayCleanup )
    {
    gdcm_assert( !NeedOverlayCleanup );
    gdcm_assert( PI != PhotometricInterpretation::YBR_PARTIAL_422 );
    gdcm_assert( PI != PhotometricInterpretation::YBR_PARTIAL_420 );
    gdcm_assert( PI != PhotometricInterpretation::YBR_ICT );
    gdcm_assert( this->GetPixelFormat() != PixelFormat::UINT12 );
    gdcm_assert( this->GetPixelFormat() != PixelFormat::INT12 );
    // DermaColorLossLess.dcm
    //gdcm_assert(inBufferLength == inOutBufferLength || inBufferLength == inOutBufferLength + 1);
    // What if the user request a subportion of the image:
    // this happen in the case of MOSAIC image, where we are only interested in the non-zero
    // pixel of the tiled image.
    // removal of this assert also solve an issue with: SIEMENS_GBS_III-16-ACR_NEMA_1.acr
    // where we need to discard trailing pixel data bytes.
    if( inOutBufferLength <= inBufferLength )
      {
      memcpy(outBytes, inBytes, inOutBufferLength);
      }
    else
      {
      gdcmWarningMacro( "Requesting too much data. Truncating result" );
      memcpy(outBytes, inBytes, inBufferLength);
      }
    return true;
    }
  // else
  gdcm_assert( inBytes );
  gdcm_assert( outBytes );
  std::stringstream is;
  is.write(inBytes, inBufferLength);
  std::stringstream os;
  bool r = DecodeByStreams(is, os);
  gdcm_assert( r );
  if(!r) return false;

  std::string str = os.str();

  if( this->GetPixelFormat() == PixelFormat::UINT12 ||
      this->GetPixelFormat() == PixelFormat::INT12 )
    {
    this->GetPixelFormat().SetBitsAllocated( 16 );
    const size_t len = str.size() * 16 / 12;
    if(len == inOutBufferLength) {
      char * copy = new char[len];
      bool b = Unpacker12Bits::Unpack(copy, str.data(), str.size() );
      if (!b)
        {
        delete[] copy;
        return false;
        }
      gdcm_assert(inOutBufferLength == len);
      memcpy(outBytes, copy, len);

      delete[] copy;
      return r;
    }
  }

  // DermaColorLossLess.dcm
  //assert (check == inOutBufferLength || check == inOutBufferLength + 1);
  // problem with: SIEMENS_GBS_III-16-ACR_NEMA_1.acr
  size_t len = str.size();
  if( inOutBufferLength <= len )
    memcpy(outBytes, str.c_str(), inOutBufferLength);
  else
  {
    gdcmWarningMacro( "Requesting too much data. Truncating result" );
    memcpy(outBytes, str.c_str(), len);
  }

  return r;
}

bool RAWCodec::Decode(DataElement const &in, DataElement &out)
{
  // First let's see if we can do a fast-path:
  if( !NeedByteSwap &&
    !RequestPaddedCompositePixelCode &&
    PI == PhotometricInterpretation::MONOCHROME2 &&
    !PlanarConfiguration && !RequestPlanarConfiguration &&
    GetPixelFormat().GetBitsAllocated() != 12 &&
    !NeedOverlayCleanup )
    {
    gdcm_assert( this->GetPixelFormat() != PixelFormat::UINT12 );
    gdcm_assert( this->GetPixelFormat() != PixelFormat::INT12 );
    out = in;
    return true;
    }
  // else
  const ByteValue *bv = in.GetByteValue();
  gdcm_assert( bv );
  std::stringstream is;
  is.write(bv->GetPointer(), bv->GetLength());
  std::stringstream os;
  bool r = DecodeByStreams(is, os);
  if(!r) return false;
  gdcm_assert( r );

  std::string str = os.str();
  //std::string::size_type check = str.size();

  out = in;

  if( this->GetPixelFormat() == PixelFormat::UINT12 ||
    this->GetPixelFormat() == PixelFormat::INT12 )
    {
    size_t len = str.size() * 16 / 12;
    char * copy = new char[len];//why use an array, and not a vector?
    bool b = Unpacker12Bits::Unpack(copy, str.data(), str.size() );
    gdcm_assert(b);
    (void)b;
    VL::Type lenSize = (VL::Type)len;
    out.SetByteValue( copy, lenSize );
    delete[] copy;

    this->GetPixelFormat().SetBitsAllocated( 16 );
    }
  else
    {
      VL::Type strSize = (VL::Type) str.size();
    out.SetByteValue( str.data(), strSize);
    }

  return r;
}

bool RAWCodec::DecodeByStreams(std::istream &is, std::ostream &os)
{
  bool r = ImageCodec::DecodeByStreams(is, os);
  return r;
}

bool RAWCodec::GetHeaderInfo(std::istream &, TransferSyntax &ts)
{
  ts = TransferSyntax::ExplicitVRLittleEndian;
  if( NeedByteSwap )
    {
    ts = TransferSyntax::ImplicitVRBigEndianPrivateGE;
    }
  return true;
}

ImageCodec * RAWCodec::Clone() const
{
  return nullptr;
}

} // end namespace gdcm
