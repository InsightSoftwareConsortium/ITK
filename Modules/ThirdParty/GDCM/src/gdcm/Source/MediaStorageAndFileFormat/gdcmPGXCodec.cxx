/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "gdcmPGXCodec.h"
#include "gdcmTransferSyntax.h"
#include "gdcmSystem.h"
#include "gdcmDataElement.h"
#include "gdcmSequenceOfFragments.h"
#include "gdcmSwapper.h"
#include "gdcmFilenameGenerator.h"

namespace gdcm
{

PGXCodec::PGXCodec()
= default;

PGXCodec::~PGXCodec()
= default;

bool PGXCodec::CanDecode(TransferSyntax const &) const
{
  return false;
}

bool PGXCodec::CanCode(TransferSyntax const &) const
{
  return false;
}

bool PGXCodec::Write(const char *filename, const DataElement &out) const
{
  if( !filename ) return false;
  //const PhotometricInterpretation &pi = this->GetPhotometricInterpretation();
  std::vector<std::string> filenames;
  const PixelFormat& pf = GetPixelFormat();
  unsigned short nsamples = pf.GetSamplesPerPixel();
  FilenameGenerator fg;
  std::string base = filename;
  std::string::size_type dot_pos = base.size() - 4;
  std::string prefix = base.substr(0, dot_pos );
  fg.SetPrefix( prefix.c_str() );
  fg.SetPattern( "_%d.pgx" );
  size_t zdim = Dimensions[2];
  size_t num_images = zdim * nsamples;
  fg.SetNumberOfFilenames(num_images);
  if( !fg.Generate() ) return false;
  const ByteValue *bv = out.GetByteValue();
  if(!bv)
    {
    gdcmErrorMacro( "PGX Codec does not handle compress syntax."
      "You need to decompress first." );
    return false;
    }
#if 1
  gdcm::PhotometricInterpretation pi = this->GetPhotometricInterpretation();
  if( pi != gdcm::PhotometricInterpretation::MONOCHROME2 ) {
    gdcmErrorMacro( "Bogus PI" << pi );
    return false;
  }
#endif
  const unsigned int *dims = this->GetDimensions();
  uint8_t pixelSize = pf.GetPixelSize ();
  size_t image_size = dims[0] * dims[1] * pixelSize;
  const char *img_buffer = bv->GetPointer();

  for( unsigned int i = 0; i < num_images; ++i, img_buffer += image_size )
    {
    const char *targetname = fg.GetFilename( i );

    std::ofstream os( targetname, std::ios::binary );
    os << "PG ML ";
    os << (pf.GetPixelRepresentation() ? "-" : "+");
    os << " ";
    os << pf.GetBitsStored();
    os << " ";
    os << dims[0] << " " << dims[1] << "\n";
    os.write( img_buffer, image_size );
    os.close();
    }

  return true;
}

bool PGXCodec::Read(const char *filename, DataElement &out) const
{
  (void)filename;
  (void)out;
  gdcmErrorMacro("TODO" );
  return false;
}

bool PGXCodec::GetHeaderInfo(std::istream &is0, TransferSyntax &ts)
{
  std::string str;
  std::getline(is0, str);
  {
  std::istringstream is(str);
  std::string str1, str2, sign;
  int depth;
  is >> str1;
  if( str1 != "PG" ) {
    gdcmErrorMacro("PGX Wrong header: " << str1 );
    return false;
  }
  is >> str2;
  if( str2 != "ML" ) {
    gdcmErrorMacro("PGX Wrong header: " << str2 );
    return false;
  }
  is >> sign;
  if( sign != "+" && sign != "-" ) {
    gdcmErrorMacro("PGX Wrong header: " << sign);
    return false;
  }
  is >> depth;
  if( depth <= 0 ) {
    gdcmErrorMacro("PGX Wrong header: " << depth );
    return false;
  }
  PhotometricInterpretation pi;
    pi = PhotometricInterpretation::MONOCHROME2;
  unsigned int dims[3] = {};
  is >> dims[0]; is >> dims[1];
  PixelFormat pf = GetPixelFormat();
  unsigned int numbytes = ( depth + 7 ) / 8;
  pf.SetBitsAllocated( numbytes * 8);
  pf.SetBitsStored( depth );
  if( sign[0] == '-' ) pf.SetPixelRepresentation(1); 
    ts = TransferSyntax::ExplicitVRLittleEndian;

  SetPhotometricInterpretation( pi );
  SetPixelFormat( pf );
  SetDimensions( dims );
  }

  return true;
}

ImageCodec * PGXCodec::Clone() const
{
  return nullptr;
}

} // end namespace gdcm
