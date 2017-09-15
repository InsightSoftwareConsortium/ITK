/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "gdcmSplitMosaicFilter.h"
#include "gdcmCSAHeader.h"
#include "gdcmAttribute.h"
#include "gdcmImageHelper.h"
#include "gdcmDirectionCosines.h"
#include "gdcmAnonymizer.h"

#include <math.h>

namespace gdcm
{
SplitMosaicFilter::SplitMosaicFilter():F(new File),I(new Image) {}
SplitMosaicFilter::~SplitMosaicFilter() {}

namespace details {
/*
 *  gdcmDataExtra/gdcmSampleData/images_of_interest/MR-sonata-3D-as-Tile.dcm
 */
static bool reorganize_mosaic(const unsigned short *input, const unsigned int *inputdims,
  unsigned int square, const unsigned int *outputdims, unsigned short *output )
{
  for(unsigned int x = 0; x < outputdims[0]; ++x)
    {
    for(unsigned int y = 0; y < outputdims[1]; ++y)
      {
      for(unsigned int z = 0; z < outputdims[2]; ++z)
        {
        const size_t outputidx = x + y*outputdims[0] + z*outputdims[0]*outputdims[1];
        const size_t inputidx = (x + (z%square)*outputdims[0]) +
          (y + (z/square)*outputdims[1])*inputdims[0];
        output[ outputidx ] = input[ inputidx ];
        }
      }
    }
  return true;
}

#ifdef SNVINVERT
static bool reorganize_mosaic_invert(const unsigned short *input, const unsigned int *inputdims,
  unsigned int square, const unsigned int *outputdims, unsigned short *output )
{
  for(unsigned int x = 0; x < outputdims[0]; ++x)
    {
    for(unsigned int y = 0; y < outputdims[1]; ++y)
      {
      for(unsigned int z = 0; z < outputdims[2]; ++z)
        {
        const size_t outputidx = x + y*outputdims[0] + (outputdims[2]-1-z)*outputdims[0]*outputdims[1];
        const size_t inputidx = (x + (z%square)*outputdims[0]) +
          (y + (z/square)*outputdims[1])*inputdims[0];
        output[ outputidx ] = input[ inputidx ];
        }
      }
    }
  return true;
}
#endif

}

void SplitMosaicFilter::SetImage(const Image& image)
{
  I = image;
}

bool SplitMosaicFilter::ComputeMOSAICDimensions( unsigned int dims[3] )
{
  CSAHeader csa;
  DataSet& ds = GetFile().GetDataSet();

  const PrivateTag &t1 = csa.GetCSAImageHeaderInfoTag();
  int numberOfImagesInMosaic = 0;
  if( csa.LoadFromDataElement( ds.GetDataElement( t1 ) ) )
  {
    if( csa.FindCSAElementByName( "NumberOfImagesInMosaic" ) )
    {
      const CSAElement &csael4 = csa.GetCSAElementByName( "NumberOfImagesInMosaic" );
      if( !csael4.IsEmpty() )
      {
        Element<VR::IS, VM::VM1> el4 = {{ 0 }};
        el4.Set( csael4.GetValue() );
        numberOfImagesInMosaic = el4.GetValue();
      }
    }
  }
  else
  {
    // Some weird anonymizer remove the private creator but leave the actual element.
    // oh well, let try harder:
    // (0019,100a) US 72   # 2,1 NumberOfImagesInMosaic
    PrivateTag t2 (0x0019,0x0a, "SIEMENS MR HEADER");
    if( ds.FindDataElement( t2 ) )
    {
      const DataElement &de = ds.GetDataElement( t2 );
      const ByteValue * bv = de.GetByteValue();
      if( bv )
      {
        Element<VR::US, VM::VM1> el1 = {{0}};
        std::istringstream is;
        is.str( std::string( bv->GetPointer(), bv->GetLength() ) );
        el1.Read( is );
        numberOfImagesInMosaic = el1.GetValue();
      }
    }
  }
  if( !numberOfImagesInMosaic )
  {
    gdcmErrorMacro( "Could not find NumberOfImagesInMosaic" );
    return false;
  }

  std::vector<unsigned int> colrow =
    ImageHelper::GetDimensionsValue( GetFile() );
  dims[0] = colrow[0];
  dims[1] = colrow[1];

  const unsigned int div = (unsigned int )ceil(sqrt( (double)numberOfImagesInMosaic ) );
  dims[0] /= div;
  dims[1] /= div;
  dims[2] = numberOfImagesInMosaic;
  return true;
}

bool SplitMosaicFilter::ComputeMOSAICSliceNormal( double slicenormalvector[3], bool & inverted )
{
  CSAHeader csa;
  DataSet& ds = GetFile().GetDataSet();

  double normal[3];
  bool snvfound = false;
  const PrivateTag &t1 = csa.GetCSAImageHeaderInfoTag();
  static const char snvstr[] = "SliceNormalVector";
  if( csa.LoadFromDataElement( ds.GetDataElement( t1 ) ) )
  {
    if( csa.FindCSAElementByName( snvstr ) )
    {
      const CSAElement &snv_csa = csa.GetCSAElementByName( snvstr );
      if( !snv_csa.IsEmpty() )
      {
        const ByteValue * bv = snv_csa.GetByteValue();
        const std::string str(bv->GetPointer(), bv->GetLength());
        std::istringstream is;
        is.str( str );
        char sep;
        double *snv = normal;
        if( is >> snv[0] >> sep >> snv[1] >> sep >> snv[2] )
        {
          snvfound = true;
        }
      }
    }
  }
  if( snvfound )
  {
    Attribute<0x20,0x37> iop;
    iop.SetFromDataSet( ds );
    DirectionCosines dc( iop.GetValues() );
    double z[3];
    dc.Cross (z);
    const double snv_dot = dc.Dot( normal, z );
    if( (1. - snv_dot) < 1e-6 )
    {
      gdcmDebugMacro("Same direction");
      inverted = false;
    }
    else if( (-1. - snv_dot) < 1e-6 )
    {
      gdcmWarningMacro("SliceNormalVector is opposite direction");
      inverted = true;
    }
    else
    {
      gdcmErrorMacro( "Unexpected normal for SliceNormalVector, dot is: " << snv_dot );
      return false;
    }
  }
 
  for( int i = 0; i < 3; ++i)
    slicenormalvector[i] = normal[i];

  return snvfound;
}

bool SplitMosaicFilter::ComputeMOSAICSlicePosition( double pos[3], bool )
{
  CSAHeader csa;
  DataSet& ds = GetFile().GetDataSet();

  MrProtocol mrprot;
  if( !csa.GetMrProtocol(ds, mrprot) ) return false;

  MrProtocol::SliceArray sa;
  bool b = mrprot.GetSliceArray(sa);
  if( !b ) return false;

  size_t size = sa.Slices.size();
  if( !size ) return false;
#if 0
  {
    double z[3];
    for( int i = 0; i < size; ++i )
    {
      MrProtocol::Slice & slice = sa.Slices[i];
      MrProtocol::Vector3 & p = slice.Position;
      z[0] = p.dSag;
      z[1] = p.dCor;
      z[2] = p.dTra;
      const double snv_dot = DirectionCosines::Dot( slicenormalvector, z );
      if( (1. - snv_dot) < 1e-6 )
      {
        gdcmErrorMacro("Invalid direction found");
        return false;
      }
    }
  }
#endif

  size_t index = 0;
  MrProtocol::Slice & slice = sa.Slices[index];
  MrProtocol::Vector3 & p = slice.Position;
  pos[0] = p.dSag;
  pos[1] = p.dCor;
  pos[2] = p.dTra;

  return true;
}

bool SplitMosaicFilter::Split()
{
  bool success = true;
  DataSet& ds = GetFile().GetDataSet();

  unsigned int dims[3] = {0,0,0};
  if( ! ComputeMOSAICDimensions( dims ) )
    {
    return false;
    }
  const unsigned int div = (unsigned int )ceil(sqrt( (double)dims[2]) );
  bool inverted;
  double normal[3];
  if( !ComputeMOSAICSliceNormal( normal, inverted ) )
  {
    return false;
  }
  double origin[3];
  if( !ComputeMOSAICSlicePosition( origin, inverted ) )
  {
    return false;
  }

  const Image &inputimage = GetImage();
  if( inputimage.GetPixelFormat() != PixelFormat::UINT16 )
    {
    gdcmErrorMacro( "Expecting UINT16 PixelFormat" );
    return false;
    }
  unsigned long l = inputimage.GetBufferLength();
  std::vector<char> buf;
  buf.resize(l);
  inputimage.GetBuffer( &buf[0] );
  DataElement pixeldata( Tag(0x7fe0,0x0010) );

  std::vector<char> outbuf;
  outbuf.resize(l);

  bool b;
#ifdef SNVINVERT
  if( inverted )
  {
    b = details::reorganize_mosaic_invert(
        (unsigned short*)&buf[0], inputimage.GetDimensions(), div, dims,
        (unsigned short*)&outbuf[0] );
  }
  else
#endif
  {
    b = details::reorganize_mosaic(
        (unsigned short*)&buf[0], inputimage.GetDimensions(), div, dims,
        (unsigned short*)&outbuf[0] );
  }
  if( !b ) return false;

  VL::Type outbufSize = (VL::Type)outbuf.size();
  pixeldata.SetByteValue( &outbuf[0], outbufSize );

  Image &image = GetImage();
  const TransferSyntax &ts = image.GetTransferSyntax();
  if( ts.IsExplicit() )
     {
     image.SetTransferSyntax( TransferSyntax::ExplicitVRLittleEndian );
     }
   else
     {
     image.SetTransferSyntax( TransferSyntax::ImplicitVRLittleEndian );
     }
  image.SetNumberOfDimensions( 3 );
  image.SetDimension(0, dims[0] );
  image.SetDimension(1, dims[1] );
  image.SetDimension(2, dims[2] );

  // Fix origin (direction is ok since we reorganize the tiles):
  image.SetOrigin( origin );

  PhotometricInterpretation pi;
  pi = PhotometricInterpretation::MONOCHROME2;

  image.SetDataElement( pixeldata );

  // Second part need to fix the Media Storage, now that this is not a single slice anymore
  MediaStorage ms = MediaStorage::SecondaryCaptureImageStorage;
  ms.SetFromFile( GetFile() );

  if( ms == MediaStorage::MRImageStorage )
    {
    // Ok make it a MediaStorage::EnhancedMRImageStorage
//    ms = MediaStorage::EnhancedMRImageStorage;
//
//    // Remove old MRImageStorage attribute then:
//    ds.Remove( Tag(0x0020,0x0032) ); // Image Position (Patient)
//    ds.Remove( Tag(0x0020,0x0037) ); // Image Orientation (Patient)
//    ds.Remove( Tag(0x0028,0x1052) ); // Rescale Intercept
//    ds.Remove( Tag(0x0028,0x1053) ); // Rescale Slope
//    ds.Remove( Tag(0x0028,0x1054) ); // Rescale Type
    }
  else
    {
    gdcmDebugMacro( "Expecting MRImageStorage" );
    return false;
    }
  DataElement de( Tag(0x0008, 0x0016) );
  const char* msstr = MediaStorage::GetMSString(ms);
  VL::Type strlenMsstr = (VL::Type)strlen(msstr);
  de.SetByteValue( msstr, strlenMsstr );
  de.SetVR( Attribute<0x0008, 0x0016>::GetVR() );
  ds.Replace( de );

  return success;
}


} // end namespace gdcm
