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
#include "gdcmSplitMosaicFilter.h"
#include "gdcmCSAHeader.h"
#include "gdcmAttribute.h"

#include <math.h>

namespace gdcm
{
SplitMosaicFilter::SplitMosaicFilter():F(new File),I(new Image) {}
SplitMosaicFilter::~SplitMosaicFilter() {}

/*
 *  gdcmDataExtra/gdcmSampleData/images_of_interest/MR-sonata-3D-as-Tile.dcm
 */
bool reorganize_mosaic(const unsigned short *input, const unsigned int *inputdims, unsigned int square,
  const unsigned int *outputdims, unsigned short *output )
{
  for(unsigned x = 0; x < outputdims[0]; ++x)
    {
    for(unsigned y = 0; y < outputdims[1]; ++y)
      {
      for(unsigned z = 0; z < outputdims[2]; ++z)
        {
        output[ x + y*outputdims[0] + z*outputdims[0]*outputdims[1] ] =
          input[ (x + z * outputdims[0]) + (y + (z/square)*outputdims[0])*inputdims[0] ];
        }
      }
    }
  return true;
}


void SplitMosaicFilter::SetImage(const Image& image)
{
  I = image;
}

bool SplitMosaicFilter::Split()
{
  bool success = true;

  gdcm::CSAHeader csa;
  gdcm::DataSet& ds = GetFile().GetDataSet();

  const gdcm::PrivateTag &t1 = csa.GetCSAImageHeaderInfoTag();
  //std::cout << t1 << std::endl;
  //const gdcm::PrivateTag &t2 = csa.GetCSASeriesHeaderInfoTag();

  if( ds.FindDataElement( t1 ) )
    {
    csa.LoadFromDataElement( ds.GetDataElement( t1 ) );
    //csa.Print( std::cout );
    }

  //int dims[2] = { 448, 448 };
  //dims[0] /= 7;
  //dims[1] /= 7;
    unsigned int dims[3] = {};
  //double spacing[2] = {1,1};
    {
    const gdcm::DataElement& de = ds.GetDataElement( gdcm::Tag(0x0028, 0x0011) );
    gdcm::Attribute<0x0028,0x0011> at;
    at.SetFromDataElement( de );
    //pixeldata.SetDimension(0, at.GetValue() );
    dims[0] = at.GetValue();
    }

    {
    const gdcm::DataElement& de = ds.GetDataElement( gdcm::Tag(0x0028, 0x0010) );
    gdcm::Attribute<0x0028,0x0010> at;
    at.SetFromDataElement( de );
    //pixeldata.SetDimension(1, at.GetValue() );
    dims[1] = at.GetValue();
    }


  // SliceThickness ??
  const gdcm::CSAElement &csael4 = csa.GetCSAElementByName( "NumberOfImagesInMosaic" );
  //std::cout << csael4 << std::endl;
  gdcm::Element<gdcm::VR::IS, gdcm::VM::VM1> el4;
  el4.Set( csael4.GetValue() );
  int numberOfImagesInMosaic = el4.GetValue();

  unsigned int div = (unsigned int )ceil(sqrt( (double)numberOfImagesInMosaic ) );
  dims[0] /= div;
  dims[1] /= div;
  dims[2] = numberOfImagesInMosaic;

  //std::cout << "NumberOfImagesInMosaic:" << numberOfImagesInMosaic << std::endl;

  const gdcm::Image &inputimage = GetImage();
  //const double *spacing = inputimage.GetSpacing();
  unsigned long l = inputimage.GetBufferLength();
  std::vector<char> buf;
  buf.resize(l);
  inputimage.GetBuffer( &buf[0] );
  gdcm::DataElement pixeldata( gdcm::Tag(0x7fe1,0x1010) );

  std::vector<char> outbuf;
  outbuf.resize(l);
  //outbuf = buf;

  bool b = reorganize_mosaic((unsigned short*)&buf[0], inputimage.GetDimensions(), div, dims, (unsigned short*)&outbuf[0] );
  (void)b;

  pixeldata.SetByteValue( &outbuf[0], outbuf.size() );
  //const gdcm::DataElement & pixeldata = ds.GetDataElement( gdcm::Tag(0x7fe1,0x1010) );
  //const gdcm::DataElement & pixeldata = ds.GetDataElement( gdcm::Tag(0x7fe0,0x0010) );
  //const gdcm::VL &l = pixeldata.GetVL();
  const int p =  l / (dims[0] * dims[1]);
  (void)p;
  //std::cout << "VL:" << l << std::endl;
  //std::cout << "pixel:" << p << std::endl;

  gdcm::Image &image = GetImage();
  //image.SetNumberOfDimensions( 2 ); // good default
  //image.SetSpacing(0, spacing[0] );
  //image.SetSpacing(1, spacing[1] );
  //gdcm::PixelFormat pixeltype = inputimage.GetPixelFormat(); //gdcm::PixelFormat::INT16; // bytepix = spm_type('int16','bits')/8;

  image.SetNumberOfDimensions( 3 );
  //image.SetDimension(2, p / pixeltype.GetPixelSize() );
  image.SetDimension(0, dims[0] );
  image.SetDimension(1, dims[1] );
  image.SetDimension(2, numberOfImagesInMosaic );

  gdcm::PhotometricInterpretation pi;
  pi = gdcm::PhotometricInterpretation::MONOCHROME2;
  //pixeltype.SetSamplesPerPixel(  );
  //image.SetPhotometricInterpretation( pi );
  //image.SetPixelFormat( pixeltype );
  //image.SetIntercept( inputimage.GetIntercept() );
  //image.SetSlope( inputimage.GetSlope() );

  image.SetDataElement( pixeldata );


  // Second part need to fix the Media Storage, now that this is not a single slice anymore
  gdcm::MediaStorage ms = gdcm::MediaStorage::SecondaryCaptureImageStorage;
  ms.SetFromFile( GetFile() );

  if( ms == gdcm::MediaStorage::MRImageStorage )
    {
    // Ok make it a MediaStorage::EnhancedMRImageStorage
//    ms = gdcm::MediaStorage::EnhancedMRImageStorage;
//
//    // Remove old MRImageStorage attribute then:
//    ds.Remove( gdcm::Tag(0x0020,0x0032) ); // Image Position (Patient)
//    ds.Remove( gdcm::Tag(0x0020,0x0037) ); // Image Orientation (Patient)
//    ds.Remove( gdcm::Tag(0x0028,0x1052) ); // Rescale Intercept
//    ds.Remove( gdcm::Tag(0x0028,0x1053) ); // Rescale Slope
//    ds.Remove( gdcm::Tag(0x0028,0x1054) ); // Rescale Type
    }
  else
    {
    return false;
    }
  gdcm::DataElement de( gdcm::Tag(0x0008, 0x0016) );
  const char* msstr = gdcm::MediaStorage::GetMSString(ms);
  de.SetByteValue( msstr, strlen(msstr) );
  de.SetVR( gdcm::Attribute<0x0008, 0x0016>::GetVR() );
  ds.Replace( de );

  return success;
}


} // end namespace gdcm
