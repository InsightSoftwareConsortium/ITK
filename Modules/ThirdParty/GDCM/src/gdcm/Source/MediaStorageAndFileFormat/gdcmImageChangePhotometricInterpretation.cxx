/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "gdcmImageChangePhotometricInterpretation.h"
#include "gdcmSequenceOfFragments.h"
#include "gdcmSequenceOfItems.h"
#include "gdcmFragment.h"
#include "gdcmRAWCodec.h"

namespace gdcm
{

/*
 * http://groups.google.com/group/comp.protocols.dicom/browse_thread/thread/10f91b14e3013a11
 * http://groups.google.com/group/comp.protocols.dicom/browse_thread/thread/1190189387c1702c
 * http://groups.google.com/group/comp.protocols.dicom/browse_thread/thread/a9e118fbbf6dcc9f
 * http://forum.dcmtk.org/viewtopic.php?p=5441&sid=61ad1304edb31203c4136890ab651405

YBR_FULL as Photometric Interpretation is really the right thing to do. The
problem is that the JPEG bitstream as such does not contain any indication of
the color model - it just specifies that there are three samples per pixel. In
theory it is well possible to apply baseline JPEG compression to RGB pixel
data, although this is an unusual approach since YCbCr provides for better
compression ratio at given image quality. A JFIF header would contain that
information, but the JFIF header is neither required nor recommended in the
DICOM JPEG bitstream. In the absence of that information, and with a JPEG
compressed DICOM file where Photometric Interpretation is "RGB", the parser
needs to decide whether the encoder did something unusual but legal and
decompress the JPEG bitstream as RGB, or whether the encoder just failed to
correctly encode the color model of the JPEG bitstream (which in my experience
is in most cases the correct assumption) and ignore Photometric Interpretation
(and thus incorrectly decode unusual but legal images).
*/
bool ImageChangePhotometricInterpretation::ChangeMonochrome()
{
  // Ok let's give up on this one for now.
  // We would need to take care of Pixel Padding Value to actually be able to
  // invert the image without this information we potentially will be making
  // mistake. just like Largest Image Pixel Value and other would be wrong
  const Bitmap &image = *Input;
  PhotometricInterpretation pi = image.GetPhotometricInterpretation();
  if( pi != PhotometricInterpretation::MONOCHROME1 && pi != PhotometricInterpretation::MONOCHROME2 ) return false;
  if( pi == PI )
    {
    return true;
    }

  unsigned long len = image.GetBufferLength();
  char *p = new char[len];
  image.GetBuffer( p );
  std::stringstream is;
  is.write( p, len );
  delete[] p;

  //ImageCodec ic;
  RAWCodec ic;
  ic.SetPixelFormat(image.GetPixelFormat());
  std::ostringstream os;
  ic.DoInvertMonochrome( is, os );

  DataElement &de = Output->GetDataElement();
  std::string str = os.str();
  VL::Type strSize = (VL::Type)str.size();
  de.SetByteValue( str.c_str(), strSize);
  //Output->GetLUT().Clear();
  Output->SetPhotometricInterpretation( PI );
  //Output->GetPixelFormat().SetSamplesPerPixel( 3 );
  //Output->SetPlanarConfiguration( 0 ); // FIXME OT-PAL-8-face.dcm has a PlanarConfiguration while being PALETTE COLOR...
  //const TransferSyntax &ts = image.GetTransferSyntax();
  ////assert( ts == TransferSyntax::RLELossless );
  //if( ts.IsExplicit() )
  //  {
  //  Output->SetTransferSyntax( TransferSyntax::ExplicitVRLittleEndian );
  //  }
  //else
  //  {
  //  assert( ts.IsImplicit() );
  //  Output->SetTransferSyntax( TransferSyntax::ImplicitVRLittleEndian );
  //  }


  bool success = true;
  return success;
}

bool ImageChangePhotometricInterpretation::ChangeYBR2RGB()
{
  // Ok let's give up on this one for now.
  // We would need to take care of Pixel Padding Value to actually be able to
  // invert the image without this information we potentially will be making
  // mistake. just like Largest Image Pixel Value and other would be wrong
  const Bitmap &image = *Input;
  PhotometricInterpretation pi = image.GetPhotometricInterpretation();
  //assert( pi == PhotometricInterpretation::MONOCHROME1 || pi == PhotometricInterpretation::MONOCHROME2 );
  if( pi == PI )
  {
    return true;
  }

  unsigned long len = image.GetBufferLength();
  char *p8 = new char[len];
  image.GetBuffer( p8 );

  const PixelFormat &pf = image.GetPixelFormat();
  if( image.GetPlanarConfiguration() != 0 ) return false;
  if( pf.GetSamplesPerPixel() != 3 || pf.GetPixelRepresentation() != 0 ) return false;
  if( pf.GetBitsAllocated() == 16 )
  {
    unsigned short *p = (unsigned short*)p8;
    unsigned short rgb[3];
    unsigned short ybr[3];
    for( unsigned long i = 0; i < len / (3 * 2); ++i ) {
      ybr[0] = p[ 3 * i + 0];
      ybr[1] = p[ 3 * i + 1];
      ybr[2] = p[ 3 * i + 2];
      YBR2RGB(rgb, ybr);
      p[ 3 * i + 0] = rgb[0];
      p[ 3 * i + 1] = rgb[1];
      p[ 3 * i + 2] = rgb[2];
    }
  }
  else if( pf.GetBitsAllocated() == 8 )
  {
    unsigned char *p = (unsigned char*)p8;
    unsigned char rgb[3];
    unsigned char ybr[3];
    for( unsigned long i = 0; i < len / 3; ++i ) {
      ybr[0] = p[ 3 * i + 0];
      ybr[1] = p[ 3 * i + 1];
      ybr[2] = p[ 3 * i + 2];
      YBR2RGB(rgb, ybr);
      p[ 3 * i + 0] = rgb[0];
      p[ 3 * i + 1] = rgb[1];
      p[ 3 * i + 2] = rgb[2];
    }
  }

  DataElement &de = Output->GetDataElement();
  de.SetByteValue( p8, len);
  //Output->GetLUT().Clear();
  Output->SetPhotometricInterpretation( PI );
  //Output->GetPixelFormat().SetSamplesPerPixel( 3 );
  //Output->SetPlanarConfiguration( 0 ); // FIXME OT-PAL-8-face.dcm has a PlanarConfiguration while being PALETTE COLOR...
  //const TransferSyntax &ts = image.GetTransferSyntax();
  ////assert( ts == TransferSyntax::RLELossless );
  //if( ts.IsExplicit() )
  //  {
  //  Output->SetTransferSyntax( TransferSyntax::ExplicitVRLittleEndian );
  //  }
  //else
  //  {
  //  assert( ts.IsImplicit() );
  //  Output->SetTransferSyntax( TransferSyntax::ImplicitVRLittleEndian );
  //  }


  bool success = true;
  delete[] p8;
  return success;
}

bool ImageChangePhotometricInterpretation::ChangeRGB2YBR()
{
  // Ok let's give up on this one for now.
  // We would need to take care of Pixel Padding Value to actually be able to
  // invert the image without this information we potentially will be making
  // mistake. just like Largest Image Pixel Value and other would be wrong
  const Bitmap &image = *Input;
  PhotometricInterpretation pi = image.GetPhotometricInterpretation();
  //assert( pi == PhotometricInterpretation::MONOCHROME1 || pi == PhotometricInterpretation::MONOCHROME2 );
  if( pi == PI )
  {
    return true;
  }

  unsigned long len = image.GetBufferLength();
  char *p8 = new char[len];
  image.GetBuffer( p8 );

  const PixelFormat &pf = image.GetPixelFormat();
  if( image.GetPlanarConfiguration() != 0 ) return false;
  if( pf.GetSamplesPerPixel() != 3 || pf.GetPixelRepresentation() != 0 ) return false;
  if( pf.GetBitsAllocated() == 16 )
  {
    unsigned short *p = (unsigned short*)p8;
    unsigned short rgb[3];
    unsigned short ybr[3];
    for( unsigned long i = 0; i < len / (3 * 2); ++i ) {
      rgb[0] = p[ 3 * i + 0];
      rgb[1] = p[ 3 * i + 1];
      rgb[2] = p[ 3 * i + 2];
      RGB2YBR(ybr, rgb, pf.GetBitsStored());
      p[ 3 * i + 0] = ybr[0];
      p[ 3 * i + 1] = ybr[1];
      p[ 3 * i + 2] = ybr[2];
    }
  }
  else if( pf.GetBitsAllocated() == 8 )
  {
    unsigned char *p = (unsigned char*)p8;
    unsigned char rgb[3];
    unsigned char ybr[3];
    for( unsigned long i = 0; i < len / 3; ++i ) {
      rgb[0] = p[ 3 * i + 0];
      rgb[1] = p[ 3 * i + 1];
      rgb[2] = p[ 3 * i + 2];
      RGB2YBR(ybr, rgb, pf.GetBitsStored());
      p[ 3 * i + 0] = ybr[0];
      p[ 3 * i + 1] = ybr[1];
      p[ 3 * i + 2] = ybr[2];
    }
  }

  DataElement &de = Output->GetDataElement();
  de.SetByteValue( p8, len);
  //Output->GetLUT().Clear();
  Output->SetPhotometricInterpretation( PI );
  //Output->GetPixelFormat().SetSamplesPerPixel( 3 );
  //Output->SetPlanarConfiguration( 0 ); // FIXME OT-PAL-8-face.dcm has a PlanarConfiguration while being PALETTE COLOR...
  //const TransferSyntax &ts = image.GetTransferSyntax();
  ////assert( ts == TransferSyntax::RLELossless );
  //if( ts.IsExplicit() )
  //  {
  //  Output->SetTransferSyntax( TransferSyntax::ExplicitVRLittleEndian );
  //  }
  //else
  //  {
  //  assert( ts.IsImplicit() );
  //  Output->SetTransferSyntax( TransferSyntax::ImplicitVRLittleEndian );
  //  }


  bool success = true;
  delete[] p8;
  return success;
}

bool ImageChangePhotometricInterpretation::Change()
{
  // PS 3.3 - 2008 C.7.6.3.1.2 Photometric Interpretation
  Output = Input;
  if( PI == PhotometricInterpretation::YBR_FULL )
    {
    if( Input->GetPhotometricInterpretation() != PhotometricInterpretation::RGB ) return false;
    /*
    In the case where Bits Allocated (0028,0100) has a value of 8 then the following equations convert
    between RGB and YCBCR Photometric Interpretation.
    Y = + .2990R + .5870G + .1140B
    CB = - .1687R - .3313G + .5000B + 128
    CR = + .5000R - .4187G - .0813B + 128
    Note: The above is based on CCIR Recommendation 601-2 dated 1990.
    */
    return ChangeRGB2YBR();
    }
  else if( PI == PhotometricInterpretation::RGB )
    {
    if( Input->GetPhotometricInterpretation() != PhotometricInterpretation::YBR_FULL ) return false;
    /* octave:
     * B = [.2990,.5870,.1140;- .16874, - .33126,  .5000; .5000, - .41869, - .08131]
     * inv(B)
     * 1.0000e+00   -3.6820e-05    1.4020e+00
     * 1.0000e+00   -3.4411e-01   -7.1410e-01
     * 1.0000e+00    1.7720e+00   -1.3458e-04
     */
    return ChangeYBR2RGB();
    }
  else if( PI == PhotometricInterpretation::MONOCHROME1 || PI == PhotometricInterpretation::MONOCHROME2 )
    {
    return ChangeMonochrome();
    }
  //else
  return false;
}


} // end namespace gdcm
