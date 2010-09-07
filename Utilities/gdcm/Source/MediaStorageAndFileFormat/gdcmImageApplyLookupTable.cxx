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
#include "gdcmImageApplyLookupTable.h"

namespace gdcm
{

bool ImageApplyLookupTable::Apply()
{
  Output = Input;
  const Pixmap &image = *Input;

  PhotometricInterpretation pi = image.GetPhotometricInterpretation();
  if( pi != PhotometricInterpretation::PALETTE_COLOR )
    {
    gdcmDebugMacro( "Image is not palettized" );
    return false;
    }
  const gdcm::LookupTable &lut = image.GetLUT();
  int bitsample = lut.GetBitSample();
  assert( bitsample );
  (void)bitsample;//warning removal

  //const DataElement& pixeldata = image.GetDataElement();
  //const ByteValue *bv = pixeldata.GetByteValue();
  //const char *p = bv->GetPointer();
  //std::istringstream is;
  //is.str( std::string( p, p + bv->GetLength() ) );
  unsigned long len = image.GetBufferLength();
  char *p = new char[len];
  image.GetBuffer( p );
  std::stringstream is;
  is.write( p, len );
  delete[] p;

  std::ostringstream os;
  lut.Decode(is, os);

  DataElement &de = Output->GetDataElement();
  std::string str = os.str();
  VL::Type strSize = (VL::Type)str.size();
  de.SetByteValue( str.c_str(), strSize);
  Output->GetLUT().Clear();
  Output->SetPhotometricInterpretation( PhotometricInterpretation::RGB );
  Output->GetPixelFormat().SetSamplesPerPixel( 3 );
  Output->SetPlanarConfiguration( 0 ); // FIXME OT-PAL-8-face.dcm has a PlanarConfiguration while being PALETTE COLOR...
  const gdcm::TransferSyntax &ts = image.GetTransferSyntax();
  //assert( ts == TransferSyntax::RLELossless );
  if( ts.IsExplicit() )
    {
    Output->SetTransferSyntax( TransferSyntax::ExplicitVRLittleEndian );
    }
  else
    {
    assert( ts.IsImplicit() );
    Output->SetTransferSyntax( TransferSyntax::ImplicitVRLittleEndian );
    }


  bool success = true;
  return success;
}


} // end namespace gdcm
