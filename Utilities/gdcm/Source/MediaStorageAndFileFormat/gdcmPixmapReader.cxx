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
#include "gdcmPixmapReader.h"
#include "gdcmExplicitDataElement.h"
#include "gdcmImplicitDataElement.h"
#include "gdcmValue.h"
#include "gdcmFileMetaInformation.h"
#include "gdcmElement.h"
#include "gdcmPhotometricInterpretation.h"
#include "gdcmSegmentedPaletteColorLookupTable.h"
#include "gdcmTransferSyntax.h"
#include "gdcmLookupTable.h"
#include "gdcmAttribute.h"
#include "gdcmIconImage.h"
#include "gdcmPrivateTag.h"
#include "gdcmJPEGCodec.h"

namespace gdcm
{
PixmapReader::PixmapReader():PixelData(new Pixmap)
{
}

PixmapReader::~PixmapReader()
{
}

const Pixmap& PixmapReader::GetPixmap() const
{
  return *PixelData;
}
Pixmap& PixmapReader::GetPixmap()
{
  return *PixelData;
}

//void PixmapReader::SetPixmap(Pixmap const &img)
//{
//  PixelData = img;
//}

const ByteValue* PixmapReader::GetPointerFromElement(Tag const &tag) const
{
  const DataSet &ds = F->GetDataSet();
  if( ds.FindDataElement( tag ) )
    {
    const DataElement &de = ds.GetDataElement( tag );
    return de.GetByteValue();
    }
  return 0;
}

bool PixmapReader::Read()
{
  if( !Reader::Read() )
    {
    // cemra_bug/IM-0001-0066.dcm
    // will return from the parser with an error
    // but a partial Pixel Data can be seen
    return false;
    }

  const FileMetaInformation &header = F->GetHeader();
  const DataSet &ds = F->GetDataSet();
  const TransferSyntax &ts = header.GetDataSetTransferSyntax();

  // Need to set the type of image we are dealing with:
  PixelData->SetTransferSyntax( ts );

  bool res = false;
  /* Does it really make sense to check for Media Storage SOP Class UID?
   * I need then to check consistency with 0008 0016 Instance SOP Class UID
   * ... I don't think there is an end.
   * I'd rather go the old way check a bunch of tags (From Image Plane
   * Module).
   */
  MediaStorage ms = header.GetMediaStorage();
  bool isImage = MediaStorage::IsImage( ms );
  if( isImage )
    {
    // I cannot leave this here, since ELSCINT1 / LOSSLESS RICE declares CT Image Storage,
    // when in fact this is a private Media Storage (no Pixel Data element)
    //assert( ds.FindDataElement( Tag(0x7fe0,0x0010 ) ) );
    assert( ts != TransferSyntax::TS_END && ms != MediaStorage::MS_END );
    // Good it's the easy case. It's declared as an Image:
    gdcmDebugMacro( "Sweet ! Finally a good DICOM file !" );
    //PixelData->SetCompressionFromTransferSyntax( ts );
    res = ReadImage(ms);
    }
  //else if( ms == MediaStorage::MRSpectroscopyStorage )
  //  {
  //  res = ReadImage(ms);
  //  }
  else
    {
    //assert( !ds.FindDataElement( Tag(0x7fe0,0x0010 ) ) );
    if( ms != MediaStorage::MS_END )
      {
      gdcmDebugMacro( "DICOM file is not an Image file but a : " <<
        MediaStorage::GetMSString(ms) << " SOP Class UID" );
      res = false;
      }
    else
      {
      // Too bad the media storage type was not recognized...
      // what should we do ?
      // Let's check 0008,0016:
      // D 0008|0016 [UI] [SOP Class UID] [1.2.840.10008.5.1.4.1.1.7 ]
      // ==> [Secondary Capture Image Storage]
      const Tag tsopclassuid(0x0008, 0x0016);
      if( ds.FindDataElement( tsopclassuid) && !ds.GetDataElement( tsopclassuid).IsEmpty() )
        {
        const ByteValue *sopclassuid
          = GetPointerFromElement( tsopclassuid );
        std::string sopclassuid_str(
          sopclassuid->GetPointer(),
          sopclassuid->GetLength() );
        MediaStorage ms2 = MediaStorage::GetMSType(sopclassuid_str.c_str());
        bool isImage2 = MediaStorage::IsImage( ms2 );
        if( isImage2 )
          {
          gdcmDebugMacro( "After all it might be a DICOM file "
            "(Mallinckrodt-like)" );

    //PixelData->SetCompressionFromTransferSyntax( ts );
          //PixelData->SetCompressionType( Compression::RAW );
          res = ReadImage(ms2);
          }
        else
          {
          ms2.SetFromFile( *F );
          if( MediaStorage::IsImage( ms2 ) )
            {
            res = ReadImage(ms2);
            }
          else
            {
            gdcmDebugMacro( "DICOM file is not an Image file but a : " <<
              MediaStorage::GetMSString(ms2) << " SOP Class UID" );
            res = false;
            }
          }
        }
      else if( ts == TransferSyntax::ImplicitVRBigEndianACRNEMA || header.IsEmpty() )
        {
        // Those transfer syntax have a high probability of being ACR NEMA
        gdcmDebugMacro( "Looks like an ACR-NEMA file" );
        // Hopefully all ACR-NEMA are RAW:
        //PixelData->SetCompressionType( Compression::RAW );
        res = ReadACRNEMAImage();
        }
      else // there is a Unknown Media Storage Syntax
        {
        assert( ts != TransferSyntax::TS_END && ms == MediaStorage::MS_END );
        // god damit I don't know what to do...
        gdcmWarningMacro( "Attempting to read this file as a DICOM file"
          "\nDesperate attempt" );
        MediaStorage ms3;
        ms3.SetFromFile( GetFile() );
        if( ms3 != MediaStorage::MS_END )
          {
          res = ReadImage(ms3);
          }
        else
          {
          // Giving up
          res = false;
          }
        }
      }
    }

  //if(res) PixelData->Print( std::cout );
  return res;
}

// PICKER-16-MONO2-Nested_icon.dcm
void DoIconImage(const DataSet& rootds, Pixmap& image)
{
  const Tag ticonimage(0x0088,0x0200);
  const PrivateTag tgeiconimage(0x0009,0x0010,"GEIIS");
  // AFAIK this icon SQ is undocumented , but I found it in:
  // gdcmDataExtra/gdcmBreakers/2929J888_8b_YBR_RLE_PlanConf0_breaker.dcm
  // aka 'SmallPreview'
  // The SQ contains a DataElement:
  // (0002,0010) UI [1.2.840.10008.1.2.1]                          # 20,1 Transfer Syntax UID
  // sigh...
  const PrivateTag tgeiconimage2(0x6003,0x0010,"GEMS_Ultrasound_ImageGroup_001");
  IconImage &pixeldata = image.GetIconImage();
  if( rootds.FindDataElement( ticonimage ) )
    {
    const DataElement &iconimagesq = rootds.GetDataElement( ticonimage );
    //const SequenceOfItems* sq = iconimagesq.GetSequenceOfItems();
    SmartPointer<SequenceOfItems> sq = iconimagesq.GetValueAsSQ();
    // Is SQ empty ?
    if( !sq ) return;
    SequenceOfItems::ConstIterator it = sq->Begin();
    const DataSet &ds = it->GetNestedDataSet();

    // D 0028|0011 [US] [Columns] [512]
      {
      //const DataElement& de = ds.GetDataElement( Tag(0x0028, 0x0011) );
      Attribute<0x0028,0x0011> at = { 0 };
      at.SetFromDataSet( ds );
      pixeldata.SetDimension(0, at.GetValue() );
      }

    // D 0028|0010 [US] [Rows] [512]
      {
      //const DataElement& de = ds.GetDataElement( Tag(0x0028, 0x0010) );
      Attribute<0x0028,0x0010> at = { 0 };
      at.SetFromDataSet( ds );
      pixeldata.SetDimension(1, at.GetValue() );
      }

    PixelFormat pf;
    // D 0028|0100 [US] [Bits Allocated] [16]
      {
      //const DataElement& de = ds.GetDataElement( Tag(0x0028, 0x0100) );
      Attribute<0x0028,0x0100> at = { 0 };
      at.SetFromDataSet( ds );
      pf.SetBitsAllocated( at.GetValue() );
      }
    // D 0028|0101 [US] [Bits Stored] [12]
      {
      //const DataElement& de = ds.GetDataElement( Tag(0x0028, 0x0101) );
      Attribute<0x0028,0x0101> at = { 0 };
      at.SetFromDataSet( ds );
      pf.SetBitsStored( at.GetValue() );
      }
    // D 0028|0102 [US] [High Bit] [11]
      {
      //const DataElement& de = ds.GetDataElement( Tag(0x0028, 0x0102) );
      Attribute<0x0028,0x0102> at = { 0 };
      at.SetFromDataSet( ds );
      pf.SetHighBit( at.GetValue() );
      }
    // D 0028|0103 [US] [Pixel Representation] [0]
      {
      //const DataElement& de = ds.GetDataElement( Tag(0x0028, 0x0103) );
      Attribute<0x0028,0x0103> at = { 0 };
      at.SetFromDataSet( ds );
      pf.SetPixelRepresentation( at.GetValue() );
      }
    // (0028,0002) US 1                                        #   2, 1 SamplesPerPixel
      {
    //if( ds.FindDataElement( Tag(0x0028, 0x0002) ) )
    {
    //const DataElement& de = ds.GetDataElement( Tag(0x0028, 0x0002) );
      Attribute<0x0028,0x0002> at = { 1 };
      at.SetFromDataSet( ds );
      pf.SetSamplesPerPixel( at.GetValue() );
    }
    // else pf will default to 1...
      }
    pixeldata.SetPixelFormat( pf );
    // D 0028|0004 [CS] [Photometric Interpretation] [MONOCHROME2 ]
    const Tag tphotometricinterpretation(0x0028, 0x0004);
    assert( ds.FindDataElement( tphotometricinterpretation ) );
    const ByteValue *photometricinterpretation = ds.GetDataElement( tphotometricinterpretation ).GetByteValue();
    std::string photometricinterpretation_str(
      photometricinterpretation->GetPointer(),
      photometricinterpretation->GetLength() );
    PhotometricInterpretation pi(
      PhotometricInterpretation::GetPIType(
        photometricinterpretation_str.c_str()));
    assert( pi != PhotometricInterpretation::UNKNOW);
    pixeldata.SetPhotometricInterpretation( pi );

    //
    if ( pi == PhotometricInterpretation::PALETTE_COLOR )
      {
      SmartPointer<LookupTable> lut = new LookupTable;
      const Tag testseglut(0x0028, (0x1221 + 0));
      if( ds.FindDataElement( testseglut ) )
        {
        assert(0 && "Please report this image");
        lut = new SegmentedPaletteColorLookupTable;
        }
      //SmartPointer<SegmentedPaletteColorLookupTable> lut = new SegmentedPaletteColorLookupTable;
      lut->Allocate( pf.GetBitsAllocated() );

      // for each red, green, blue:
      for(int i=0; i<3; ++i)
        {
        // (0028,1101) US 0\0\16
        // (0028,1102) US 0\0\16
        // (0028,1103) US 0\0\16
        const Tag tdescriptor(0x0028, (0x1101 + i));
        //const Tag tdescriptor(0x0028, 0x3002);
        Element<VR::US,VM::VM3> el_us3;
        // Now pass the byte array to a DICOMizer:
        el_us3.SetFromDataElement( ds[tdescriptor] ); //.GetValue() );
        lut->InitializeLUT( LookupTable::LookupTableType(i),
          el_us3[0], el_us3[1], el_us3[2] );

        // (0028,1201) OW
        // (0028,1202) OW
        // (0028,1203) OW
        const Tag tlut(0x0028, (0x1201 + i));
        //const Tag tlut(0x0028, 0x3006);

        // Segmented LUT
        // (0028,1221) OW
        // (0028,1222) OW
        // (0028,1223) OW
        const Tag seglut(0x0028, (0x1221 + i));
        if( ds.FindDataElement( tlut ) )
          {
          const ByteValue *lut_raw = ds.GetDataElement( tlut ).GetByteValue();
          assert( lut_raw );
          // LookupTableType::RED == 0
          lut->SetLUT( LookupTable::LookupTableType(i),
            (unsigned char*)lut_raw->GetPointer(), lut_raw->GetLength() );
          //assert( pf.GetBitsAllocated() == el_us3.GetValue(2) );

          unsigned long check =
            (el_us3.GetValue(0) ? el_us3.GetValue(0) : 65536)
            * el_us3.GetValue(2) / 8;
          assert( check == lut_raw->GetLength() ); (void)check;
          }
        else if( ds.FindDataElement( seglut ) )
          {
          const ByteValue *lut_raw = ds.GetDataElement( seglut ).GetByteValue();
          assert( lut_raw );
          lut->SetLUT( LookupTable::LookupTableType(i),
            (unsigned char*)lut_raw->GetPointer(), lut_raw->GetLength() );
          //assert( pf.GetBitsAllocated() == el_us3.GetValue(2) );

          //unsigned long check =
          //  (el_us3.GetValue(0) ? el_us3.GetValue(0) : 65536)
          //  * el_us3.GetValue(2) / 8;
          //assert( check == lut_raw->GetLength() ); (void)check;
          }
        else
          {
          gdcmWarningMacro( "Icon Sequence is incomplete. Giving up" );
          pixeldata.Clear();
          return;
          }
        }
      pixeldata.SetLUT(*lut);
      }

    const Tag tpixeldata = Tag(0x7fe0, 0x0010);
    if( !ds.FindDataElement( tpixeldata ) )
      {
      gdcmWarningMacro( "Icon Sequence is incomplete. Giving up" );
      pixeldata.Clear();
      return;
      }
    const DataElement& de = ds.GetDataElement( tpixeldata );
    pixeldata.SetDataElement( de );

    // Pass TransferSyntax:
    pixeldata.SetTransferSyntax( image.GetTransferSyntax() );
    }
  else if( false && rootds.FindDataElement( tgeiconimage ) )
    {
    const DataElement &iconimagesq = rootds.GetDataElement( tgeiconimage );
    //const SequenceOfItems* sq = iconimagesq.GetSequenceOfItems();
    SmartPointer<SequenceOfItems> sq = iconimagesq.GetValueAsSQ();
    // Is SQ empty ?
    if( !sq ) return;
    SequenceOfItems::ConstIterator it = sq->Begin();
    const DataSet &ds = it->GetNestedDataSet();

    // D 0028|0011 [US] [Columns] [512]
      {
      //const DataElement& de = ds.GetDataElement( Tag(0x0028, 0x0011) );
      Attribute<0x0028,0x0011> at = { 0 };
      at.SetFromDataSet( ds );
      pixeldata.SetDimension(0, at.GetValue() );
      }

    // D 0028|0010 [US] [Rows] [512]
      {
      //const DataElement& de = ds.GetDataElement( Tag(0x0028, 0x0010) );
      Attribute<0x0028,0x0010> at = { 0 };
      at.SetFromDataSet( ds );
      pixeldata.SetDimension(1, at.GetValue() );
      }

    PixelFormat pf;
    // D 0028|0100 [US] [Bits Allocated] [16]
      {
      //const DataElement& de = ds.GetDataElement( Tag(0x0028, 0x0100) );
      Attribute<0x0028,0x0100> at = { 0 };
      at.SetFromDataSet( ds );
      pf.SetBitsAllocated( at.GetValue() );
      }
    // D 0028|0101 [US] [Bits Stored] [12]
      {
      //const DataElement& de = ds.GetDataElement( Tag(0x0028, 0x0101) );
      Attribute<0x0028,0x0101> at = { 0 };
      at.SetFromDataSet( ds );
      pf.SetBitsStored( at.GetValue() );
      }
    // D 0028|0102 [US] [High Bit] [11]
      {
      //const DataElement& de = ds.GetDataElement( Tag(0x0028, 0x0102) );
      Attribute<0x0028,0x0102> at = { 0 };
      at.SetFromDataSet( ds );
      pf.SetHighBit( at.GetValue() );
      }
    // D 0028|0103 [US] [Pixel Representation] [0]
      {
      //const DataElement& de = ds.GetDataElement( Tag(0x0028, 0x0103) );
      Attribute<0x0028,0x0103> at = { 0 };
      at.SetFromDataSet( ds );
      pf.SetPixelRepresentation( at.GetValue() );
      }
    // (0028,0002) US 1                                        #   2, 1 SamplesPerPixel
      {
      //const DataElement& de = ds.GetDataElement( Tag(0x0028, 0x0002) );
      Attribute<0x0028,0x0002> at = { 1 };
      at.SetFromDataSet( ds );
      pf.SetSamplesPerPixel( at.GetValue() );
      }
    pixeldata.SetPixelFormat( pf );
    // D 0028|0004 [CS] [Photometric Interpretation] [MONOCHROME2 ]
    const Tag tphotometricinterpretation(0x0028, 0x0004);
    assert( ds.FindDataElement( tphotometricinterpretation ) );
    const ByteValue *photometricinterpretation = ds.GetDataElement( tphotometricinterpretation ).GetByteValue();
    std::string photometricinterpretation_str(
      photometricinterpretation->GetPointer(),
      photometricinterpretation->GetLength() );
    PhotometricInterpretation pi(
      PhotometricInterpretation::GetPIType(
        photometricinterpretation_str.c_str()));
    assert( pi != PhotometricInterpretation::UNKNOW);
    pixeldata.SetPhotometricInterpretation( pi );
    const Tag tpixeldata = Tag(0x7fe0, 0x0010);
    assert( ds.FindDataElement( tpixeldata ) );
      {
      const DataElement& de = ds.GetDataElement( tpixeldata );
      JPEGCodec jpeg;
      jpeg.SetPhotometricInterpretation( pixeldata.GetPhotometricInterpretation() );
      jpeg.SetPlanarConfiguration( 0 );
      PixelFormat pf = pixeldata.GetPixelFormat();
      // Apparently bits stored can only be 8 or 12:
      if( pf.GetBitsStored() == 16 )
        {
        pf.SetBitsStored( 12 );
        }
      jpeg.SetPixelFormat( pf );
      DataElement de2;
      jpeg.Decode( de, de2);
      pixeldata.SetDataElement( de2 );
      }
    }
  else if( false && rootds.FindDataElement( tgeiconimage2 ) )
    {
    const DataElement &iconimagesq = rootds.GetDataElement( tgeiconimage2 );
    //const SequenceOfItems* sq = iconimagesq.GetSequenceOfItems();
    SmartPointer<SequenceOfItems> sq = iconimagesq.GetValueAsSQ();
    // Is SQ empty ?
    if( !sq ) return;
    SequenceOfItems::ConstIterator it = sq->Begin();
    const DataSet &ds = it->GetNestedDataSet();

    // D 0028|0011 [US] [Columns] [512]
      {
      const DataElement& de = ds.GetDataElement( Tag(0x0028, 0x0011) );
      Attribute<0x0028,0x0011> at;
      at.SetFromDataElement( de );
      pixeldata.SetDimension(0, at.GetValue() );
      }

    // D 0028|0010 [US] [Rows] [512]
      {
      const DataElement& de = ds.GetDataElement( Tag(0x0028, 0x0010) );
      Attribute<0x0028,0x0010> at;
      at.SetFromDataElement( de );
      pixeldata.SetDimension(1, at.GetValue() );
      }

    PixelFormat pf;
    // D 0028|0100 [US] [Bits Allocated] [16]
      {
      const DataElement& de = ds.GetDataElement( Tag(0x0028, 0x0100) );
      Attribute<0x0028,0x0100> at;
      at.SetFromDataElement( de );
      pf.SetBitsAllocated( at.GetValue() );
      }
    // D 0028|0101 [US] [Bits Stored] [12]
      {
      const DataElement& de = ds.GetDataElement( Tag(0x0028, 0x0101) );
      Attribute<0x0028,0x0101> at;
      at.SetFromDataElement( de );
      pf.SetBitsStored( at.GetValue() );
      }
    // D 0028|0102 [US] [High Bit] [11]
      {
      const DataElement& de = ds.GetDataElement( Tag(0x0028, 0x0102) );
      Attribute<0x0028,0x0102> at;
      at.SetFromDataElement( de );
      pf.SetHighBit( at.GetValue() );
      }
    // D 0028|0103 [US] [Pixel Representation] [0]
      {
      const DataElement& de = ds.GetDataElement( Tag(0x0028, 0x0103) );
      Attribute<0x0028,0x0103> at;
      at.SetFromDataElement( de );
      pf.SetPixelRepresentation( at.GetValue() );
      }
    // (0028,0002) US 1                                        #   2, 1 SamplesPerPixel
      {
      const DataElement& de = ds.GetDataElement( Tag(0x0028, 0x0002) );
      Attribute<0x0028,0x0002> at;
      at.SetFromDataElement( de );
      pf.SetSamplesPerPixel( at.GetValue() );
      }
    pixeldata.SetPixelFormat( pf );
    // D 0028|0004 [CS] [Photometric Interpretation] [MONOCHROME2 ]
    const Tag tphotometricinterpretation(0x0028, 0x0004);
    assert( ds.FindDataElement( tphotometricinterpretation ) );
    const ByteValue *photometricinterpretation = ds.GetDataElement( tphotometricinterpretation ).GetByteValue();
    std::string photometricinterpretation_str(
      photometricinterpretation->GetPointer(),
      photometricinterpretation->GetLength() );
    PhotometricInterpretation pi(
      PhotometricInterpretation::GetPIType(
        photometricinterpretation_str.c_str()));
    assert( pi != PhotometricInterpretation::UNKNOW);
    pixeldata.SetPhotometricInterpretation( pi );
    //const Tag tpixeldata = Tag(0x7fe0, 0x0010);
    const PrivateTag tpixeldata(0x6003,0x0011,"GEMS_Ultrasound_ImageGroup_001");
    assert( ds.FindDataElement( tpixeldata ) );
      {
      const DataElement& de = ds.GetDataElement( tpixeldata );
      pixeldata.SetDataElement( de );
      /*
      JPEGCodec jpeg;
      jpeg.SetPhotometricInterpretation( pixeldata.GetPhotometricInterpretation() );
      jpeg.SetPlanarConfiguration( 0 );
      PixelFormat pf = pixeldata.GetPixelFormat();
      // Apparently bits stored can only be 8 or 12:
      if( pf.GetBitsStored() == 16 )
      {
      pf.SetBitsStored( 12 );
      }
      jpeg.SetPixelFormat( pf );
      DataElement de2;
      jpeg.Decode( de, de2);
      pixeldata.SetDataElement( de2 );
       */
      }
    }
  else
    {
    //gdcmDebugMacro( "No icon found" );
    }
}

// GE_DLX-8-MONO2-Multiframe.dcm
void DoCurves(const DataSet& ds, Pixmap& pixeldata)
{
  unsigned int numcurves;
  if( (numcurves = Curve::GetNumberOfCurves( ds )) )
    {
    pixeldata.SetNumberOfCurves( numcurves );

    Tag curve(0x5000,0x0000);
    bool finished = false;
    unsigned int idxcurves = 0;
    while( !finished )
      {
      const DataElement &de = ds.FindNextDataElement( curve );
      // Are we done:
      if( de.GetTag().GetGroup() > 0x50FF ) // last possible curve curve
        {
        finished = true;
        }
      else if( de.GetTag().IsPrivate() ) // GEMS owns some 0x5003
        {
        // Move on to the next public one:
        curve.SetGroup( de.GetTag().GetGroup() + 1 );
        curve.SetElement( 0 );
        }
      else
        {
        // Yay! this is an curve element
        Curve &ov = pixeldata.GetCurve(idxcurves);
        ++idxcurves; // move on to the next one
        curve = de.GetTag();
        uint16_t currentcurve = curve.GetGroup();
        assert( !(currentcurve % 2) ); // 0x6001 is not an curve...
        // Now loop on all element from this current group:
        DataElement de2 = de;
        while( de2.GetTag().GetGroup() == currentcurve )
          {
          ov.Update(de2);
          curve.SetElement( de2.GetTag().GetElement() + 1 );
          de2 = ds.FindNextDataElement( curve );
          // Next element:
          //curve.SetElement( curve.GetElement() + 1 );
          }
        // If we exit the loop we have pass the current curve and potentially point to the next one:
        //curve.SetElement( curve.GetElement() + 1 );
        //ov.Print( std::cout );
        }
      }
    //std::cout << "Num of curves: " << numcurves << std::endl;
    assert( idxcurves == numcurves );
    }
}

void DoOverlays(const DataSet& ds, Pixmap& pixeldata)
{
  unsigned int numoverlays;
  if( (numoverlays = Overlay::GetNumberOfOverlays( ds )) )
    {
    pixeldata.SetNumberOfOverlays( numoverlays );

    Tag overlay(0x6000,0x0000);
    bool finished = false;
    unsigned int idxoverlays = 0;
    while( !finished )
      {
      const DataElement &de = ds.FindNextDataElement( overlay );
      // Are we done:
      if( de.GetTag().GetGroup() > 0x60FF ) // last possible overlay curve
        {
        finished = true;
        }
      else if( de.GetTag().IsPrivate() ) // GEMS owns some 0x6003
        {
        // Move on to the next public one:
        overlay.SetGroup( de.GetTag().GetGroup() + 1 );
        overlay.SetElement( 0 );
        }
      else
        {
        // Yay! this is an overlay element
        Overlay &ov = pixeldata.GetOverlay(idxoverlays);
        ++idxoverlays; // move on to the next one
        overlay = de.GetTag();
        uint16_t currentoverlay = overlay.GetGroup();
        assert( !(currentoverlay % 2) ); // 0x6001 is not an overlay...
        // Now loop on all element from this current group:
        DataElement de2 = de;
        while( de2.GetTag().GetGroup() == currentoverlay )
          {
          ov.Update(de2);
          overlay.SetElement( de2.GetTag().GetElement() + 1 );
          de2 = ds.FindNextDataElement( overlay );
          // Next element:
          //overlay.SetElement( overlay.GetElement() + 1 );
          }
        // If we exit the loop we have pass the current overlay and potentially point to the next one:
        //overlay.SetElement( overlay.GetElement() + 1 );
        //ov.Print( std::cout );

        // Let's decode it:
        std::ostringstream unpack;
        ov.Decompress( unpack );
        std::string s = unpack.str();
        //size_t l = s.size();
        // The following line will fail with images like XA_GE_JPEG_02_with_Overlays.dcm
        // since the overlays are stored in the unused bit of the PixelData
        if( !ov.IsEmpty() )
          {
          //assert( unpack.str().size() / 8 == ((ov.GetRows() * ov.GetColumns()) + 7 ) / 8 );
          assert( ov.IsInPixelData( ) == false );
          }
        else
          {
          gdcmDebugMacro( "This image does not contains Overlay in the 0x60xx tags. "
            << "Instead the overlay is stored in the unused bit of the Pixel Data. "
            << "This is not supported right now"
            << std::endl );
          ov.IsInPixelData( true );
          if( !ov.GrabOverlayFromPixelData(ds) )
            {
            gdcmErrorMacro( "Could not extract Overlay from Pixel Data" );
            }
          }
        }
      }
    //std::cout << "Num of Overlays: " << numoverlays << std::endl;
    assert( idxoverlays == numoverlays );
    }
}

bool PixmapReader::ReadImage(MediaStorage const &ms)
{
  const DataSet &ds = F->GetDataSet();
  std::stringstream ss;
  std::string conversion;

  bool isacrnema = false;
  const Tag trecognitioncode(0x0008,0x0010);
  if( ds.FindDataElement( trecognitioncode ) && !ds.GetDataElement( trecognitioncode ).IsEmpty() )
    {
    // PHILIPS_Gyroscan-12-MONO2-Jpeg_Lossless.dcm
    // PHILIPS_Gyroscan-12-Jpeg_Extended_Process_2_4.dcm
    gdcmDebugMacro( "Mixture of ACR NEMA and DICOM file" );
    isacrnema = true;
    const char *str = ds.GetDataElement( trecognitioncode ).GetByteValue()->GetPointer();
    assert( strncmp( str, "ACR-NEMA", strlen( "ACR-NEMA" ) ) == 0 ||
      strncmp( str, "ACRNEMA", strlen( "ACRNEMA" ) ) == 0 );
    (void)str;//warning removal
    }

  // Ok we have the dataset let's feed the Image (PixelData)
  // 1. First find how many dimensions there is:
  // D 0028|0008 [IS] [Number of Frames] [8 ]
  const Tag tnumberofframes = Tag(0x0028, 0x0008);
  //if( ds.FindDataElement( tnumberofframes ) /*&& ms != MediaStorage::SecondaryCaptureImageStorage*/ )
    {
    //const DataElement& de = ds.GetDataElement( tnumberofframes );
    Attribute<0x0028,0x0008> at = { 0 };
    at.SetFromDataSet( ds );
    int numberofframes = at.GetValue();
    // What should I do when numberofframes == 0 ?
    if( numberofframes > 1 )
      {
      PixelData->SetNumberOfDimensions(3);
      PixelData->SetDimension(2, numberofframes );
      }
    else
      {
      gdcmDebugMacro( "NumberOfFrames was specified with a value of: "
        << numberofframes );
      PixelData->SetNumberOfDimensions(2);
      }
    }
  //else
  //  {
  //  gdcmDebugMacro( "Attempting a guess for the number of dimensions" ); // FIXME
  //  PixelData->SetNumberOfDimensions(2);
  //  }


  // 2. What are the col & rows:
  // D 0028|0011 [US] [Columns] [512]
  //const Tag tcolumns(0x0028, 0x0011);
  //if( ds.FindDataElement( tcolumns ) )
    {
    //PixelData->SetDimension(0,
    //  ReadUSFromTag( tcolumns, ss, conversion ) );
    //const DataElement& de = ds.GetDataElement( tcolumns );
    Attribute<0x0028,0x0011> at = { 0 };
    at.SetFromDataSet( ds );
    PixelData->SetDimension(0, at.GetValue() );
    }
  //else
  //  {
  //  const TransferSyntax &ts = PixelData->GetTransferSyntax();
  //  gdcmWarningMacro( "This should not happen: No Columns found." );
  //  if( !ts.IsEncapsulated() || ts == TransferSyntax::RLELossless )
  //    {
  //    // Pretty bad we really need this information. Should not
  //    // happen in theory. Maybe papyrus files ??
  //    return false;
  //    }
  //  }

  // D 0028|0010 [US] [Rows] [512]
  //PixelData->SetDimension(1,
  //  ReadUSFromTag( Tag(0x0028, 0x0010), ss, conversion ) );
    {
    Attribute<0x0028,0x0010> at = { 0 };
    //if( ds.FindDataElement( at.GetTag() ) )
      {
      //const DataElement& de = ds.GetDataElement( at.GetTag() );
      at.SetFromDataSet( ds );
      PixelData->SetDimension(1, at.GetValue() );
      //assert( at.GetValue() == ReadUSFromTag( Tag(0x0028, 0x0010), ss, conversion ) );
      }
    //else
    //  {
    //  const TransferSyntax &ts = PixelData->GetTransferSyntax();
    //  gdcmWarningMacro( "This should not happen: No Rows found." );
    //  if( !ts.IsEncapsulated() || ts == TransferSyntax::RLELossless )
    //    {
    //    // Pretty bad we really need this information. Should not
    //    // happen in theory. Maybe papyrus files ??
    //    return false;
    //    }
    //  }
    }

  // Dummy check
  //const unsigned int *dims = PixelData->GetDimensions();
  //if( dims[0] == 0 || dims[1] == 0 )
  //  {
  //  // PhilipsLosslessRice.dcm
  //  gdcmWarningMacro( "Image is empty" );
  //  return false;
  //  }

  // 3. Pixel Format ?
  PixelFormat pf;
  // D 0028|0002 [US] [Samples per Pixel] [1]
  const Tag samplesperpixel = Tag(0x0028, 0x0002);
    {
    Attribute<0x0028,0x0002> at = { 1 }; // By default assume 1 Samples Per Pixel
    at.SetFromDataSet( ds );
    pf.SetSamplesPerPixel( at.GetValue() );
    }

  if( ms == MediaStorage::MRSpectroscopyStorage )
    {
    pf.SetScalarType( PixelFormat::FLOAT32 );
    }
  else
    {
    assert( MediaStorage::IsImage( ms ) );
    // D 0028|0100 [US] [Bits Allocated] [16]
    //pf.SetBitsAllocated(
    //  ReadUSFromTag( Tag(0x0028, 0x0100), ss, conversion ) );
    {
    //const DataElement& de = ds.GetDataElement( Tag(0x0028, 0x0100) );
    Attribute<0x0028,0x0100> at = { 0 };
    at.SetFromDataSet( ds );
    pf.SetBitsAllocated( at.GetValue() );
    //assert( at.GetValue() == ReadUSFromTag( Tag(0x0028, 0x0100), ss, conversion ) );
    }

    // D 0028|0101 [US] [Bits Stored] [12]
    //pf.SetBitsStored(
    //  ReadUSFromTag( Tag(0x0028, 0x0101), ss, conversion ) );
    {
    //const DataElement& de = ds.GetDataElement( Tag(0x0028, 0x0101) );
    Attribute<0x0028,0x0101> at = { 0 };
    at.SetFromDataSet( ds );
    pf.SetBitsStored( at.GetValue() );
    //assert( at.GetValue() == ReadUSFromTag( Tag(0x0028, 0x0101), ss, conversion ) );
    }

    // D 0028|0102 [US] [High Bit] [11]
    //pf.SetHighBit(
    //  ReadUSFromTag( Tag(0x0028, 0x0102), ss, conversion ) );
    {
    //const DataElement& de = ds.GetDataElement( Tag(0x0028, 0x0102) );
    Attribute<0x0028,0x0102> at = { 0 };
    at.SetFromDataSet( ds );
    pf.SetHighBit( at.GetValue() );
    //assert( at.GetValue() == ReadUSFromTag( Tag(0x0028, 0x0102), ss, conversion ) );
    }

    // D 0028|0103 [US] [Pixel Representation] [0]
    //Tag tpixelrep(0x0028, 0x0103);
    //if( ds.FindDataElement( tpixelrep ) && !ds.GetDataElement( tpixelrep ).IsEmpty() )
      {
      //pf.SetPixelRepresentation(
      //  ReadUSFromTag( Tag(0x0028, 0x0103), ss, conversion ) );
    //const DataElement& de = ds.GetDataElement( Tag(0x0028, 0x0103) );
    Attribute<0x0028,0x0103> at = { 0 };
    at.SetFromDataSet( ds );
    pf.SetPixelRepresentation( at.GetValue() );
    //assert( at.GetValue() == ReadUSFromTag( Tag(0x0028, 0x0103), ss, conversion ) );

      }
//    else
//      {
//      gdcmWarningMacro( "Pixel Representation was not found. Default to Unsigned Pixel Representation" );
//      pf.SetPixelRepresentation( 0 );
//      }
    }

  // 5. Photometric Interpretation
  // D 0028|0004 [CS] [Photometric Interpretation] [MONOCHROME2 ]
  const Tag tphotometricinterpretation(0x0028, 0x0004);
  const ByteValue *photometricinterpretation
    = GetPointerFromElement( tphotometricinterpretation );
  PhotometricInterpretation pi = PhotometricInterpretation::UNKNOW;
  if( photometricinterpretation )
    {
    std::string photometricinterpretation_str(
      photometricinterpretation->GetPointer(),
      photometricinterpretation->GetLength() );
    pi = PhotometricInterpretation::GetPIType( photometricinterpretation_str.c_str() );
    }
  else
    {
    if( pf.GetSamplesPerPixel() == 1 )
      {
      gdcmWarningMacro( "No PhotometricInterpretation found, default to MONOCHROME2" );
      pi = PhotometricInterpretation::MONOCHROME2;
      }
    else if( pf.GetSamplesPerPixel() == 3 )
      {
      gdcmWarningMacro( "No PhotometricInterpretation found, default to RGB" );
      pi = PhotometricInterpretation::RGB;
      }
    else if( pf.GetSamplesPerPixel() == 4 )
      {
      gdcmWarningMacro( "No PhotometricInterpretation found, default to RGB" );
      pi = PhotometricInterpretation::ARGB;
      }
    }

  if( !pf.GetSamplesPerPixel() || (pi.GetSamplesPerPixel() != pf.GetSamplesPerPixel()) )
    {
    if( pi != PhotometricInterpretation::UNKNOW )
      {
      pf.SetSamplesPerPixel( pi.GetSamplesPerPixel() );
      }
    else if ( isacrnema )
      {
      assert ( pf.GetSamplesPerPixel() == 0 );
      assert ( pi == PhotometricInterpretation::UNKNOW );
      pf.SetSamplesPerPixel( 1 );
      pi = PhotometricInterpretation::MONOCHROME2;
      }
    else
      {
      gdcmWarningMacro( "Cannot recognize image type. Does not looks like ACR-NEMA and is missing both Sample Per Pixel AND PhotometricInterpretation. Please report" );
      return false;
      }
    }
  assert ( pf.GetSamplesPerPixel() != 0 );
  // Very important to set the PixelFormat here before PlanarConfiguration
  PixelData->SetPixelFormat( pf );
  pf = PixelData->GetPixelFormat();
  if( !pf.IsValid() )
    {
    return false;
    }
  if( pi == PhotometricInterpretation::UNKNOW ) return false;
  PixelData->SetPhotometricInterpretation( pi );

  // 4. Planar Configuration
  // D 0028|0006 [US] [Planar Configuration] [1]
  const Tag planarconfiguration = Tag(0x0028, 0x0006);
  // FIXME: Whatif planaconfiguration is send in a grayscale image... it would be empty...
  // well hopefully :(
  if( ds.FindDataElement( planarconfiguration ) && !ds.GetDataElement( planarconfiguration ).IsEmpty() )
    {
    const DataElement& de = ds.GetDataElement( planarconfiguration );
    Attribute<0x0028,0x0006> at = { 0 };
    at.SetFromDataElement( de );

    //unsigned int pc = ReadUSFromTag( planarconfiguration, ss, conversion );
    unsigned int pc = at.GetValue();
    if( pc && PixelData->GetPixelFormat().GetSamplesPerPixel() != 3 )
      {
      gdcmDebugMacro( "Cannot have PlanarConfiguration=1, when Sample Per Pixel != 3" );
      pc = 0;
      }
    PixelData->SetPlanarConfiguration( pc );
    }


  // Do the Palette Color:
  // 1. Modality LUT Sequence
  bool modlut = ds.FindDataElement(Tag(0x0028,0x3000) );
  if( modlut )
    {
    gdcmWarningMacro( "Modality LUT (0028,3000) are not handled. Image will not be displayed properly" );
    }
  // 2. LUTData (0028,3006)
  // technically I do not need to warn about LUTData since either modality lut XOR VOI LUT need to
  // be sent to require a LUT Data...
  bool lutdata = ds.FindDataElement(Tag(0x0028,0x3006) );
  if( lutdata )
    {
    gdcmWarningMacro( "LUT Data (0028,3006) are not handled. Image will not be displayed properly" );
    }
  // 3. VOILUTSequence (0028,3010)
  bool voilut = ds.FindDataElement(Tag(0x0028,0x3010) );
  if( voilut )
    {
    gdcmWarningMacro( "VOI LUT (0028,3010) are not handled. Image will not be displayed properly" );
    }
  // (0028,0120) US 32767                                    #   2, 1 PixelPaddingValue
  bool pixelpaddingvalue = ds.FindDataElement(Tag(0x0028,0x0120));

  // PS 3.3 - 2008 / C.7.5.1.1.2 Pixel Padding Value and Pixel Padding Range Limit
  if(pixelpaddingvalue)
    {
    // Technically if Pixel Padding Value is 0 on MONOCHROME2 image, then appearance should be fine...
    bool vizissue = true;
    if( pf.GetPixelRepresentation() == 0 )
      {
      Element<VR::US,VM::VM1> ppv;
      if( !ds.GetDataElement(Tag(0x0028,0x0120) ).IsEmpty() )
        {
        ppv.SetFromDataElement( ds.GetDataElement(Tag(0x0028,0x0120)) ); //.GetValue() );
        if( pi == PhotometricInterpretation::MONOCHROME2 && ppv.GetValue() == 0 )
          {
          vizissue = false;
          }
        }
      }
    else if( pf.GetPixelRepresentation() == 1 )
      {
      gdcmDebugMacro( "TODO" );
      }
    // test if there is any viz issue:
    if( vizissue )
      {
      gdcmDebugMacro( "Pixel Padding Value (0028,0120) is not handled. Image will not be displayed properly" );
      }
    }
  // 4. Palette Color Lookup Table Descriptor
  if ( pi == PhotometricInterpretation::PALETTE_COLOR )
    {
    //const DataElement& modlutsq = ds.GetDataElement( Tag(0x0028,0x3000) );
    //const SequenceOfItems* sq = modlutsq.GetSequenceOfItems();
    //SequenceOfItems::ConstIterator it = sq->Begin();
    //const DataSet &ds = it->GetNestedDataSet();

    SmartPointer<LookupTable> lut = new LookupTable;
    const Tag testseglut(0x0028, (0x1221 + 0));
    if( ds.FindDataElement( testseglut ) )
      {
      lut = new SegmentedPaletteColorLookupTable;
      }
    //SmartPointer<SegmentedPaletteColorLookupTable> lut = new SegmentedPaletteColorLookupTable;
    lut->Allocate( pf.GetBitsAllocated() );

    // for each red, green, blue:
    for(int i=0; i<3; ++i)
      {
      // (0028,1101) US 0\0\16
      // (0028,1102) US 0\0\16
      // (0028,1103) US 0\0\16
      const Tag tdescriptor(0x0028, (0x1101 + i));
      //const Tag tdescriptor(0x0028, 0x3002);
      Element<VR::US,VM::VM3> el_us3 = {};
      // Now pass the byte array to a DICOMizer:
      el_us3.SetFromDataElement( ds[tdescriptor] ); //.GetValue() );
      lut->InitializeLUT( LookupTable::LookupTableType(i),
        el_us3[0], el_us3[1], el_us3[2] );

      // (0028,1201) OW
      // (0028,1202) OW
      // (0028,1203) OW
      const Tag tlut(0x0028, (0x1201 + i));
      //const Tag tlut(0x0028, 0x3006);

      // Segmented LUT
      // (0028,1221) OW
      // (0028,1222) OW
      // (0028,1223) OW
      const Tag seglut(0x0028, (0x1221 + i));
      if( ds.FindDataElement( tlut ) )
        {
        const ByteValue *lut_raw = ds.GetDataElement( tlut ).GetByteValue();
        if( lut_raw )
          {
          // LookupTableType::RED == 0
          lut->SetLUT( LookupTable::LookupTableType(i),
            (unsigned char*)lut_raw->GetPointer(), lut_raw->GetLength() );
          //assert( pf.GetBitsAllocated() == el_us3.GetValue(2) );
          }
        else
          {
          lut->Clear();
          }

        unsigned long check =
          (el_us3.GetValue(0) ? el_us3.GetValue(0) : 65536)
          * el_us3.GetValue(2) / 8;
        assert( !lut->Initialized() || check == lut_raw->GetLength() ); (void)check;
        }
      else if( ds.FindDataElement( seglut ) )
        {
        const ByteValue *lut_raw = ds.GetDataElement( seglut ).GetByteValue();
        if( lut_raw )
          {
          lut->SetLUT( LookupTable::LookupTableType(i),
            (unsigned char*)lut_raw->GetPointer(), lut_raw->GetLength() );
          //assert( pf.GetBitsAllocated() == el_us3.GetValue(2) );
          }
        else
          {
          lut->Clear();
          }

        //unsigned long check =
        //  (el_us3.GetValue(0) ? el_us3.GetValue(0) : 65536)
         // * el_us3.GetValue(2) / 8;
        //assert( check == lut_raw->GetLength() ); (void)check;
        }
      else
        {
        assert(0);
        }
      }
    if( ! lut->Initialized() ) return false;
    PixelData->SetLUT(*lut);
    }
  // TODO
  //assert( pi.GetSamplesPerPixel() == pf.GetSamplesPerPixel() );

  // 5.5 Do IconImage if any
  assert( PixelData->GetIconImage().IsEmpty() );
  DoIconImage(ds, *PixelData);

  // 6. Do the Curves if any
  DoCurves(ds, *PixelData);

  // 7. Do the Overlays if any
  DoOverlays(ds, *PixelData);

  // 8. Do the PixelData
  if( ms == MediaStorage::MRSpectroscopyStorage )
    {
    const Tag spectdata = Tag(0x5600, 0x0020);
    if( !ds.FindDataElement( spectdata ) )
      {
      gdcmWarningMacro( "No Spectroscopy Data Found" );
      return false;
      }
    const DataElement& xde = ds.GetDataElement( spectdata );
    //bool need = PixelData->GetTransferSyntax() == TransferSyntax::ImplicitVRBigEndianPrivateGE;
    //PixelData->SetNeedByteSwap( need );
    PixelData->SetDataElement( xde );
    }
  else
    {
    const Tag pixeldata = Tag(0x7fe0, 0x0010);
    if( !ds.FindDataElement( pixeldata ) )
      {
      gdcmWarningMacro( "No Pixel Data Found" );
      return false;
      }
    const DataElement& xde = ds.GetDataElement( pixeldata );
    bool need = PixelData->GetTransferSyntax() == TransferSyntax::ImplicitVRBigEndianPrivateGE;
    PixelData->SetNeedByteSwap( need );
    PixelData->SetDataElement( xde );

    // FIXME:
    // We should check that when PixelData is RAW that Col * Dim == PixelData->GetLength()
    //PixelFormat guesspf = PixelFormat->GuessPixelFormat();

    }

  const unsigned int *dims = PixelData->GetDimensions();
  if( dims[0] == 0 || dims[1] == 0 )
    {
    // Pseudo-declared JPEG SC image storage. Let's fix col/row/pf/pi
    gdcm::JPEGCodec jpeg;
    if( jpeg.CanDecode( PixelData->GetTransferSyntax() ) )
      {
      std::stringstream ss;
      const DataElement &de = PixelData->GetDataElement();
      //const ByteValue *bv = de.GetByteValue();
      const SequenceOfFragments *sqf = de.GetSequenceOfFragments();
      sqf->WriteBuffer( ss );
      //std::string s( bv->GetPointer(), bv->GetLength() );
      //is.str( s );
      gdcm::PixelFormat pf ( gdcm::PixelFormat::UINT8 ); // usual guess...
      jpeg.SetPixelFormat( pf );
      gdcm::TransferSyntax ts;
      bool b = jpeg.GetHeaderInfo( ss, ts );
      if( b )
        {
        std::vector<unsigned int> v(3);
        v[0] = PixelData->GetDimensions()[0];
        v[1] = PixelData->GetDimensions()[1];
        v[2] = PixelData->GetDimensions()[2];
        assert( jpeg.GetDimensions()[0] );
        assert( jpeg.GetDimensions()[1] );
        v[0] = jpeg.GetDimensions()[0];
        v[1] = jpeg.GetDimensions()[1];
        PixelData->SetDimensions( &v[0] );
        //PixelData->SetPixelFormat( jpeg.GetPixelFormat() );
        //PixelData->SetPhotometricInterpretation( jpeg.GetPhotometricInterpretation() );
        assert( PixelData->IsTransferSyntaxCompatible( ts ) );
        }
      else
        {
        gdcmDebugMacro( "Columns or Row was found to be 0. Cannot compute dimension." );
        return false;
        }
      }
    else
      {
      gdcmDebugMacro( "Columns or Row was found to be 0. Cannot compute dimension." );
      return false;
      }
    }

  PixelData->ComputeLossyFlag();

  return true;
}

bool PixmapReader::ReadACRNEMAImage()
{
  const DataSet &ds = F->GetDataSet();
  std::stringstream ss;
  std::string conversion;

  // Ok we have the dataset let's feed the Image (PixelData)
  // 1. First find how many dimensions there is:
  // D 0028|0005 [SS] [Image Dimensions (RET)] [2]
  const Tag timagedimensions = Tag(0x0028, 0x0005);
  if( ds.FindDataElement( timagedimensions ) )
    {
    const DataElement& de = ds.GetDataElement( timagedimensions );
    Attribute<0x0028,0x0005> at = { 0 };
    at.SetFromDataElement( de );
    assert( at.GetNumberOfValues() == 1 );
    unsigned short imagedimensions = at.GetValue();
    //assert( imagedimensions == ReadSSFromTag( timagedimensions, ss, conversion ) );
    if ( imagedimensions == 3 )
      {
      PixelData->SetNumberOfDimensions(3);
      // D 0028|0012 [US] [Planes] [262]
      const DataElement& de = ds.GetDataElement( Tag(0x0028, 0x0012) );
      Attribute<0x0028,0x0012> at = { 0 };
      at.SetFromDataElement( de );
      assert( at.GetNumberOfValues() == 1 );
      PixelData->SetDimension(2, at.GetValue() );
      //assert( at.GetValue() == ReadUSFromTag( Tag(0x0028, 0x0012), ss, conversion ) );
      }
    else if ( imagedimensions == 2 )
      {
      PixelData->SetNumberOfDimensions(2);
      }
    else
      {
      gdcmErrorMacro( "Unhandled Image Dimensions: " << imagedimensions );
      return false;
      }
    }
  else
    {
    gdcmWarningMacro( "Attempting a guess for the number of dimensions" );
    PixelData->SetNumberOfDimensions( 2 );
    }

  // 2. What are the col & rows:
  // D 0028|0011 [US] [Columns] [512]
    {
    //const DataElement& de = ds.GetDataElement( Tag(0x0028, 0x0011) );
    Attribute<0x0028,0x0011> at = { 0 };
    at.SetFromDataSet( ds );
    PixelData->SetDimension(0, at.GetValue() );
    //assert( at.GetValue() == ReadUSFromTag( Tag(0x0028, 0x0011), ss, conversion ) );
    }

  // D 0028|0010 [US] [Rows] [512]
    {
    //const DataElement& de = ds.GetDataElement( Tag(0x0028, 0x0010) );
    Attribute<0x0028,0x0010> at = { 0 };
    at.SetFromDataSet( ds );
    PixelData->SetDimension(1, at.GetValue() );
    //assert( at.GetValue() == ReadUSFromTag( Tag(0x0028, 0x0010), ss, conversion ) );
    }

  // This is the definition of an ACR NEMA image:
  // D 0008|0010 [LO] [Recognition Code (RET)] [ACR-NEMA 2.0]
  // LIBIDO compatible code:
  // D 0008|0010 [LO] [Recognition Code (RET)] [ACRNEMA_LIBIDO_1.1]
  const Tag trecognitioncode(0x0008,0x0010);
  if( ds.FindDataElement( trecognitioncode ) && !ds.GetDataElement( trecognitioncode ).IsEmpty() )
    {
    const ByteValue *libido = ds.GetDataElement(trecognitioncode).GetByteValue();
    assert( libido );
    std::string libido_str( libido->GetPointer(), libido->GetLength() );
    assert( libido_str != "CANRME_AILIBOD1_1." );
    if( strcmp(libido_str.c_str() , "ACRNEMA_LIBIDO_1.1") == 0 || strcmp(libido_str.c_str() , "ACRNEMA_LIBIDO_1.0") == 0 )
      {
      // Swap Columns & Rows
      // assert( PixelData->GetNumberOfDimensions() == 2 );
      const unsigned int *dims = PixelData->GetDimensions();
      unsigned int tmp = dims[0];
      PixelData->SetDimension(0, dims[1] );
      PixelData->SetDimension(1, tmp );
      }
    else
      {
      assert( libido_str == "ACR-NEMA 2.0"
           || libido_str == "ACR-NEMA 1.0" );
      }
    }
  else
    {
    gdcmWarningMacro(
      "Reading as ACR NEMA an image which does not look likes ACR NEMA" );
    // File: acc-max.dcm is it ACR or DICOM ?
    // assert(0);
    }

  // 3. Pixel Format ?
  PixelFormat pf;
  // D 0028|0100 [US] [Bits Allocated] [16]
    {
    //const DataElement& de = ds.GetDataElement( Tag(0x0028, 0x0100) );
    Attribute<0x0028,0x0100> at = { 0 };
    at.SetFromDataSet( ds );
    pf.SetBitsAllocated( at.GetValue() );
    //assert( at.GetValue() == ReadUSFromTag( Tag(0x0028, 0x0100), ss, conversion ) );
    }

  // D 0028|0101 [US] [Bits Stored] [12]
    {
    //const DataElement& de = ds.GetDataElement( Tag(0x0028, 0x0101) );
    Attribute<0x0028,0x0101> at = { 0 };
    at.SetFromDataSet( ds );
    pf.SetBitsStored( at.GetValue() );
    //assert( at.GetValue() == ReadUSFromTag( Tag(0x0028, 0x0101), ss, conversion ) );
    }

  // D 0028|0102 [US] [High Bit] [11]
    {
    //const DataElement& de = ds.GetDataElement( Tag(0x0028, 0x0102) );
    Attribute<0x0028,0x0102> at = { 0 };
    at.SetFromDataSet( ds );
    pf.SetHighBit( at.GetValue() );
    //assert( at.GetValue() == ReadUSFromTag( Tag(0x0028, 0x0102), ss, conversion ) );
    }

  // D 0028|0103 [US] [Pixel Representation] [0]
    {
    //const DataElement& de = ds.GetDataElement( Tag(0x0028, 0x0103) );
    Attribute<0x0028,0x0103> at = { 0 };
    at.SetFromDataSet( ds );
    pf.SetPixelRepresentation( at.GetValue() );
    //assert( at.GetValue() == ReadUSFromTag( Tag(0x0028, 0x0103), ss, conversion ) );
    }

  PixelData->SetPixelFormat( pf );

  // 4. Do the Curves/Overlays if any
  DoCurves(ds, *PixelData);
  DoOverlays(ds, *PixelData);

  // 5. Do the PixelData
  const Tag pixeldata = Tag(0x7fe0, 0x0010);
  if( !ds.FindDataElement( pixeldata ) )
    {
    gdcmWarningMacro( "No Pixel Data Found" );
    return false;
    }
  const DataElement& de = ds.GetDataElement( pixeldata );
  if ( de.GetVR() == VR::OW )
    {
    //assert(0);
    //PixelData->SetNeedByteSwap(true);
    }
  PixelData->SetDataElement( de );

  // There is no such thing as Photometric Interpretation and
  // Planar Configuration in ACR NEMA so let's default to something ...
  PixelData->SetPhotometricInterpretation(
    PhotometricInterpretation::MONOCHROME2 );
  PixelData->SetPlanarConfiguration(0);
  const Tag planarconfiguration(0x0028, 0x0006);
  if( ds.FindDataElement( planarconfiguration ) && !ds.GetDataElement( planarconfiguration ).IsEmpty() )
    {
    //const DataElement& de = ds.GetDataElement( planarconfiguration );
    Attribute<0x0028,0x0006> at = { 0 };
    at.SetFromDataSet( ds );

    //unsigned int pc = ReadUSFromTag( planarconfiguration, ss, conversion );
    unsigned int pc = at.GetValue();
    if( pc && PixelData->GetPixelFormat().GetSamplesPerPixel() != 3 )
      {
      gdcmDebugMacro( "Cannot have PlanarConfiguration=1, when Sample Per Pixel != 3" );
      pc = 0;
      }
    PixelData->SetPlanarConfiguration( pc );
    }

  const Tag tphotometricinterpretation(0x0028, 0x0004);
  // Some funny ACR NEMA file have PhotometricInterpretation ...
  if( ds.FindDataElement( tphotometricinterpretation ) && !ds.GetDataElement( tphotometricinterpretation ).IsEmpty() )
    {
    const ByteValue *photometricinterpretation
      = ds.GetDataElement( tphotometricinterpretation ).GetByteValue();
    assert( photometricinterpretation );
    std::string photometricinterpretation_str(
      photometricinterpretation->GetPointer(),
      photometricinterpretation->GetLength() );
    PhotometricInterpretation pi(
      PhotometricInterpretation::GetPIType(
        photometricinterpretation_str.c_str()));
    }
  else
    {
    // Wild guess:
    if( PixelData->GetPixelFormat().GetSamplesPerPixel() == 1 )
      {
      assert( PixelData->GetPhotometricInterpretation() == PhotometricInterpretation::MONOCHROME2 );
      // No need...
      //PixelData->SetPhotometricInterpretation( PhotometricInterpretation::MONOCHROME2 );
      }
    else if( PixelData->GetPixelFormat().GetSamplesPerPixel() == 3 )
      {
      // LIBIDO-24-ACR_NEMA-Rectangle.dcm
      PixelData->SetPhotometricInterpretation( PhotometricInterpretation::RGB );
      }
    else if( PixelData->GetPixelFormat().GetSamplesPerPixel() == 4 )
      {
      PixelData->SetPhotometricInterpretation( PhotometricInterpretation::ARGB );
      }
    else
      {
      gdcmErrorMacro( "Cannot handle Samples Per Pixel=" << PixelData->GetPixelFormat().GetSamplesPerPixel() );
      return false;
      }
    }

  return true;
}


} // end namespace gdcm
