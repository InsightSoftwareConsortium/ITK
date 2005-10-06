/*=========================================================================

  Program:   gdcm
  Module:    gdcmPixelReadConvert.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$
                                                                                
  Copyright (c) CREATIS (Centre de Recherche et d'Applications en Traitement de
  l'Image). All rights reserved. See Doc/License.txt or
  http://www.creatis.insa-lyon.fr/Public/Gdcm/License.html for details.
                                                                                
     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.
                                                                                
=========================================================================*/

#include "gdcmDebug.h"
#include "gdcmFile.h"
#include "gdcmGlobal.h"
#include "gdcmTS.h"
#include "gdcmPixelReadConvert.h"
#include "gdcmDocEntry.h"
#include "gdcmRLEFramesInfo.h"
#include "gdcmJPEGFragmentsInfo.h"

#include "gdcmUtil.h"

#include <fstream>
#include <stdio.h> //for sscanf

namespace gdcm
{
//-----------------------------------------------------------------------------
#define str2num(str, typeNum) *((typeNum *)(str))

//-----------------------------------------------------------------------------
// Constructor / Destructor
/// Constructor
PixelReadConvert::PixelReadConvert() 
{
   RGB          = 0;
   RGBSize      = 0;
   Raw          = 0;
   RawSize      = 0;
   LutRGBA      = 0;
   LutRedData   = 0;
   LutGreenData = 0;
   LutBlueData  = 0;
   IsDLXGE      = false;
}

/// Canonical Destructor
PixelReadConvert::~PixelReadConvert() 
{
   Squeeze();
}

//-----------------------------------------------------------------------------
// Public
/**
 * \brief Predicate to know whether the image[s] (once Raw) is RGB.
 * \note See comments of \ref ConvertHandleColor
 */
bool PixelReadConvert::IsRawRGB()
{
   if (   IsMonochrome
       || PlanarConfiguration == 2
       || IsPaletteColor )
   {
      return false;
   }
   return true;
}
/**
 * \brief Gets various usefull informations from the file header
 * @param file gdcm::File pointer
 */
void PixelReadConvert::GrabInformationsFromFile( File *file )
{
   // Number of Bits Allocated for storing a Pixel is defaulted to 16
   // when absent from the file.
   BitsAllocated = file->GetBitsAllocated();
   if ( BitsAllocated == 0 )
   {
      BitsAllocated = 16;
   }

   // Number of "Bits Stored", defaulted to number of "Bits Allocated"
   // when absent from the file.
   BitsStored = file->GetBitsStored();
   if ( BitsStored == 0 )
   {
      BitsStored = BitsAllocated;
   }

   // High Bit Position, defaulted to "Bits Allocated" - 1
   HighBitPosition = file->GetHighBitPosition();
   if ( HighBitPosition == 0 )
   {
      HighBitPosition = BitsAllocated - 1;
   }

   XSize           = file->GetXSize();
   YSize           = file->GetYSize();
   ZSize           = file->GetZSize();
   SamplesPerPixel = file->GetSamplesPerPixel();
   PixelSize       = file->GetPixelSize();
   PixelSign       = file->IsSignedPixelData();
   SwapCode        = file->GetSwapCode();

   std::string ts  = file->GetTransferSyntax();
   IsRaw =
        ( ! file->IsDicomV3() )
     || Global::GetTS()->GetSpecialTransferSyntax(ts) == TS::ImplicitVRLittleEndian
     || Global::GetTS()->GetSpecialTransferSyntax(ts) == TS::ImplicitVRLittleEndianDLXGE
     || Global::GetTS()->GetSpecialTransferSyntax(ts) == TS::ExplicitVRLittleEndian
     || Global::GetTS()->GetSpecialTransferSyntax(ts) == TS::ExplicitVRBigEndian
     || Global::GetTS()->GetSpecialTransferSyntax(ts) == TS::DeflatedExplicitVRLittleEndian;

   // cache whether this is a strange GE transfer syntax (which uses
   // a little endian transfer syntax for the header and a big endian
   // transfer syntax for the pixel data).
   if (file->IsDicomV3())
   {
     IsDLXGE = Global::GetTS()->GetSpecialTransferSyntax(ts) == TS::ImplicitVRLittleEndianDLXGE;
   }
   else
   {
    IsDLXGE = false;
   }
   IsJPEG2000      = Global::GetTS()->IsJPEG2000(ts);
   IsJPEGLS        = Global::GetTS()->IsJPEGLS(ts);
   IsJPEGLossy     = Global::GetTS()->IsJPEGLossy(ts);
   IsJPEGLossless  = Global::GetTS()->IsJPEGLossless(ts);
   IsRLELossless   = Global::GetTS()->IsRLELossless(ts);

   PixelOffset     = file->GetPixelOffset();
   PixelDataLength = file->GetPixelAreaLength();
   RLEInfo  = file->GetRLEInfo();
   JPEGInfo = file->GetJPEGInfo();

   PlanarConfiguration = file->GetPlanarConfiguration();
   IsMonochrome = file->IsMonochrome();
   IsPaletteColor = file->IsPaletteColor();
   IsYBRFull = file->IsYBRFull();

   /////////////////////////////////////////////////////////////////
   // LUT section:
   HasLUT = file->HasLUT();
   if ( HasLUT )
   {
      // Just in case some access to a File element requires disk access.
      LutRedDescriptor   = file->GetEntryValue( 0x0028, 0x1101 );
      LutGreenDescriptor = file->GetEntryValue( 0x0028, 0x1102 );
      LutBlueDescriptor  = file->GetEntryValue( 0x0028, 0x1103 );
   
      // Depending on the value of Document::MAX_SIZE_LOAD_ELEMENT_VALUE
      // [ refer to invocation of Document::SetMaxSizeLoadEntry() in
      // Document::Document() ], the loading of the value (content) of a
      // [Bin|Val]Entry occurence migth have been hindered (read simply NOT
      // loaded). Hence, we first try to obtain the LUTs data from the file
      // and when this fails we read the LUTs data directly from disk.
      // \TODO Reading a [Bin|Val]Entry directly from disk is a kludge.
      //       We should NOT bypass the [Bin|Val]Entry class. Instead
      //       an access to an UNLOADED content of a [Bin|Val]Entry occurence
      //       (e.g. BinEntry::GetBinArea()) should force disk access from
      //       within the [Bin|Val]Entry class itself. The only problem
      //       is that the [Bin|Val]Entry is unaware of the FILE* is was
      //       parsed from. Fix that. FIXME.
   
      // //// Red round
      file->LoadEntryBinArea(0x0028, 0x1201);
      LutRedData = (uint8_t*)file->GetEntryBinArea( 0x0028, 0x1201 );
      if ( ! LutRedData )
      {
         gdcmWarningMacro( "Unable to read Red LUT data" );
      }

      // //// Green round:
      file->LoadEntryBinArea(0x0028, 0x1202);
      LutGreenData = (uint8_t*)file->GetEntryBinArea(0x0028, 0x1202 );
      if ( ! LutGreenData)
      {
         gdcmWarningMacro( "Unable to read Green LUT data" );
      }

      // //// Blue round:
      file->LoadEntryBinArea(0x0028, 0x1203);
      LutBlueData = (uint8_t*)file->GetEntryBinArea( 0x0028, 0x1203 );
      if ( ! LutBlueData )
      {
         gdcmWarningMacro( "Unable to read Blue LUT data" );
      }
   }

   ComputeRawAndRGBSizes();
}

/// \brief Reads from disk and decompresses Pixels
bool PixelReadConvert::ReadAndDecompressPixelData( std::ifstream *fp )
{
   // ComputeRawAndRGBSizes is already made by 
   // ::GrabInformationsFromfile. So, the structure sizes are
   // correct
   Squeeze();

   //////////////////////////////////////////////////
   //// First stage: get our hands on the Pixel Data.
   if ( !fp )
   {
      gdcmWarningMacro( "Unavailable file pointer." );
      return false;
   }

   fp->seekg( PixelOffset, std::ios::beg );
   if( fp->fail() || fp->eof())
   {
      gdcmWarningMacro( "Unable to find PixelOffset in file." );
      return false;
   }

   AllocateRaw();

   //////////////////////////////////////////////////
   //// Second stage: read from disk dans decompress.
   if ( BitsAllocated == 12 )
   {
      ReadAndDecompress12BitsTo16Bits( fp);
   }
   else if ( IsRaw )
   {
      // This problem can be found when some obvious informations are found
      // after the field containing the image data. In this case, these
      // bad data are added to the size of the image (in the PixelDataLength
      // variable). But RawSize is the right size of the image !
      if( PixelDataLength != RawSize)
      {
         gdcmWarningMacro( "Mismatch between PixelReadConvert : "
                            << PixelDataLength << " and RawSize : " << RawSize );
      }
      if( PixelDataLength > RawSize)
      {
         fp->read( (char*)Raw, RawSize);
      }
      else
      {
         fp->read( (char*)Raw, PixelDataLength);
      }

      if ( fp->fail() || fp->eof())
      {
         gdcmWarningMacro( "Reading of Raw pixel data failed." );
         return false;
      }
   } 
   else if ( IsRLELossless )
   {
      if ( ! RLEInfo->DecompressRLEFile( fp, Raw, XSize, YSize, ZSize, BitsAllocated ) )
      {
         gdcmWarningMacro( "RLE decompressor failed." );
         return false;
      }
   }
   else
   {
      // Default case concerns JPEG family
      if ( ! ReadAndDecompressJPEGFile( fp ) )
      {
         gdcmWarningMacro( "JPEG decompressor failed." );
         return false;
      }
   }

   ////////////////////////////////////////////
   //// Third stage: twigle the bytes and bits.
   ConvertReorderEndianity();
   ConvertReArrangeBits();
   ConvertHandleColor();

   return true;
}

/// Deletes Pixels Area
void PixelReadConvert::Squeeze() 
{
   if ( RGB )
      delete [] RGB;
   RGB = 0;

   if ( Raw )
      delete [] Raw;
   Raw = 0;

   if ( LutRGBA )
      delete [] LutRGBA;
   LutRGBA = 0;
}

/**
 * \brief Build the RGB image from the Raw imagage and the LUTs.
 */
bool PixelReadConvert::BuildRGBImage()
{
   if ( RGB )
   {
      // The job is already done.
      return true;
   }

   if ( ! Raw )
   {
      // The job can't be done
      return false;
   }

   BuildLUTRGBA();
   if ( ! LutRGBA )
   {
      // The job can't be done
      return false;
   }
                                                                                
   // Build RGB Pixels
   AllocateRGB();
   uint8_t *localRGB = RGB;
   for (size_t i = 0; i < RawSize; ++i )
   {
      int j  = Raw[i] * 4;
      *localRGB++ = LutRGBA[j];
      *localRGB++ = LutRGBA[j+1];
      *localRGB++ = LutRGBA[j+2];
   }
   return true;
}

//-----------------------------------------------------------------------------
// Protected

//-----------------------------------------------------------------------------
// Private
/**
 * \brief Read from file a 12 bits per pixel image and decompress it
 *        into a 16 bits per pixel image.
 */
void PixelReadConvert::ReadAndDecompress12BitsTo16Bits( std::ifstream *fp )
               throw ( FormatError )
{
   int nbPixels = XSize * YSize;
   uint16_t *localDecompres = (uint16_t*)Raw;

   for( int p = 0; p < nbPixels; p += 2 )
   {
      uint8_t b0, b1, b2;

      fp->read( (char*)&b0, 1);
      if ( fp->fail() || fp->eof() )
      {
         throw FormatError( "PixelReadConvert::ReadAndDecompress12BitsTo16Bits()",
                                "Unfound first block" );
      }

      fp->read( (char*)&b1, 1 );
      if ( fp->fail() || fp->eof())
      {
         throw FormatError( "PixelReadConvert::ReadAndDecompress12BitsTo16Bits()",
                                "Unfound second block" );
      }

      fp->read( (char*)&b2, 1 );
      if ( fp->fail() || fp->eof())
      {
         throw FormatError( "PixelReadConvert::ReadAndDecompress12BitsTo16Bits()",
                                "Unfound second block" );
      }

      // Two steps are necessary to please VC++
      //
      // 2 pixels 12bit =     [0xABCDEF]
      // 2 pixels 16bit = [0x0ABD] + [0x0FCE]
      //                        A                     B                 D
      *localDecompres++ =  ((b0 >> 4) << 8) + ((b0 & 0x0f) << 4) + (b1 & 0x0f);
      //                        F                     C                 E
      *localDecompres++ =  ((b2 & 0x0f) << 8) + ((b1 >> 4) << 4) + (b2 >> 4);

      /// \todo JPR Troubles expected on Big-Endian processors ?
   }
}

/**
 * \brief     Reads from disk the Pixel Data of JPEG Dicom encapsulated
 *            file and decompress it.
 * @param     fp File Pointer
 * @return    Boolean
 */
bool PixelReadConvert::ReadAndDecompressJPEGFile( std::ifstream *fp )
{
   if ( IsJPEG2000 )
   {
      gdcmWarningMacro( "Sorry, JPEG2000 not yet taken into account" );
      fp->seekg( JPEGInfo->GetFirstFragment()->GetOffset(), std::ios::beg);
//    if ( ! gdcm_read_JPEG2000_file( fp,Raw ) )
          return false;
   }

   if ( IsJPEGLS )
   {
      gdcmWarningMacro( "Sorry, JPEG-LS not yet taken into account" );
      fp->seekg( JPEGInfo->GetFirstFragment()->GetOffset(), std::ios::beg);
//    if ( ! gdcm_read_JPEGLS_file( fp,Raw ) )
         return false;
   }

   // else ??
   // Precompute the offset localRaw will be shifted with
   int length = XSize * YSize * SamplesPerPixel;
   int numberBytes = BitsAllocated / 8;

   JPEGInfo->DecompressFromFile(fp, Raw, BitsStored, numberBytes, length );
   return true;
}

/**
 * \brief Build Red/Green/Blue/Alpha LUT from File
 *         when (0028,0004),Photometric Interpretation = [PALETTE COLOR ]
 *          and (0028,1101),(0028,1102),(0028,1102)
 *            - xxx Palette Color Lookup Table Descriptor - are found
 *          and (0028,1201),(0028,1202),(0028,1202)
 *            - xxx Palette Color Lookup Table Data - are found
 * \warning does NOT deal with :
 *   0028 1100 Gray Lookup Table Descriptor (Retired)
 *   0028 1221 Segmented Red Palette Color Lookup Table Data
 *   0028 1222 Segmented Green Palette Color Lookup Table Data
 *   0028 1223 Segmented Blue Palette Color Lookup Table Data
 *   no known Dicom reader deals with them :-(
 * @return a RGBA Lookup Table
 */
void PixelReadConvert::BuildLUTRGBA()
{
   if ( LutRGBA )
   {
      return;
   }
   // Not so easy : see
   // http://www.barre.nom.fr/medical/dicom2/limitations.html#Color%20Lookup%20Tables
                                                                                
   if ( ! IsPaletteColor )
   {
      return;
   }
                                                                                
   if (   LutRedDescriptor   == GDCM_UNFOUND
       || LutGreenDescriptor == GDCM_UNFOUND
       || LutBlueDescriptor  == GDCM_UNFOUND )
   {
      return;
   }

   ////////////////////////////////////////////
   // Extract the info from the LUT descriptors
   int lengthR;   // Red LUT length in Bytes
   int debR;      // Subscript of the first Lut Value
   int nbitsR;    // Lut item size (in Bits)
   int nbRead = sscanf( LutRedDescriptor.c_str(),
                        "%d\\%d\\%d",
                        &lengthR, &debR, &nbitsR );
   if( nbRead != 3 )
   {
      gdcmWarningMacro( "Wrong Red LUT descriptor" );
   }
                                                                                
   int lengthG;  // Green LUT length in Bytes
   int debG;     // Subscript of the first Lut Value
   int nbitsG;   // Lut item size (in Bits)
   nbRead = sscanf( LutGreenDescriptor.c_str(),
                    "%d\\%d\\%d",
                    &lengthG, &debG, &nbitsG );
   if( nbRead != 3 )
   {
      gdcmWarningMacro( "Wrong Green LUT descriptor" );
   }
                                                                                
   int lengthB;  // Blue LUT length in Bytes
   int debB;     // Subscript of the first Lut Value
   int nbitsB;   // Lut item size (in Bits)
   nbRead = sscanf( LutRedDescriptor.c_str(),
                    "%d\\%d\\%d",
                    &lengthB, &debB, &nbitsB );
   if( nbRead != 3 )
   {
      gdcmWarningMacro( "Wrong Blue LUT descriptor" );
   }
                                                                                
   ////////////////////////////////////////////////////////
   if ( ( ! LutRedData ) || ( ! LutGreenData ) || ( ! LutBlueData ) )
   {
      return;
   }

   ////////////////////////////////////////////////
   // forge the 4 * 8 Bits Red/Green/Blue/Alpha LUT
   LutRGBA = new uint8_t[ 1024 ]; // 256 * 4 (R, G, B, Alpha)
   if ( !LutRGBA )
      return;

   memset( LutRGBA, 0, 1024 );
                                                                                
   int mult;
   if ( ( nbitsR == 16 ) && ( BitsAllocated == 8 ) )
   {
      // when LUT item size is different than pixel size
      mult = 2; // high byte must be = low byte
   }
   else
   {
      // See PS 3.3-2003 C.11.1.1.2 p 619
      mult = 1;
   }
                                                                                
   // if we get a black image, let's just remove the '+1'
   // from 'i*mult+1' and check again
   // if it works, we shall have to check the 3 Palettes
   // to see which byte is ==0 (first one, or second one)
   // and fix the code
   // We give up the checking to avoid some (useless ?) overhead
   // (optimistic asumption)
   int i;
   uint8_t *a = LutRGBA + 0;
   for( i=0; i < lengthR; ++i )
   {
      *a = LutRedData[i*mult+1];
      a += 4;
   }
                                                                                
   a = LutRGBA + 1;
   for( i=0; i < lengthG; ++i)
   {
      *a = LutGreenData[i*mult+1];
      a += 4;
   }
                                                                                
   a = LutRGBA + 2;
   for(i=0; i < lengthB; ++i)
   {
      *a = LutBlueData[i*mult+1];
      a += 4;
   }
                                                                                
   a = LutRGBA + 3;
   for(i=0; i < 256; ++i)
   {
      *a = 1; // Alpha component
      a += 4;
   }
}

/**
 * \brief Swap the bytes, according to \ref SwapCode.
 */
void PixelReadConvert::ConvertSwapZone()
{
   unsigned int i;

   // If this file is TS::ImplicitVRLittleEndianDLXGE, then the header
   // is in little endian format and the pixel data is in big endian
   // format.  When reading the header, GDCM has already established
   // a byte swapping code suitable for this machine to read the
   // header. In TS::ImplicitVRLittleEndianDLXGE, this code will need
   // to be switched in order to read the pixel data.  This must be
   // done REGARDLESS of the processor endianess!
   //
   // Example:  Assume we are on a little endian machine.  When
   // GDCM reads the header, the header will match the machine
   // endianess and the swap code will be established as a no-op.
   // When GDCM reaches the pixel data, it will need to switch the
   // swap code to do big endian to little endian conversion.
   //
   // Now, assume we are on a big endian machine.  When GDCM reads the
   // header, the header will be recognized as a different endianess
   // than the machine endianess, and a swap code will be established
   // to convert from little endian to big endian.  When GDCM readers
   // the pixel data, the pixel data endianess will now match the
   // machine endianess.  But we currently have a swap code that
   // converts from little endian to big endian.  In this case, we
   // need to switch the swap code to a no-op.
   //
   // Therefore, in either case, if the file is in
   // ImplicitVRLittleEndianDLXGE, then GDCM needs to switch the byte
   // swapping code when entering the pixel data.
   int tempSwapCode = SwapCode;
   if (IsDLXGE) {
     if (SwapCode == 1234) {
       tempSwapCode = 4321;
     }
     else if (SwapCode == 4321) {
       tempSwapCode = 1234;
     }
   }

   if( BitsAllocated == 16 )
   {
      uint16_t *im16 = (uint16_t*)Raw;
      switch( tempSwapCode )
      {
         case 1234:
            break;
         case 3412:
         case 2143:
         case 4321:
            for( i = 0; i < RawSize / 2; i++ )
            {
               im16[i]= (im16[i] >> 8) | (im16[i] << 8 );
            }
            break;
         default:
            gdcmWarningMacro("SwapCode value (16 bits) not allowed.");
      }
   }
   else if( BitsAllocated == 32 )
   {
      uint32_t s32;
      uint16_t high;
      uint16_t low;
      uint32_t *im32 = (uint32_t*)Raw;
      switch ( tempSwapCode )
      {
         case 1234:
            break;
         case 4321:
            for( i = 0; i < RawSize / 4; i++ )
            {
               low     = im32[i] & 0x0000ffff;  // 4321
               high    = im32[i] >> 16;
               high    = ( high >> 8 ) | ( high << 8 );
               low     = ( low  >> 8 ) | ( low  << 8 );
               s32     = low;
               im32[i] = ( s32 << 16 ) | high;
            }
            break;
         case 2143:
            for( i = 0; i < RawSize / 4; i++ )
            {
               low     = im32[i] & 0x0000ffff;   // 2143
               high    = im32[i] >> 16;
               high    = ( high >> 8 ) | ( high << 8 );
               low     = ( low  >> 8 ) | ( low  << 8 );
               s32     = high;
               im32[i] = ( s32 << 16 ) | low;
            }
            break;
         case 3412:
            for( i = 0; i < RawSize / 4; i++ )
            {
               low     = im32[i] & 0x0000ffff; // 3412
               high    = im32[i] >> 16;
               s32     = low;
               im32[i] = ( s32 << 16 ) | high;
            }
            break;
         default:
            gdcmWarningMacro("SwapCode value (32 bits) not allowed." );
      }
   }

}

/**
 * \brief Deal with endianness i.e. re-arange bytes inside the integer
 */
void PixelReadConvert::ConvertReorderEndianity()
{
   if ( BitsAllocated != 8 )
   {
      ConvertSwapZone();
   }

   // Special kludge in order to deal with xmedcon broken images:
   if ( BitsAllocated == 16
     && BitsStored < BitsAllocated
     && !PixelSign )
   {
      int l = (int)( RawSize / ( BitsAllocated / 8 ) );
      uint16_t *deb = (uint16_t *)Raw;
      for(int i = 0; i<l; i++)
      {
         if( *deb == 0xffff )
         {
           *deb = 0;
         }
         deb++;
      }
   }
}

/**
 * \brief  Re-arrange the bits within the bytes.
 * @return Boolean
 */
bool PixelReadConvert::ConvertReArrangeBits() throw ( FormatError )
{
   if ( BitsStored != BitsAllocated )
   {
      int l = (int)( RawSize / ( BitsAllocated / 8 ) );
      if ( BitsAllocated == 16 )
      {
         uint16_t mask = 0xffff;
         mask = mask >> ( BitsAllocated - BitsStored );
         uint16_t *deb = (uint16_t*)Raw;
         for(int i = 0; i<l; i++)
         {
            *deb = (*deb >> (BitsStored - HighBitPosition - 1)) & mask;
            deb++;
         }
      }
      else if ( BitsAllocated == 32 )
      {
         uint32_t mask = 0xffffffff;
         mask = mask >> ( BitsAllocated - BitsStored );
         uint32_t *deb = (uint32_t*)Raw;
         for(int i = 0; i<l; i++)
         {
            *deb = (*deb >> (BitsStored - HighBitPosition - 1)) & mask;
            deb++;
         }
      }
      else
      {
         gdcmWarningMacro("Weird image");
         throw FormatError( "Weird image !?" );
      }
   }
   return true;
}

/**
 * \brief   Convert (Red plane, Green plane, Blue plane) to RGB pixels
 * \warning Works on all the frames at a time
 */
void PixelReadConvert::ConvertRGBPlanesToRGBPixels()
{
   uint8_t *localRaw = Raw;
   uint8_t *copyRaw = new uint8_t[ RawSize ];
   memmove( copyRaw, localRaw, RawSize );

   int l = XSize * YSize * ZSize;

   uint8_t *a = copyRaw;
   uint8_t *b = copyRaw + l;
   uint8_t *c = copyRaw + l + l;

   for (int j = 0; j < l; j++)
   {
      *(localRaw++) = *(a++);
      *(localRaw++) = *(b++);
      *(localRaw++) = *(c++);
   }
   delete[] copyRaw;
}

/**
 * \brief   Convert (cY plane, cB plane, cR plane) to RGB pixels
 * \warning Works on all the frames at a time
 */
void PixelReadConvert::ConvertYcBcRPlanesToRGBPixels()
{
   uint8_t *localRaw = Raw;
   uint8_t *copyRaw = new uint8_t[ RawSize ];
   memmove( copyRaw, localRaw, RawSize );

   // to see the tricks about YBR_FULL, YBR_FULL_422,
   // YBR_PARTIAL_422, YBR_ICT, YBR_RCT have a look at :
   // ftp://medical.nema.org/medical/dicom/final/sup61_ft.pdf
   // and be *very* affraid
   //
   int l        = XSize * YSize;
   int nbFrames = ZSize;

   uint8_t *a = copyRaw;
   uint8_t *b = copyRaw + l;
   uint8_t *c = copyRaw + l + l;
   double R, G, B;

   /// \todo : Replace by the 'well known' integer computation
   ///         counterpart. Refer to
   ///            http://lestourtereaux.free.fr/papers/data/yuvrgb.pdf
   ///         for code optimisation.

   for ( int i = 0; i < nbFrames; i++ )
   {
      for ( int j = 0; j < l; j++ )
      {
         R = 1.164 *(*a-16) + 1.596 *(*c -128) + 0.5;
         G = 1.164 *(*a-16) - 0.813 *(*c -128) - 0.392 *(*b -128) + 0.5;
         B = 1.164 *(*a-16) + 2.017 *(*b -128) + 0.5;

         if (R < 0.0)   R = 0.0;
         if (G < 0.0)   G = 0.0;
         if (B < 0.0)   B = 0.0;
         if (R > 255.0) R = 255.0;
         if (G > 255.0) G = 255.0;
         if (B > 255.0) B = 255.0;

         *(localRaw++) = (uint8_t)R;
         *(localRaw++) = (uint8_t)G;
         *(localRaw++) = (uint8_t)B;
         a++;
         b++;
         c++;
      }
   }
   delete[] copyRaw;
}

/// \brief Deals with the color decoding i.e. handle:
///   - R, G, B planes (as opposed to RGB pixels)
///   - YBR (various) encodings.
///   - LUT[s] (or "PALETTE COLOR").

void PixelReadConvert::ConvertHandleColor()
{
   //////////////////////////////////
   // Deal with the color decoding i.e. handle:
   //   - R, G, B planes (as opposed to RGB pixels)
   //   - YBR (various) encodings.
   //   - LUT[s] (or "PALETTE COLOR").
   //
   // The classification in the color decoding schema is based on the blending
   // of two Dicom tags values:
   // * "Photometric Interpretation" for which we have the cases:
   //  - [Photo A] MONOCHROME[1|2] pictures,
   //  - [Photo B] RGB or YBR_FULL_422 (which acts as RGB),
   //  - [Photo C] YBR_* (with the above exception of YBR_FULL_422)
   //  - [Photo D] "PALETTE COLOR" which indicates the presence of LUT[s].
   // * "Planar Configuration" for which we have the cases:
   //  - [Planar 0] 0 then Pixels are already RGB
   //  - [Planar 1] 1 then we have 3 planes : R, G, B,
   //  - [Planar 2] 2 then we have 1 gray Plane and 3 LUTs
   //
   // Now in theory, one could expect some coherence when blending the above
   // cases. For example we should not encounter files belonging at the
   // time to case [Planar 0] and case [Photo D].
   // Alas, this was only theory ! Because in practice some odd (read ill
   // formated Dicom) files (e.g. gdcmData/US-PAL-8-10x-echo.dcm) we encounter:
   //     - "Planar Configuration" = 0,
   //     - "Photometric Interpretation" = "PALETTE COLOR".
   // Hence gdcm will use the folowing "heuristic" in order to be tolerant
   // towards Dicom-non-conformance files:
   //   << whatever the "Planar Configuration" value might be, a
   //      "Photometric Interpretation" set to "PALETTE COLOR" forces
   //      a LUT intervention >>
   //
   // Now we are left with the following handling of the cases:
   // - [Planar 0] OR  [Photo A] no color decoding (since respectively
   //       Pixels are already RGB and monochrome pictures have no color :),
   // - [Planar 1] AND [Photo B] handled with ConvertRGBPlanesToRGBPixels()
   // - [Planar 1] AND [Photo C] handled with ConvertYcBcRPlanesToRGBPixels()
   // - [Planar 2] OR  [Photo D] requires LUT intervention.

   if ( ! IsRawRGB() )
   {
      // [Planar 2] OR  [Photo D]: LUT intervention done outside
      return;
   }
                                                                                
   if ( PlanarConfiguration == 1 )
   {
      if ( IsYBRFull )
      {
         // [Planar 1] AND [Photo C] (remember YBR_FULL_422 acts as RGB)
         ConvertYcBcRPlanesToRGBPixels();
      }
      else
      {
         // [Planar 1] AND [Photo C]
         ConvertRGBPlanesToRGBPixels();
      }
      return;
   }
                                                                                
   // When planarConf is 0, and RLELossless (forbidden by Dicom norm)
   // pixels need to be RGB-fied anyway
   if (IsRLELossless)
   {
     ConvertRGBPlanesToRGBPixels();
   }
   // In *normal *case, when planarConf is 0, pixels are already in RGB
}

/// Computes the Pixels Size
void PixelReadConvert::ComputeRawAndRGBSizes()
{
   int bitsAllocated = BitsAllocated;
   // Number of "Bits Allocated" is fixed to 16 when it's 12, since
   // in this case we will expand the image to 16 bits (see
   //    \ref ReadAndDecompress12BitsTo16Bits() )
   if (  BitsAllocated == 12 )
   {
      bitsAllocated = 16;
   }
                                                                                
   RawSize =  XSize * YSize * ZSize
                     * ( bitsAllocated / 8 )
                     * SamplesPerPixel;
   if ( HasLUT )
   {
      RGBSize = 3 * RawSize;
   }
   else
   {
      RGBSize = RawSize;
   }
}

/// Allocates room for RGB Pixels
void PixelReadConvert::AllocateRGB()
{
  if ( RGB )
     delete [] RGB;
  RGB = new uint8_t[RGBSize];
}

/// Allocates room for RAW Pixels
void PixelReadConvert::AllocateRaw()
{
  if ( Raw )
     delete [] Raw;
  Raw = new uint8_t[RawSize];
}

//-----------------------------------------------------------------------------
// Print
/**
 * \brief        Print self.
 * @param indent Indentation string to be prepended during printing.
 * @param os     Stream to print to.
 */
void PixelReadConvert::Print( std::ostream &os, std::string const &indent )
{
   os << indent
      << "--- Pixel information -------------------------"
      << std::endl;
   os << indent
      << "Pixel Data: offset " << PixelOffset
      << " x(" << std::hex << PixelOffset << std::dec
      << ")   length " << PixelDataLength
      << " x(" << std::hex << PixelDataLength << std::dec
      << ")" << std::endl;

   if ( IsRLELossless )
   {
      if ( RLEInfo )
      {
         RLEInfo->Print( os, indent );
      }
      else
      {
         gdcmWarningMacro("Set as RLE file but NO RLEinfo present.");
      }
   }

   if ( IsJPEG2000 || IsJPEGLossless || IsJPEGLossy || IsJPEGLS )
   {
      if ( JPEGInfo )
      {
         JPEGInfo->Print( os, indent );
      }
      else
      {
         gdcmWarningMacro("Set as JPEG file but NO JPEGinfo present.");
      }
   }
}

//-----------------------------------------------------------------------------
} // end namespace gdcm

// NOTES on File internal calls
// User
// ---> GetImageData
//     ---> GetImageDataIntoVector
//        |---> GetImageDataIntoVectorRaw
//        | lut intervention
// User
// ---> GetImageDataRaw
//     ---> GetImageDataIntoVectorRaw

