/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmPixelConvert.cxx
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

//////////////////   TEMPORARY NOTE
// look for "fixMem" and convert that to a member of this class
// Removing the prefix fixMem and dealing with allocations should do the trick
//
// grep PIXELCONVERT everywhere and clean up !

#include "gdcmDebug.h"
#include "gdcmPixelConvert.h"
#include <fstream>
#include <stdio.h>

namespace gdcm
{
#define str2num(str, typeNum) *((typeNum *)(str))

// For JPEG 2000, body in file gdcmJpeg2000.cxx
bool gdcm_read_JPEG2000_file (std::ifstream* fp, void* image_buffer);

// For JPEG 8 Bits, body in file gdcmJpeg8.cxx
bool gdcm_read_JPEG_file8    (std::ifstream* fp, void* image_buffer);

// For JPEG 12 Bits, body in file gdcmJpeg12.cxx
bool gdcm_read_JPEG_file12   (std::ifstream* fp, void* image_buffer);

// For JPEG 16 Bits, body in file gdcmJpeg16.cxx
// Beware this is misleading there is no 16bits DCT algorithm, only
// jpeg lossless compression exist in 16bits.
bool gdcm_read_JPEG_file16   (std::ifstream* fp, void* image_buffer);


//-----------------------------------------------------------------------------
// Constructor / Destructor
PixelConvert::PixelConvert() 
{
   RGB = 0;
   RGBSize = 0;
   Decompressed = 0;
   DecompressedSize = 0;
   LutRGBA = 0;
   LutRedData = 0;
   LutGreenData = 0;
   LutBlueData =0;
}

void PixelConvert::Squeeze() 
{
   if ( RGB )
   {
      delete [] RGB;
   } 
   if ( Decompressed )
   {
      delete [] Decompressed;
   }
   if ( LutRGBA )
   {
      delete [] LutRGBA;
   }
}

PixelConvert::~PixelConvert() 
{
   Squeeze();
}

void PixelConvert::AllocateRGB()
{
  if ( RGB ) {
     delete [] RGB;
  }
  RGB = new uint8_t[ RGBSize ];
}

void PixelConvert::AllocateDecompressed()
{
  if ( Decompressed ) {
     delete [] Decompressed;
  }
  Decompressed = new uint8_t[ DecompressedSize ];
}

/**
 * \brief Read from file a 12 bits per pixel image and decompress it
 *        into a 16 bits per pixel image.
 */
void PixelConvert::ReadAndDecompress12BitsTo16Bits( std::ifstream* fp )
               throw ( FormatError )
{
   int nbPixels = XSize * YSize;
   uint16_t* localDecompres = (uint16_t*)Decompressed;

   for( int p = 0; p < nbPixels; p += 2 )
   {
      uint8_t b0, b1, b2;

      fp->read( (char*)&b0, 1);
      if ( fp->fail() || fp->eof() )//Fp->gcount() == 1
      {
         throw FormatError( "PixelConvert::ReadAndDecompress12BitsTo16Bits()",
                                "Unfound first block" );
      }

      fp->read( (char*)&b1, 1 );
      if ( fp->fail() || fp->eof())//Fp->gcount() == 1
      {
         throw FormatError( "PixelConvert::ReadAndDecompress12BitsTo16Bits()",
                                "Unfound second block" );
      }

      fp->read( (char*)&b2, 1 );
      if ( fp->fail() || fp->eof())//Fp->gcount() == 1
      {
         throw FormatError( "PixelConvert::ReadAndDecompress12BitsTo16Bits()",
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
 * \brief     Try to deal with RLE 16 Bits. 
 *            We assume the RLE has allready been parsed and loaded in
 *            Decompressed (through \ref ReadAndDecompressJPEGFile ).
 *            We here need to make 16 Bits Pixels from Low Byte and
 *            High Byte 'Planes'...(for what it may mean)
 * @return    Boolean
 */
bool PixelConvert::DecompressRLE16BitsFromRLE8Bits( int NumberOfFrames )
{
   size_t PixelNumber = XSize * YSize;
   size_t decompressedSize = XSize * YSize * NumberOfFrames;

   // We assumed Decompressed contains the decoded RLE pixels but as
   // 8 bits per pixel. In order to convert those pixels to 16 bits
   // per pixel we cannot work in place within Decompressed and hence
   // we copy it in a safe place, say copyDecompressed.

   uint8_t* copyDecompressed = new uint8_t[ decompressedSize * 2 ];
   memmove( copyDecompressed, Decompressed, decompressedSize * 2 );

   uint8_t* x = Decompressed;
   uint8_t* a = copyDecompressed;
   uint8_t* b = a + PixelNumber;

   for ( int i = 0; i < NumberOfFrames; i++ )
   {
      for ( unsigned int j = 0; j < PixelNumber; j++ )
      {
         *(x++) = *(a++);
         *(x++) = *(b++);
      }
   }

   delete[] copyDecompressed;
      
   /// \todo check that operator new []didn't fail, and sometimes return false
   return true;
}

/**
 * \brief Implementation of the RLE decoding algorithm for decompressing
 *        a RLE fragment. [refer to PS 3.5-2003, section G.3.2 p 86]
 * @param subDecompressed Sub region of \ref Decompressed where the de
 *        decoded fragment should be placed.
 * @param fragmentSize The length of the binary fragment as found on the disk.
 * @param decompressedSegmentSize The expected length of the fragment ONCE
 *        decompressed.
 * @param fp File Pointer: on entry the position should be the one of
 *        the fragment to be decoded.
 */
bool PixelConvert::ReadAndDecompressRLEFragment( uint8_t* subDecompressed,
                                                 long fragmentSize,
                                                 long decompressedSegmentSize,
                                                 std::ifstream* fp )
{
   int8_t count;
   long numberOfOutputBytes = 0;
   long numberOfReadBytes = 0;

   while( numberOfOutputBytes < decompressedSegmentSize )
   {
      fp->read( (char*)&count, 1 );
      numberOfReadBytes += 1;
      if ( count >= 0 )
      // Note: count <= 127 comparison is always true due to limited range
      //       of data type int8_t [since the maximum of an exact width
      //       signed integer of width N is 2^(N-1) - 1, which for int8_t
      //       is 127].
      {
         fp->read( (char*)subDecompressed, count + 1);
         numberOfReadBytes   += count + 1;
         subDecompressed     += count + 1;
         numberOfOutputBytes += count + 1;
      }
      else
      {
         if ( ( count <= -1 ) && ( count >= -127 ) )
         {
            int8_t newByte;
            fp->read( (char*)&newByte, 1);
            numberOfReadBytes += 1;
            for( int i = 0; i < -count + 1; i++ )
            {
               subDecompressed[i] = newByte;
            }
            subDecompressed     += -count + 1;
            numberOfOutputBytes += -count + 1;
         }
      }
      // if count = 128 output nothing
                                                                                
      if ( numberOfReadBytes > fragmentSize )
      {
         dbg.Verbose(0, "PixelConvert::ReadAndDecompressRLEFragment: we "
                        "read more bytes than the segment size.");
         return false;
      }
   }
   return true;
}

/**
 * \brief     Reads from disk the Pixel Data of 'Run Length Encoded'
 *            Dicom encapsulated file and decompress it.
 * @param     fp already open File Pointer
 *            at which the pixel data should be copied
 * @return    Boolean
 */
bool PixelConvert::ReadAndDecompressRLEFile( std::ifstream* fp )
{
   uint8_t* subDecompressed = Decompressed;
   long decompressedSegmentSize = XSize * YSize;

   // Loop on the frame[s]
   for( RLEFramesInfo::RLEFrameList::iterator
        it  = RLEInfo->Frames.begin();
        it != RLEInfo->Frames.end();
      ++it )
   {
      // Loop on the fragments
      for( unsigned int k = 1; k <= (*it)->NumberFragments; k++ )
      {
         fp->seekg(  (*it)->Offset[k] , std::ios::beg );
         (void)ReadAndDecompressRLEFragment( subDecompressed,
                                             (*it)->Length[k],
                                             decompressedSegmentSize, 
                                             fp );
         subDecompressed += decompressedSegmentSize;
      }
   }

   if ( BitsAllocated == 16 )
   {
      // Try to deal with RLE 16 Bits
      (void)DecompressRLE16BitsFromRLE8Bits( ZSize );
   }

   return true;
}

/**
 * \brief Swap the bytes, according to \ref SwapCode.
 */
void PixelConvert::ConvertSwapZone()
{
   unsigned int i;

   if( BitsAllocated == 16 )
   {
      uint16_t* im16 = (uint16_t*)Decompressed;
      switch( SwapCode )
      {
         case 0:
         case 12:
         case 1234:
            break;
         case 21:
         case 3412:
         case 2143:
         case 4321:
            for( i = 0; i < DecompressedSize / 2; i++ )
            {
               im16[i]= (im16[i] >> 8) | (im16[i] << 8 );
            }
            break;
         default:
            dbg.Verbose( 0, "PixelConvert::ConvertSwapZone: SwapCode value "
                            "(16 bits) not allowed." );
      }
   }
   else if( BitsAllocated == 32 )
   {
      uint32_t s32;
      uint16_t high;
      uint16_t low;
      uint32_t* im32 = (uint32_t*)Decompressed;
      switch ( SwapCode )
      {
         case 0:
         case 1234:
            break;
         case 4321:
            for( i = 0; i < DecompressedSize / 4; i++ )
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
            for( i = 0; i < DecompressedSize / 4; i++ )
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
            for( i = 0; i < DecompressedSize / 4; i++ )
            {
               low     = im32[i] & 0x0000ffff; // 3412
               high    = im32[i] >> 16;
               s32     = low;
               im32[i] = ( s32 << 16 ) | high;
            }
            break;
         default:
            dbg.Verbose( 0, "PixelConvert::ConvertSwapZone: SwapCode value "
                            "(32 bits) not allowed." );
      }
   }
}

/**
 * \brief Deal with endianity i.e. re-arange bytes inside the integer
 */
void PixelConvert::ConvertReorderEndianity()
{
   if ( BitsAllocated != 8 )
   {
      ConvertSwapZone();
   }

   // Special kludge in order to deal with xmedcon broken images:
   if (  ( BitsAllocated == 16 )
       && ( BitsStored < BitsAllocated )
       && ( ! PixelSign ) )
   {
      int l = (int)( DecompressedSize / ( BitsAllocated / 8 ) );
      uint16_t *deb = (uint16_t *)Decompressed;
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
 * \brief     Reads from disk the Pixel Data of JPEG Dicom encapsulated
 &            file and decompress it.
 * @param     fp File Pointer
 * @return    Boolean
 */
bool PixelConvert::ReadAndDecompressJPEGFile( std::ifstream* fp )
{
   uint8_t* localDecompressed = Decompressed;
   // Loop on the fragment[s]
   for( JPEGFragmentsInfo::JPEGFragmentsList::iterator
        it  = JPEGInfo->Fragments.begin();
        it != JPEGInfo->Fragments.end();
      ++it )
   {
      fp->seekg( (*it)->Offset, std::ios::beg);

      if ( IsJPEG2000 )
      {
         if ( ! gdcm_read_JPEG2000_file( fp,localDecompressed ) )
         {
            return false;
         }
      }
      else if ( BitsStored == 8)
      {
         // JPEG Lossy : call to IJG 6b
         if ( ! gdcm_read_JPEG_file8( fp, localDecompressed ) )
         {
            return false;
         }
      }
      else if ( BitsStored == 12)
      {
         // Reading Fragment pixels
         if ( ! gdcm_read_JPEG_file12 ( fp, localDecompressed ) )
         {
            return false;
         }
      }
      else if ( BitsStored == 16)
      {
         // Reading Fragment pixels
         if ( ! gdcm_read_JPEG_file16 ( fp, localDecompressed ) )
         {
            return false;
         }
         //assert( IsJPEGLossless );
      }
      else
      {
         // other JPEG lossy not supported
         dbg.Error("PixelConvert::ReadAndDecompressJPEGFile: unknown "
                   "jpeg lossy compression ");
         return false;
      }

      // Advance to next free location in Decompressed 
      // for next fragment decompression (if any)
      int length = XSize * YSize * SamplesPerPixel;
      int numberBytes = BitsAllocated / 8;

      localDecompressed += length * numberBytes;
   }
   return true;
}

/**
 * \brief  Re-arrange the bits within the bytes.
 * @return Boolean
 */
bool PixelConvert::ConvertReArrangeBits() throw ( FormatError )
{
   if ( BitsStored != BitsAllocated )
   {
      int l = (int)( DecompressedSize / ( BitsAllocated / 8 ) );
      if ( BitsAllocated == 16 )
      {
         uint16_t mask = 0xffff;
         mask = mask >> ( BitsAllocated - BitsStored );
         uint16_t* deb = (uint16_t*)Decompressed;
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
         uint32_t* deb = (uint32_t*)Decompressed;
         for(int i = 0; i<l; i++)
         {
            *deb = (*deb >> (BitsStored - HighBitPosition - 1)) & mask;
            deb++;
         }
      }
      else
      {
         dbg.Verbose(0, "PixelConvert::ConvertReArrangeBits: weird image");
         throw FormatError( "PixelConvert::ConvertReArrangeBits()",
                                "weird image !?" );
      }
   }
   return true;
}

/**
 * \brief   Convert (Y plane, cB plane, cR plane) to RGB pixels
 * \warning Works on all the frames at a time
 */
void PixelConvert::ConvertYcBcRPlanesToRGBPixels()
{
   uint8_t* localDecompressed = Decompressed;
   uint8_t* copyDecompressed = new uint8_t[ DecompressedSize ];
   memmove( copyDecompressed, localDecompressed, DecompressedSize );

   // to see the tricks about YBR_FULL, YBR_FULL_422,
   // YBR_PARTIAL_422, YBR_ICT, YBR_RCT have a look at :
   // ftp://medical.nema.org/medical/dicom/final/sup61_ft.pdf
   // and be *very* affraid
   //
   int l        = XSize * YSize;
   int nbFrames = ZSize;

   uint8_t* a = copyDecompressed;
   uint8_t* b = copyDecompressed + l;
   uint8_t* c = copyDecompressed + l + l;
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

         *(localDecompressed++) = (uint8_t)R;
         *(localDecompressed++) = (uint8_t)G;
         *(localDecompressed++) = (uint8_t)B;
         a++;
         b++;
         c++;
      }
   }
   delete[] copyDecompressed;
}

/**
 * \brief   Convert (Red plane, Green plane, Blue plane) to RGB pixels
 * \warning Works on all the frames at a time
 */
void PixelConvert::ConvertRGBPlanesToRGBPixels()
{
   uint8_t* localDecompressed = Decompressed;
   uint8_t* copyDecompressed = new uint8_t[ DecompressedSize ];
   memmove( copyDecompressed, localDecompressed, DecompressedSize );

   int l = XSize * YSize * ZSize;

   uint8_t* a = copyDecompressed;
   uint8_t* b = copyDecompressed + l;
   uint8_t* c = copyDecompressed + l + l;

   for (int j = 0; j < l; j++)
   {
      *(localDecompressed++) = *(a++);
      *(localDecompressed++) = *(b++);
      *(localDecompressed++) = *(c++);
   }
   delete[] copyDecompressed;
}

bool PixelConvert::ReadAndDecompressPixelData( std::ifstream* fp )
{
   ComputeDecompressedAndRGBSizes();
   AllocateDecompressed();
   //////////////////////////////////////////////////
   //// First stage: get our hands on the Pixel Data.
   if ( !fp )
   {
     dbg.Verbose( 0, "PixelConvert::ReadAndDecompressPixelData: "
                     "unavailable file pointer." );
      return false;
   }

   fp->seekg( PixelOffset, std::ios::beg );
   if( fp->fail() || fp->eof()) //Fp->gcount() == 1
   {
     dbg.Verbose( 0, "PixelConvert::ReadAndDecompressPixelData: "
                     "unable to find PixelOffset in file." );
      return false;
   }

   //////////////////////////////////////////////////
   //// Second stage: read from disk dans decompress.
   if ( BitsAllocated == 12 )
   {
      ReadAndDecompress12BitsTo16Bits( fp);
   }
   else if ( IsDecompressed )
   {
      fp->read( (char*)Decompressed, PixelDataLength);
      if ( fp->fail() || fp->eof())//Fp->gcount() == 1
      {
         dbg.Verbose( 0, "PixelConvert::ReadAndDecompressPixelData: "
                         "reading of decompressed pixel data failed." );
         return false;
      }
   } 
   else if ( IsRLELossless )
   {
      if ( ! ReadAndDecompressRLEFile( fp ) )
      {
         dbg.Verbose( 0, "PixelConvert::ReadAndDecompressPixelData: "
                         "RLE decompressor failed." );
         return false;
      }
   }
   else
   {
      // Default case concerns JPEG family
      if ( ! ReadAndDecompressJPEGFile( fp ) )
      {
         dbg.Verbose( 0, "PixelConvert::ReadAndDecompressPixelData: "
                         "JPEG decompressor failed." );
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

void PixelConvert::ConvertHandleColor()
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
   // Hence gdcm shall use the folowing "heuristic" in order to be tolerant
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

   if ( ! IsDecompressedRGB() )
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

/**
 * \brief Predicate to know wether the image[s] (once decompressed) is RGB.
 * \note See comments of \ref ConvertHandleColor
 */
bool PixelConvert::IsDecompressedRGB()
{
   if (   IsMonochrome
       || PlanarConfiguration == 2
       || IsPaletteColor )
   {
      return false;
   }
   return true;
}

void PixelConvert::ComputeDecompressedAndRGBSizes()
{
   int bitsAllocated = BitsAllocated;
   // Number of "Bits Allocated" is fixed to 16 when it's 12, since
   // in this case we will expand the image to 16 bits (see
   //    \ref ReadAndDecompress12BitsTo16Bits() )
   if (  BitsAllocated == 12 )
   {
      bitsAllocated = 16;
   }
                                                                                
   DecompressedSize =  XSize * YSize * ZSize
                     * ( bitsAllocated / 8 )
                     * SamplesPerPixel;
   if ( HasLUT )
   {
      RGBSize = 3 * DecompressedSize;
   }

}

void PixelConvert::GrabInformationsFromHeader( Header* header )
{
   // Just in case some access to a Header element requires disk access.
   // Note: gdcmDocument::Fp is leaved open after OpenFile.
   std::ifstream* fp = header->OpenFile();
   // Number of Bits Allocated for storing a Pixel is defaulted to 16
   // when absent from the header.
   BitsAllocated = header->GetBitsAllocated();
   if ( BitsAllocated == 0 )
   {
      BitsAllocated = 16;
   }

   // Number of "Bits Stored" defaulted to number of "Bits Allocated"
   // when absent from the header.
   BitsStored = header->GetBitsStored();
   if ( BitsStored == 0 )
   {
      BitsStored = BitsAllocated;
   }

   // High Bit Position
   HighBitPosition = header->GetHighBitPosition();
   if ( HighBitPosition == 0 )
   {
      HighBitPosition = BitsAllocated - 1;
   }

   XSize = header->GetXSize();
   YSize = header->GetYSize();
   ZSize = header->GetZSize();
   SamplesPerPixel = header->GetSamplesPerPixel();
   PixelSize = header->GetPixelSize();
   PixelSign = header->IsSignedPixelData();
   SwapCode  = header->GetSwapCode();
   TransferSyntaxType ts = header->GetTransferSyntax();
   IsDecompressed =
        ( ! header->IsDicomV3() )
     || ts == ImplicitVRLittleEndian
     || ts == ExplicitVRLittleEndian
     || ts == ExplicitVRBigEndian
     || ts == DeflatedExplicitVRLittleEndian;
   IsJPEG2000     = header->IsJPEG2000();
   IsJPEGLossless = header->IsJPEGLossless();
   IsRLELossless  =  ( ts == RLELossless );
   PixelOffset     = header->GetPixelOffset();
   PixelDataLength = header->GetPixelAreaLength();
   RLEInfo  = header->GetRLEInfo();
   JPEGInfo = header->GetJPEGInfo();
                                                                             
   PlanarConfiguration = header->GetPlanarConfiguration();
   IsMonochrome = header->IsMonochrome();
   IsPaletteColor = header->IsPaletteColor();
   IsYBRFull = header->IsYBRFull();

   /////////////////////////////////////////////////////////////////
   // LUT section:
   HasLUT = header->HasLUT();
   if ( HasLUT )
   {
      LutRedDescriptor   = header->GetEntryByNumber( 0x0028, 0x1101 );
      LutGreenDescriptor = header->GetEntryByNumber( 0x0028, 0x1102 );
      LutBlueDescriptor  = header->GetEntryByNumber( 0x0028, 0x1103 );
   
      // Depending on the value of Document::MAX_SIZE_LOAD_ELEMENT_VALUE
      // [ refer to invocation of Document::SetMaxSizeLoadEntry() in
      // Document::Document() ], the loading of the value (content) of a
      // [Bin|Val]Entry occurence migth have been hindered (read simply NOT
      // loaded). Hence, we first try to obtain the LUTs data from the header
      // and when this fails we read the LUTs data directely from disk.
      /// \todo Reading a [Bin|Val]Entry directly from disk is a kludge.
      ///       We should NOT bypass the [Bin|Val]Entry class. Instead
      ///       an access to an UNLOADED content of a [Bin|Val]Entry occurence
      ///       (e.g. BinEntry::GetBinArea()) should force disk access from
      ///       within the [Bin|Val]Entry class itself. The only problem
      ///       is that the [Bin|Val]Entry is unaware of the FILE* is was
      ///       parsed from. Fix that. FIXME.
   
      ////// Red round:
      LutRedData = (uint8_t*)header->GetEntryBinAreaByNumber( 0x0028, 0x1201 );
      if ( ! LutRedData )
      {
         // Read the Lut Data from disk
         DocEntry* lutRedDataEntry = header->GetDocEntryByNumber( 0x0028,
                                                                  0x1201 );
         LutRedData = new uint8_t[ lutRedDataEntry->GetLength() ];
         fp->seekg(  lutRedDataEntry->GetOffset() ,std::ios::beg );
         fp->read( (char*)LutRedData, (size_t)lutRedDataEntry->GetLength());
         if ( fp->fail() || fp->eof())//Fp->gcount() == 1
         {
            dbg.Verbose(0, "PixelConvert::GrabInformationsFromHeader: "
                            "unable to read red LUT data" );
            return;
         }
      }
   
      ////// Green round:
      LutGreenData = (uint8_t*)header->GetEntryBinAreaByNumber(0x0028, 0x1202 );
      if ( ! LutGreenData)
      {
         // Read the Lut Data from disk
         DocEntry* lutGreenDataEntry = header->GetDocEntryByNumber( 0x0028,
                                                                    0x1202 );
         LutGreenData = new uint8_t[ lutGreenDataEntry->GetLength() ];
         fp->seekg( lutGreenDataEntry->GetOffset() , std::ios::beg );
         fp->read( (char*)LutGreenData, (size_t)lutGreenDataEntry->GetLength() );
         if ( fp->fail() || fp->eof())//Fp->gcount() == 1
         {
            dbg.Verbose(0, "PixelConvert::GrabInformationsFromHeader: "
                           "unable to read green LUT data" );
            return;
         }
      }
                                                                                   
      ////// Blue round:
      LutBlueData = (uint8_t*)header->GetEntryBinAreaByNumber( 0x0028, 0x1203 );
      if ( ! LutBlueData )
      {
         // Read the Lut Data from disk
         DocEntry* lutBlueDataEntry  = header->GetDocEntryByNumber( 0x0028,
                                                                    0x1203 );
         LutBlueData = new uint8_t[ lutBlueDataEntry->GetLength() ];
         fp->seekg(  lutBlueDataEntry->GetOffset() , std::ios::beg );
         fp->read( (char*)LutBlueData, (size_t)lutBlueDataEntry->GetLength() );
         if ( fp->fail() || fp->eof())//Fp->gcount() == 1
         {
            dbg.Verbose(0, "PixelConvert::GrabInformationsFromHeader: "
                           "unable to read blue LUT data" );
            return;
         }
      }
   }
                                                                                
   header->CloseFile();
}

/**
 * \brief Build Red/Green/Blue/Alpha LUT from Header
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
void PixelConvert::BuildLUTRGBA()
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
      dbg.Verbose(0, "PixelConvert::BuildLUTRGBA: wrong red LUT descriptor");
   }
                                                                                
   int lengthG;  // Green LUT length in Bytes
   int debG;     // Subscript of the first Lut Value
   int nbitsG;   // Lut item size (in Bits)
   nbRead = sscanf( LutGreenDescriptor.c_str(),
                    "%d\\%d\\%d",
                    &lengthG, &debG, &nbitsG );
   if( nbRead != 3 )
   {
      dbg.Verbose(0, "PixelConvert::BuildLUTRGBA: wrong green LUT descriptor");
   }
                                                                                
   int lengthB;  // Blue LUT length in Bytes
   int debB;     // Subscript of the first Lut Value
   int nbitsB;   // Lut item size (in Bits)
   nbRead = sscanf( LutRedDescriptor.c_str(),
                    "%d\\%d\\%d",
                    &lengthB, &debB, &nbitsB );
   if( nbRead != 3 )
   {
      dbg.Verbose(0, "PixelConvert::BuildLUTRGBA: wrong blue LUT descriptor");
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
   {
      return;
   }
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
   // We give up the checking to avoid some (useless ?)overhead
   // (optimistic asumption)
   int i;
   uint8_t* a = LutRGBA + 0;
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
 * \brief Build the RGB image from the Decompressed imagage and the LUTs.
 */
bool PixelConvert::BuildRGBImage()
{
   if ( RGB )
   {
      // The job is already done.
      return true;
   }

   if ( ! Decompressed )
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
   uint8_t* localRGB = RGB;
   for (size_t i = 0; i < DecompressedSize; ++i )
   {
      int j  = Decompressed[i] * 4;
      *localRGB++ = LutRGBA[j];
      *localRGB++ = LutRGBA[j+1];
      *localRGB++ = LutRGBA[j+2];
   }
   return true;
}

/**
 * \brief        Print self.
 * @param indent Indentation string to be prepended during printing.
 * @param os     Stream to print to.
 */
void PixelConvert::Print( std::string indent, std::ostream &os )
{
   os << indent
      << "--- Pixel information -------------------------"
      << std::endl;
   os << indent
      << "Pixel Data: offset " << PixelOffset
      << " x" << std::hex << PixelOffset << std::dec
      << "   length " << PixelDataLength
      << " x" << std::hex << PixelDataLength << std::dec
      << std::endl;

   if ( IsRLELossless )
   {
      if ( RLEInfo )
      {
         RLEInfo->Print( indent, os );
      }
      else
      {
         dbg.Verbose(0, "PixelConvert::Print: set as RLE file "
                        "but NO RLEinfo present.");
      }
   }

   if ( IsJPEG2000 || IsJPEGLossless )
   {
      if ( JPEGInfo )
      {
         JPEGInfo->Print( indent, os );
      }
      else
      {
         dbg.Verbose(0, "PixelConvert::Print: set as JPEG file "
                        "but NO JPEGinfo present.");
      }
   }
}

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
