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

#include "gdcmPixelReadConvert.h"
#include "gdcmDebug.h"
#include "gdcmFile.h"
#include "gdcmGlobal.h"
#include "gdcmTS.h"
#include "gdcmDocEntry.h"
#include "gdcmRLEFramesInfo.h"
#include "gdcmJPEGFragmentsInfo.h"

#include <fstream>
#include <stdio.h> //for sscanf

#if defined(__BORLANDC__)
   #include <mem.h> // for memset
#endif 

namespace gdcm
{

//bool ReadMPEGFile (std::ifstream *fp, void *image_buffer, size_t lenght);
bool gdcm_read_JPEG2000_file (void* raw, 
                              char *inputdata, size_t inputlength);
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
   RLEInfo      = 0;
   JPEGInfo     = 0;
   UserFunction = 0;
   FileInternal = 0;
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
   //PixelSize       = file->GetPixelSize();  Useless
   PixelSign       = file->IsSignedPixelData();
   SwapCode        = file->GetSwapCode();

   IsPrivateGETransferSyntax = IsMPEG
             = IsJPEG2000 = IsJPEGLS = IsJPEGLossy  
             = IsJPEGLossless = IsRLELossless 
             = false;

   std::string ts  = file->GetTransferSyntax();
   IsRaw =
        ( ! file->IsDicomV3() )
     || Global::GetTS()->GetSpecialTransferSyntax(ts) == TS::ImplicitVRLittleEndian
     || Global::GetTS()->GetSpecialTransferSyntax(ts) == TS::ImplicitVRBigEndianPrivateGE
     || Global::GetTS()->GetSpecialTransferSyntax(ts) == TS::ExplicitVRLittleEndian
     || Global::GetTS()->GetSpecialTransferSyntax(ts) == TS::ExplicitVRBigEndian
     || Global::GetTS()->GetSpecialTransferSyntax(ts) == TS::DeflatedExplicitVRLittleEndian;

   IsMPEG          = Global::GetTS()->IsMPEG(ts);
   IsJPEG2000      = Global::GetTS()->IsJPEG2000(ts);
   IsJPEGLS        = Global::GetTS()->IsJPEGLS(ts);
   IsJPEGLossy     = Global::GetTS()->IsJPEGLossy(ts);
   IsJPEGLossless  = Global::GetTS()->IsJPEGLossless(ts);
   IsRLELossless   = Global::GetTS()->IsRLELossless(ts);

   PixelOffset     = file->GetPixelOffset();
   PixelDataLength = file->GetPixelAreaLength();
   RLEInfo         = file->GetRLEInfo();
   JPEGInfo        = file->GetJPEGInfo();

   IsMonochrome    = file->IsMonochrome();
   IsMonochrome1   = file->IsMonochrome1();
   IsPaletteColor  = file->IsPaletteColor();
   IsYBRFull       = file->IsYBRFull();

   PlanarConfiguration = file->GetPlanarConfiguration();

   /////////////////////////////////////////////////////////////////
   // LUT section:
   HasLUT = file->HasLUT();
   if ( HasLUT )
   {
      // Just in case some access to a File element requires disk access.
      LutRedDescriptor   = file->GetEntryValue( 0x0028, 0x1101 );
      LutGreenDescriptor = file->GetEntryValue( 0x0028, 0x1102 );
      LutBlueDescriptor  = file->GetEntryValue( 0x0028, 0x1103 );
   
      // FIXME : The following comment is probabely meaningless, since LUT are *always*
      // loaded at parsing time, whatever their length is.
         
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
         gdcmWarningMacro("Unable to read Red Palette Color Lookup Table data");
      }

      // //// Green round:
      file->LoadEntryBinArea(0x0028, 0x1202);
      LutGreenData = (uint8_t*)file->GetEntryBinArea(0x0028, 0x1202 );
      if ( ! LutGreenData)
      {
         gdcmWarningMacro("Unable to read Green Palette Color Lookup Table data");
      }

      // //// Blue round:
      file->LoadEntryBinArea(0x0028, 0x1203);
      LutBlueData = (uint8_t*)file->GetEntryBinArea( 0x0028, 0x1203 );
      if ( ! LutBlueData )
      {
         gdcmWarningMacro("Unable to read Blue Palette Color Lookup Table data");
      }
   }
   FileInternal = file;   

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
   if ( fp->fail() || fp->eof() )
   {
      gdcmWarningMacro( "Unable to find PixelOffset in file." );
      return false;
   }

   AllocateRaw();

   //////////////////////////////////////////////////
   //// Second stage: read from disk and decompress.
   if ( BitsAllocated == 12 ) // We suppose 'BitsAllocated' = 12 only exist for uncompressed files
   {
      ReadAndDecompress12BitsTo16Bits( fp);
   }
   else if ( IsRaw )
   {
      // This problem can be found when some obvious informations are found
      // after the field containing the image data. In this case, these
      // bad data are added to the size of the image (in the PixelDataLength
      // variable). But RawSize is the right size of the image !
      if ( PixelDataLength != RawSize )
      {
         gdcmWarningMacro( "Mismatch between PixelReadConvert : "
                            << PixelDataLength << " and RawSize : " << RawSize );
      }
      if ( PixelDataLength > RawSize )
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
      if ( ! RLEInfo->DecompressRLEFile
                               ( fp, Raw, XSize, YSize, ZSize, BitsAllocated ) )
      {
         gdcmWarningMacro( "RLE decompressor failed." );
         return false;
      }
   }
   else if ( IsMPEG )
   {
      //gdcmWarningMacro( "Sorry, MPEG not yet taken into account" );
      //return false;
      // fp has already been seek to start of mpeg
      //ReadMPEGFile(fp, (char*)Raw, PixelDataLength); 
      return true;
   }
   else
   {
      // Default case concerns JPEG family
      if ( ! ReadAndDecompressJPEGFile( fp ) )
      {
         gdcmWarningMacro( "JPEG decompressor ( ReadAndDecompressJPEGFile()"
                              << " method ) failed." );
         return false;
      }
   }

   ////////////////////////////////////////////
   //// Third stage: twigle the bytes and bits.
   ConvertReorderEndianity();
   ConvertReArrangeBits();
   ConvertFixGreyLevels();
   if (UserFunction) // user is allowed to Mirror, TopDown, Rotate,...the image
      UserFunction( Raw, FileInternal);
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
 * \brief Build the RGB image from the Raw image and the LUTs.
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

   gdcmDebugMacro( "--> BuildRGBImage" );
                                                                                
   // Build RGB Pixels
   AllocateRGB();
   
   int j;
   if ( BitsAllocated <= 8 )
   {
      uint8_t *localRGB = RGB;
      for (size_t i = 0; i < RawSize; ++i )
      {
         j  = Raw[i] * 4;
         *localRGB++ = LutRGBA[j];
         *localRGB++ = LutRGBA[j+1];
         *localRGB++ = LutRGBA[j+2];
      }
    }
 
    else  // deal with 16 bits pixels and 16 bits Palette color
    {
      uint16_t *localRGB = (uint16_t *)RGB;
      for (size_t i = 0; i < RawSize/2; ++i )
      {
         j  = ((uint16_t *)Raw)[i] * 4;
         *localRGB++ = ((uint16_t *)LutRGBA)[j];
         *localRGB++ = ((uint16_t *)LutRGBA)[j+1];
         *localRGB++ = ((uint16_t *)LutRGBA)[j+2];
      } 
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
     // make sure this is the right JPEG compression
     assert( !IsJPEGLossless || !IsJPEGLossy || !IsJPEGLS );
     // FIXME this is really ugly but it seems I have to load the complete
     // jpeg2000 stream to use jasper:
     // I don't think we'll ever be able to deal with multiple fragments properly

      unsigned long inputlength = 0;
      JPEGFragment *jpegfrag = JPEGInfo->GetFirstFragment();
      while( jpegfrag )
      {
         inputlength += jpegfrag->GetLength();
         jpegfrag = JPEGInfo->GetNextFragment();
      }
      gdcmAssertMacro( inputlength != 0);
      uint8_t *inputdata = new uint8_t[inputlength];
      char *pinputdata = (char*)inputdata;
      jpegfrag = JPEGInfo->GetFirstFragment();
      while( jpegfrag )
      {
         fp->seekg( jpegfrag->GetOffset(), std::ios::beg);
         fp->read(pinputdata, jpegfrag->GetLength());
         pinputdata += jpegfrag->GetLength();
         jpegfrag = JPEGInfo->GetNextFragment();
      }
      // Warning the inputdata buffer is delete in the function
      if ( ! gdcm_read_JPEG2000_file( Raw, 
          (char*)inputdata, inputlength ) )
      {
         return true;
      }
      // wow what happen, must be an error
      gdcmWarningMacro( "gdcm_read_JPEG2000_file() failed "); 
      return false;
   }
   else if ( IsJPEGLS )
   {
     // make sure this is the right JPEG compression
     assert( !IsJPEGLossless || !IsJPEGLossy || !IsJPEG2000 );
   // WARNING : JPEG-LS is NOT the 'classical' Jpeg Lossless : 
   // [JPEG-LS is the basis for new lossless/near-lossless compression
   // standard for continuous-tone images intended for JPEG2000. The standard
   // is based on the LOCO-I algorithm (LOw COmplexity LOssless COmpression
   // for Images) developed at Hewlett-Packard Laboratories]
   //
   // see http://datacompression.info/JPEGLS.shtml
   //
#if 0
   std::cerr << "count:" << JPEGInfo->GetFragmentCount() << std::endl;
      unsigned long inputlength = 0;
      JPEGFragment *jpegfrag = JPEGInfo->GetFirstFragment();
      while( jpegfrag )
      {
         inputlength += jpegfrag->GetLength();
         jpegfrag = JPEGInfo->GetNextFragment();
      }
      gdcmAssertMacro( inputlength != 0);
      uint8_t *inputdata = new uint8_t[inputlength];
      char *pinputdata = (char*)inputdata;
      jpegfrag = JPEGInfo->GetFirstFragment();
      while( jpegfrag )
      {
         fp->seekg( jpegfrag->GetOffset(), std::ios::beg);
         fp->read(pinputdata, jpegfrag->GetLength());
         pinputdata += jpegfrag->GetLength();
         jpegfrag = JPEGInfo->GetNextFragment();
      }  
      
  //fp->read((char*)Raw, PixelDataLength);

  std::ofstream out("/tmp/jpegls.jpg");
  out.write((char*)inputdata, inputlength);
  out.close();
  delete[] inputdata;
#endif

      gdcmWarningMacro( "Sorry, JPEG-LS not yet taken into account" );
      fp->seekg( JPEGInfo->GetFirstFragment()->GetOffset(), std::ios::beg);
//    if ( ! gdcm_read_JPEGLS_file( fp,Raw ) )
         return false;
   }
   else
   {
     // make sure this is the right JPEG compression
     assert( !IsJPEGLS || !IsJPEG2000 );
     // Precompute the offset localRaw will be shifted with
     int length = XSize * YSize * SamplesPerPixel;
     int numberBytes = BitsAllocated / 8;

     JPEGInfo->DecompressFromFile(fp, Raw, BitsStored, numberBytes, length );
     return true;
   }
}

/**
 * \brief Build Red/Green/Blue/Alpha LUT from File when :
 *         - (0028,0004) : Photometric Interpretation == [PALETTE COLOR ]
 *         and
 *         - (0028,1101),(0028,1102),(0028,1102)
 *            xxx Palette Color Lookup Table Descriptor are found
 *          and
 *         - (0028,1201),(0028,1202),(0028,1202)
 *           xxx Palette Color Lookup Table Data - are found
 * \warning does NOT deal with :
 *   - 0028 1100 Gray Lookup Table Descriptor (Retired)
 *   - 0028 1221 Segmented Red Palette Color Lookup Table Data
 *   - 0028 1222 Segmented Green Palette Color Lookup Table Data
 *   - 0028 1223 Segmented Blue Palette Color Lookup Table Data
 *   no known Dicom reader deals with them :-(
 * @return a RGBA Lookup Table
 */
void PixelReadConvert::BuildLUTRGBA()
{

   // Note to code reviewers :
   // The problem is *much more* complicated, since a lot of manufacturers
   // Don't follow the norm :
   // have a look at David Clunie's remark at the end of this .cxx file.
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
      gdcmWarningMacro( "(At least) a LUT Descriptor is missing" );
      return;
   }

   ////////////////////////////////////////////
   // Extract the info from the LUT descriptors
   int lengthR;   // Red LUT length in Bytes
   int debR;      // Subscript of the first Lut Value
   int nbitsR;    // Lut item size (in Bits)
   int nbRead;    // nb of items in LUT descriptor (must be = 3)

   nbRead = sscanf( LutRedDescriptor.c_str(),
                        "%d\\%d\\%d",
                        &lengthR, &debR, &nbitsR );
   if ( nbRead != 3 )
   {
      gdcmWarningMacro( "Wrong Red LUT descriptor" );
   }                                                                                
   int lengthG;  // Green LUT length in Bytes
   int debG;     // Subscript of the first Lut Value
   int nbitsG;   // Lut item size (in Bits)

   nbRead = sscanf( LutGreenDescriptor.c_str(),
                    "%d\\%d\\%d",
                    &lengthG, &debG, &nbitsG );  
   if ( nbRead != 3 )
   {
      gdcmWarningMacro( "Wrong Green LUT descriptor" );
   }
                                                                                
   int lengthB;  // Blue LUT length in Bytes
   int debB;     // Subscript of the first Lut Value
   int nbitsB;   // Lut item size (in Bits)
   nbRead = sscanf( LutRedDescriptor.c_str(),
                    "%d\\%d\\%d",
                    &lengthB, &debB, &nbitsB );
   if ( nbRead != 3 )
   {
      gdcmWarningMacro( "Wrong Blue LUT descriptor" );
   }
 
   gdcmDebugMacro(" lengthR " << lengthR << " debR " 
                << debR << " nbitsR " << nbitsR);
   gdcmDebugMacro(" lengthG " << lengthG << " debG " 
                << debG << " nbitsG " << nbitsG);
   gdcmDebugMacro(" lengthB " << lengthB << " debB " 
                << debB << " nbitsB " << nbitsB);

   if ( !lengthR ) // if = 2^16, this shall be 0 see : CP-143
      lengthR=65536;
   if ( !lengthG ) // if = 2^16, this shall be 0
      lengthG=65536;
   if ( !lengthB ) // if = 2^16, this shall be 0
      lengthB=65536; 
                                                                                
   ////////////////////////////////////////////////////////

   if ( ( ! LutRedData ) || ( ! LutGreenData ) || ( ! LutBlueData ) )
   {
      gdcmWarningMacro( "(At least) a LUT is missing" );
      return;
   }

   // -------------------------------------------------------------
   
   if ( BitsAllocated <= 8 )
   {
      // forge the 4 * 8 Bits Red/Green/Blue/Alpha LUT
      LutRGBA = new uint8_t[ 1024 ]; // 256 * 4 (R, G, B, Alpha)
      if ( !LutRGBA )
         return;
      LutItemNumber = 256;
      LutItemSize   = 8;
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
      uint8_t *a;

      //take "Subscript of the first Lut Value" (debR,debG,debB) into account!

      //FIXME :  +1 : to get 'low value' byte
      //         Trouble expected on Big Endian Processors ?
      //         16 BIts Per Pixel Palette Color to be swapped?

      a = LutRGBA + 0 + debR;
      for( i=0; i < lengthR; ++i )
      {
         *a = LutRedData[i*mult+1]; 
         a += 4;
      }
                                                                                
      a = LutRGBA + 1 + debG;
      for( i=0; i < lengthG; ++i)
      {
         *a = LutGreenData[i*mult+1];
         a += 4;
      }
                                                                                
      a = LutRGBA + 2 + debB;
      for(i=0; i < lengthB; ++i)
      {
         *a = LutBlueData[i*mult+1];
         a += 4;
      }
                                    
      a = LutRGBA + 3 ;
      for(i=0; i < 256; ++i)
      {
         *a = 1; // Alpha component
         a += 4;
      }
   }
   else
   {
      // Probabely the same stuff is to be done for 16 Bits Pixels
      // with 65536 entries LUT ?!?
      // Still looking for accurate info on the web :-(

      gdcmWarningMacro( "Sorry Palette Color Lookup Tables not yet dealt with"
                         << " for 16 Bits Per Pixel images" );

      // forge the 4 * 16 Bits Red/Green/Blue/Alpha LUT

      LutRGBA = (uint8_t *)new uint16_t[ 65536*4 ]; // 2^16 * 4 (R, G, B, Alpha)
      if ( !LutRGBA )
         return;
      memset( LutRGBA, 0, 65536*4*2 );  // 16 bits = 2 bytes ;-)

      LutItemNumber = 65536;
      LutItemSize   = 16;

      int i;
      uint16_t *a16;

      //take "Subscript of the first Lut Value" (debR,debG,debB) into account!

      a16 = (uint16_t*)LutRGBA + 0 + debR;
      for( i=0; i < lengthR; ++i )
      {
         *a16 = ((uint16_t*)LutRedData)[i];
         a16 += 4;
      }
                                                                              
      a16 = (uint16_t*)LutRGBA + 1 + debG;
      for( i=0; i < lengthG; ++i)
      {
         *a16 = ((uint16_t*)LutGreenData)[i];
         a16 += 4;
      }
                                                                                
      a16 = (uint16_t*)LutRGBA + 2 + debB;
      for(i=0; i < lengthB; ++i)
      {
         *a16 = ((uint16_t*)LutBlueData)[i];
         a16 += 4;
      }
                                                                             
      a16 = (uint16_t*)LutRGBA + 3 ;
      for(i=0; i < 65536; ++i)
      {
         *a16 = 1; // Alpha component
         a16 += 4;
      }
/* Just to 'see' the LUT, at debug time
// Don't remove this commented out code.

      a16=(uint16_t*)LutRGBA;
      for (int j=0;j<65536;j++)
      {
         std::cout << *a16     << " " << *(a16+1) << " "
                   << *(a16+2) << " " << *(a16+3) << std::endl;
         a16+=4;
      }
*/
   }
}

/**
 * \brief Swap the bytes, according to \ref SwapCode.
 */
void PixelReadConvert::ConvertSwapZone()
{
   unsigned int i;

   // If this file is 'ImplicitVR BigEndian PrivateGE Transfer Syntax', 
   // then the header is in little endian format and the pixel data is in 
   // big endian format.  When reading the header, GDCM has already established
   // a byte swapping code suitable for this machine to read the
   // header. In TS::ImplicitVRBigEndianPrivateGE, this code will need
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
   // 'ImplicitVR BigEndian PrivateGE Transfer Syntax', then GDCM needs to switch
   // the byte swapping code when entering the pixel data.
   
   int tempSwapCode = SwapCode;
   if ( IsPrivateGETransferSyntax )
   {
      gdcmWarningMacro(" IsPrivateGETransferSyntax found; turn the SwapCode"); 
      // PrivateGETransferSyntax only exists for 'true' Dicom images
      // we assume there is no 'exotic' 32 bits endianess!
      if (SwapCode == 1234) 
      {
         tempSwapCode = 4321;
      }
      else if (SwapCode == 4321)
      {
         tempSwapCode = 1234;
      }
   }
    
   if ( BitsAllocated == 16 )
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
            gdcmWarningMacro("SwapCode value (16 bits) not allowed." 
                        << tempSwapCode);
      }
   }
   else if ( BitsAllocated == 32 )
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
            gdcmWarningMacro("SwapCode value (32 bits) not allowed." << tempSwapCode );
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
         if ( *deb == 0xffff )
         {
           *deb = 0;
         }
         deb++;
      }
   }
}

/**
 * \brief Deal with Grey levels i.e. re-arange them
 *        to have low values = dark, high values = bright
 */
void PixelReadConvert::ConvertFixGreyLevels()
{
   if (!IsMonochrome1)
      return;

   uint32_t i; // to please M$VC6
   int16_t j;

   if (!PixelSign)
   {
      if ( BitsAllocated == 8 )
      {
         uint8_t *deb = (uint8_t *)Raw;
         for (i=0; i<RawSize; i++)      
         {
            *deb = 255 - *deb;
            deb++;
         }
         return;
      }

      if ( BitsAllocated == 16 )
      {
         uint16_t mask =1;
         for (j=0; j<BitsStored-1; j++)
         {
            mask = (mask << 1) +1; // will be fff when BitsStored=12
         }

         uint16_t *deb = (uint16_t *)Raw;
         for (i=0; i<RawSize/2; i++)      
         {
            *deb = mask - *deb;
            deb++;
         }
         return;
       }
   }
   else
   {
      if ( BitsAllocated == 8 )
      {
         uint8_t smask8 = 255;
         uint8_t *deb = (uint8_t *)Raw;
         for (i=0; i<RawSize; i++)      
         {
            *deb = smask8 - *deb;
            deb++;
         }
         return;
      }
      if ( BitsAllocated == 16 )
      {
         uint16_t smask16 = 65535;
         uint16_t *deb = (uint16_t *)Raw;
         for (i=0; i<RawSize/2; i++)      
         {
            *deb = smask16 - *deb;
            deb++;
         }
         return;
      }
   }
}

/**
 * \brief  Re-arrange the bits within the bytes.
 * @return Boolean always true
 */
bool PixelReadConvert::ConvertReArrangeBits() throw ( FormatError )
{

   if ( BitsStored != BitsAllocated )
   {
      int l = (int)( RawSize / ( BitsAllocated / 8 ) );
      if ( BitsAllocated == 16 )
      {
         // pmask : to mask the 'unused bits' (may contain overlays)
         uint16_t pmask = 0xffff;
         pmask = pmask >> ( BitsAllocated - BitsStored );

         uint16_t *deb = (uint16_t*)Raw;

         if ( !PixelSign )  // Pixels are unsigned
         {
            for(int i = 0; i<l; i++)
            {   
               *deb = (*deb >> (BitsStored - HighBitPosition - 1)) & pmask;
               deb++;
            }
         }
         else // Pixels are signed
         {
            // smask : to check the 'sign' when BitsStored != BitsAllocated
            uint16_t smask = 0x0001;
            smask = smask << ( 16 - (BitsAllocated - BitsStored + 1) );
            // nmask : to propagate sign bit on negative values
            int16_t nmask = (int16_t)0x8000;  
            nmask = nmask >> ( BitsAllocated - BitsStored - 1 );
 
            for(int i = 0; i<l; i++)
            {
               *deb = *deb >> (BitsStored - HighBitPosition - 1);
               if ( *deb & smask )
               {
                  *deb = *deb | nmask;
               }
               else
               {
                  *deb = *deb & pmask;
               }
               deb++;
            }
         }
      }
      else if ( BitsAllocated == 32 )
      {
         // pmask : to mask the 'unused bits' (may contain overlays)
         uint32_t pmask = 0xffffffff;
         pmask = pmask >> ( BitsAllocated - BitsStored );

         uint32_t *deb = (uint32_t*)Raw;

         if ( !PixelSign )
         {
            for(int i = 0; i<l; i++)
            {             
               *deb = (*deb >> (BitsStored - HighBitPosition - 1)) & pmask;
               deb++;
            }
         }
         else
         {
            // smask : to check the 'sign' when BitsStored != BitsAllocated
            uint32_t smask = 0x00000001;
            smask = smask >> ( 32 - (BitsAllocated - BitsStored +1 ));
            // nmask : to propagate sign bit on negative values
            int32_t nmask = 0x80000000;   
            nmask = nmask >> ( BitsAllocated - BitsStored -1 );

            for(int i = 0; i<l; i++)
            {
               *deb = *deb >> (BitsStored - HighBitPosition - 1);
               if ( *deb & smask )
                  *deb = *deb | nmask;
               else
                  *deb = *deb & pmask;
               deb++;
            }
         }
      }
      else
      {
         gdcmWarningMacro("Weird image (BitsAllocated !=8, 12, 16, 32)");
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
   gdcmWarningMacro("--> ConvertRGBPlanesToRGBPixels");

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
  // Remarks for YBR newbees :
  // YBR_FULL works very much like RGB, i.e. three samples per pixel, 
  // just the color space is YCbCr instead of RGB. This is particularly useful
  // for doppler ultrasound where most of the image is grayscale 
  // (i.e. only populates the Y components) and Cb and Cr are mostly zero,
  // except for the few patches of color on the image.
  // On such images, RLE achieves a compression ratio that is much better 
  // than the compression ratio on an equivalent RGB image. 
 
   gdcmWarningMacro("--> ConvertYcBcRPlanesToRGBPixels");
   
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

   uint8_t *a = copyRaw + 0;
   uint8_t *b = copyRaw + l;
   uint8_t *c = copyRaw + l+ l;
   int32_t R, G, B;

   ///  We replaced easy to understand but time consuming floating point
   ///  computations by the 'well known' integer computation counterpart
   ///  Refer to :
   ///            http://lestourtereaux.free.fr/papers/data/yuvrgb.pdf
   ///  for code optimisation.

   for ( int i = 0; i < nbFrames; i++ )
   {
      for ( int j = 0; j < l; j++ )
      {
         R = 38142 *(*a-16) + 52298 *(*c -128);
         G = 38142 *(*a-16) - 26640 *(*c -128) - 12845 *(*b -128);
         B = 38142 *(*a-16) + 66093 *(*b -128);

         R = (R+16384)>>15;
         G = (G+16384)>>15;
         B = (B+16384)>>15;

         if (R < 0)   R = 0;
         if (G < 0)   G = 0;
         if (B < 0)   B = 0;
         if (R > 255) R = 255;
         if (G > 255) G = 255;
         if (B > 255) B = 255;

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
   // towards Dicom-non-conformant files:
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

   gdcmDebugMacro("--> ConvertHandleColor "
                     << "Planar Configuration " << PlanarConfiguration );

   if ( ! IsRawRGB() )
   {
      // [Planar 2] OR  [Photo D]: LUT intervention done outside
      gdcmDebugMacro("--> RawRGB : LUT intervention done outside");
      return;
   }
                                                                                
   if ( PlanarConfiguration == 1 )
   {
      if ( IsYBRFull )
      {
         // [Planar 1] AND [Photo C] (remember YBR_FULL_422 acts as RGB)
         gdcmDebugMacro("--> YBRFull");
         ConvertYcBcRPlanesToRGBPixels();
      }
      else
      {
         // [Planar 1] AND [Photo C]
         gdcmDebugMacro("--> YBRFull");
         ConvertRGBPlanesToRGBPixels();
      }
      return;
   }
                                                                                
   // When planarConf is 0, and RLELossless (forbidden by Dicom norm)
   // pixels need to be RGB-fyied anyway

   if (IsRLELossless)
   { 
     gdcmDebugMacro("--> RLE Lossless");
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
      RGBSize = 3 * RawSize; // works for 8 and 16 bits per Pixel
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

// Note to developpers :
// Here is a very detailled post from David Clunie, on the troubles caused 
// 'non standard' LUT and LUT description
// We shall have to take it into accound in our code.
// Some day ...


/*
Subject: Problem with VOI LUTs in Agfa and Fuji CR and GE DX images, was Re: VOI LUT issues
Date: Sun, 06 Feb 2005 17:13:40 GMT
From: David Clunie <dclunie@dclunie.com>
Reply-To: dclunie@dclunie.com
Newsgroups: comp.protocols.dicom
References: <1107553502.040221.189550@o13g2000cwo.googlegroups.com>

> THE LUT that comes with [my] image claims to be 16-bit, but none of the
> values goes higher than 4095.  That being said, though, none of my
> original pixel values goes higher than that, either.  I have read
> elsewhere on this group that when that happens you are supposed to
> adjust the LUT.  Can someone be more specific?  There was a thread from
> 2002 where Marco and David were mentioning doing precisely that.
>
> Thanks
>
> -carlos rodriguez


You have encountered the well known "we know what the standard says but
we are going to ignore it and do what we have been doing for almost
a decade regardless" CR vendor bug. Agfa started this, but they are not
the only vendor doing this now; GE and Fuji may have joined the club.

Sadly, one needs to look at the LUT Data, figure out what the maximum
value actually encoded is, and find the next highest power of 2 (e.g.
212 in this case), to figure out what the range of the data is
supposed to be. I have assumed that if the maximum value in the LUT
data is less than a power of 2 minus 1 (e.g. 0xebc) then the intent
of the vendor was not to use the maximum available grayscale range
of the display (e.g. the maximum is 0xfff in this case). An alternative
would be to scale to the actual maximum rather than a power of two.

Very irritating, and in theory not totally reliable if one really
intended the full 16 bits and only used, say 15, but that is extremely
unlikely since everything would be too dark, and this heuristic
seems to work OK.

There has never been anything in the standard that describes having
to go through these convolutions. Since the only value in the
standard that describes the bit depth of the LUT values is LUT
Descriptor value 3 and that is (usually) always required to be
either 8 or 16, it mystifies me how the creators' of these images
imagine that the receiver is going to divine the range that is intended. Further, the standard is quite explicit that this 3rd
value defines the range of LUT values, but as far as I am aware, all
the vendors are ignoring the standard and indeed sending a third value
of 16 in all cases.

This problem is not confined to CR, and is also seen with DX products.

Typically I have seen:

- Agfa CR, which usually (always ?) sends LUTs, values up to 0x0fff
- Fuji CR, which occasionally send LUTs, values up to 0x03ff
- GE DX, for presentation, which always have LUTs, up to 0x3fff

Swissray, Siemens, Philips, Canon and Kodak never seem to send VOI LUTs
at this point (which is a whole other problem). Note that the presence
or absence of a VOI LUT as opposed to window values may be configurable
on the modality in some cases, and I have just looked at what I happen
to have received from a myriad of sites over whose configuration I have
no control. This may be why the majority of Fuji images have no VOI LUTs,
but a few do (or it may be the Siemens system that these Fuji images went
through that perhaps added it). I do have some test Hologic DX images that
are not from a clinical site that do actually get this right (a value
of 12 for the third value and a max of 0xfff).

Since almost every vendor that I have encountered that encodes LUTs
makes this mistake, perhaps it is time to amend the standard to warn
implementor's of receivers and/or sanction this bad behavior. We have
talked about this in the past in WG 6 but so far everyone has been
reluctant to write into the standard such a comment. Maybe it is time
to try again, since if one is not aware of this problem, one cannot
effectively implement display using VOI LUTs, and there is a vast
installed base to contend with.

I did not check presentation states, in which VOI LUTs could also be
encountered, for the prevalence of this mistake, nor did I look at the
encoding of Modality LUT's, which are unusual. Nor did I check digital
mammography images. I would be interested to hear from anyone who has.

David

PS. The following older thread in this newsgroup discusses this:

"http://groups-beta.google.com/group/comp.protocols.dicom/browse_frm/t hread/6a033444802a35fc/0f0a9a1e35c1468e?q=voi+lut&_done=%2Fgroup%2Fcom p.protocols.dicom%2Fsearch%3Fgroup%3Dcomp.protocols.dicom%26q%3Dvoi+lu t%26qt_g%3D1%26searchnow%3DSearch+this+group%26&_doneTitle=Back+to+Sea rch&&d#0f0a9a1e35c1468e"

PPS. From a historical perspective, the following may be of interest.

In the original standard in 1993, all that was said about this was a
reference to the corresponding such where Modality LUTs are described
that said:

"The third value specifies the number of bits for each entry in the
LUT Data. It shall take the value 8 or 16. The LUT Data shall be stored
in a format equivalent to 8 or 16 bits allocated and high bit equal
1-bits allocated."

Since the high bit hint was not apparently explicit enough, a very
early CP, CP 15 (submitted by Agfa as it happens), replaced this with:

"The third value conveys the range of LUT entry values. It shall take
the value 8 or 16, corresponding with the LUT entry value range of
256 or 65536.

Note:    The third value is not required for describing the
    LUT data and is only included for informational usage
    and for maintaining compatibility with ACRNEMA 2.0.

The LUT Data contains the LUT entry values."

That is how it read in the 1996, 1998 and 1999 editions.

By the 2000 edition, Supplement 33 that introduced presentation states
extensively reworked this entire section and tried to explain this in
different words:

"The output range is from 0 to 2^n-1 where n is the third value of LUT
Descriptor. This range is always unsigned."

and also added a note to spell out what the output range meant in the
VOI LUT section:

"9. The output of the Window Center/Width or VOI LUT transformation
is either implicitly scaled to the full range of the display device
if there is no succeeding transformation defined, or implicitly scaled
to the full input range of the succeeding transformation step (such as
the Presentation LUT), if present. See C.11.6.1."

It still reads this way in the 2004 edition.

Note that LUTs in other applications than the general VOI LUT allow for
values other than 8 or 16 in the third value of LUT descriptor to permit
ranges other than 0 to 255 or 65535.

In addition, the DX Image Module specializes the VOI LUT
attributes as follows, in PS 3.3 section C.8.11.3.1.5 (added in Sup 32):

"The third value specifies the number of bits for each entry in the LUT
Data (analogous to ìbits storedî). It shall be between 10-16. The LUT
Data shall be stored in a format equivalent to 16 ìbits allocatedî and
ìhigh bitî equal to ìbits storedî - 1. The third value conveys the range
of LUT entry values. These unsigned LUT entry values shall range between
0 and 2^n-1, where n is the third value of the LUT Descriptor."

So in the case of the GE DX for presentation images, the third value of
LUT descriptor is allowed to be and probably should be 14 rather than 16.

*/
