/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmPixelReadConvert.h
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


#ifndef GDCMPIXELREADCONVERT_H
#define GDCMPIXELREADCONVERT_H

#include "gdcmBase.h"
#include "gdcmException.h"
#include <fstream>

namespace gdcm
{
class File;
class RLEFramesInfo;
class JPEGFragmentsInfo;

typedef void (*VOID_FUNCTION_PUINT8_PFILE_POINTER)(uint8_t *, File *);

/**
 * \brief Utility container for gathering the various forms the pixel data
 *        migth take during the user demanded processes.
 * WARNING : *none* of these functions may be invoked by gdcm user
 *           (internal use only)
 */
class GDCM_EXPORT PixelReadConvert : public Base
{
friend class FileHelper;

private:
   PixelReadConvert();
   virtual ~PixelReadConvert();

   void Print( std::ostream &os = std::cout, std::string const &indent = "" );

   // Getter accessors:
   /// \brief returns pixel area holding RGB Pixels, made from Grey level + LUT
   uint8_t *GetRGB()           { return RGB;     }
   /// \brief returns pixel area length -RGB Pixels, (from Grey level + LUT)-
   size_t   GetRGBSize()       { return RGBSize; }
   /// \brief returns pixel area holding native RGB Pixels or Grey level
   uint8_t *GetRaw()           { return Raw;     }
   /// \brief returns pixel area size -native RGB Pixels or Grey level-
   size_t   GetRawSize()       { return RawSize; }
   /// \brief returns Red Green Blue Alpha LUT
   uint8_t *GetLutRGBA()       { return LutRGBA; }
   /// \brief returns Lut Item Number
   int      GetLutItemNumber() { return LutItemNumber; }
   /// \brief returns Lut Item Size
   int      GetLutItemSize()   { return LutItemSize;   }
   // Predicates:
   bool IsRawRGB();

// In progress
   void GrabInformationsFromFile( File *file );
   bool ReadAndDecompressPixelData( std::ifstream *fp );
   void Squeeze();
   bool BuildRGBImage();
   void BuildLUTRGBA();
/// \brief Allow user to pass his own function to modify pixels 
///        (e.g; mirror, upsidedown, ...) just after reading
   void SetUserFunction( VOID_FUNCTION_PUINT8_PFILE_POINTER userFunc ) 
                         { UserFunction = userFunc; }

   // Use the fp:
   void ReadAndDecompress12BitsTo16Bits( std::ifstream *fp ) 
                                 throw ( FormatError );
   bool ReadAndDecompressJPEGFile( std::ifstream *fp );

   // In place (within Decompressed and with no fp access) decompression
   // or convertion:
   void ConvertSwapZone();
   void ConvertReorderEndianity();
   bool ConvertReArrangeBits() throw ( FormatError );
   void ConvertFixGreyLevels();
   void ConvertRGBPlanesToRGBPixels();
   void ConvertYcBcRPlanesToRGBPixels();
   void ConvertHandleColor();

   void ComputeRawAndRGBSizes();
   void AllocateRGB();
   void AllocateRaw();

// Variables
/**
 * \brief Pixel data represented as RGB after LUT color interpretation.
 *        'uint8_t' is just to avoid warnings at compile time.
 *        feel free to cast it as uint16_t if you need
 */ 
   uint8_t *RGB;
   /// Size of RGB image.
   size_t   RGBSize;
   /// Pixel data after decompression and bit/byte rearrangement.
   uint8_t *Raw;
   /// Size of Decompressed image.
   size_t   RawSize;
   /// \brief Red/Green/Blue/Alpha LookUpTable build out of the
   ///        Red/Green/Blue LUT descriptors (see \ref BuildLUTRGBA ).
   uint8_t *LutRGBA;
   int LutItemNumber;
   int LutItemSize;

   // *ALL* the following info belong to the FileHelper
   // One should think there is an analyze error in the model !

   size_t PixelOffset;
   size_t PixelDataLength;
   int XSize;
   int YSize;
   int ZSize;
   int BitsAllocated;
   int BitsStored;
   int HighBitPosition;
   int SamplesPerPixel;
   //int PixelSize; // useless
   bool PixelSign;
   int SwapCode;

   // cache whether this is a strange GE transfer syntax (which has   
   // one transfer syntax for the header and another for the pixel data).
   bool IsPrivateGETransferSyntax;

   bool IsRaw;
   bool IsJPEG2000;
   bool IsJPEGLS;
   bool IsJPEGLossless;
   bool IsJPEGLossy;
   bool IsJPEG;
   bool IsRLELossless;
   bool IsMPEG;

   RLEFramesInfo *RLEInfo;
   JPEGFragmentsInfo *JPEGInfo;

   // For handling color stage
   int PlanarConfiguration;
   bool IsMonochrome;
   bool IsMonochrome1;
   bool IsPaletteColor;
   bool IsYBRFull;
   bool HasLUT;
   // The 3 LUT descriptors may be different:
   std::string LutRedDescriptor;
   std::string LutGreenDescriptor;
   std::string LutBlueDescriptor;
   uint8_t *LutRedData;
   uint8_t *LutGreenData;
   uint8_t *LutBlueData;
   
   File *FileInternal; // must be passed to User Function
   VOID_FUNCTION_PUINT8_PFILE_POINTER UserFunction;
};
} // end namespace gdcm

//-----------------------------------------------------------------------------
#endif
