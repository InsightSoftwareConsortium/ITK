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

#include "gdcmCommon.h"
#include "gdcmBase.h"
#include "gdcmException.h"

namespace gdcm
{
class File;
class RLEFramesInfo;
class JPEGFragmentsInfo;

/**
 * \brief Utility container for gathering the various forms the pixel data
 *        migth take during the user demanded processes.
 */
class GDCM_EXPORT PixelReadConvert : public Base
{
public:
   PixelReadConvert();
   virtual ~PixelReadConvert();

   void Print( std::ostream &os = std::cout, std::string const &indent = "" );

   // Getter accessors:
   uint8_t *GetRGB()     { return RGB;     }
   size_t   GetRGBSize() { return RGBSize; }
   uint8_t *GetRaw()     { return Raw;     }
   size_t   GetRawSize() { return RawSize; }
   uint8_t *GetLutRGBA() { return LutRGBA; }

   // Predicates:
   bool IsRawRGB();

// In progress
   void GrabInformationsFromFile( File *file );
   bool ReadAndDecompressPixelData( std::ifstream *fp );
   void Squeeze();
   bool BuildRGBImage();

private:
   // Use the fp:
   void ReadAndDecompress12BitsTo16Bits( std::ifstream *fp ) 
                                 throw ( FormatError );
   bool ReadAndDecompressJPEGFile( std::ifstream *fp );

   // In place (within Decompressed and with no fp access) decompression
   // or convertion:
   void BuildLUTRGBA();
   void ConvertSwapZone();
   void ConvertReorderEndianity();
   bool ConvertReArrangeBits() throw ( FormatError );
   void ConvertRGBPlanesToRGBPixels();
   void ConvertYcBcRPlanesToRGBPixels();
   void ConvertHandleColor();

   void ComputeRawAndRGBSizes();
   void AllocateRGB();
   void AllocateRaw();

// Variables
   /// Pixel data represented as RGB after LUT color interpretation.
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

   size_t PixelOffset;
   size_t PixelDataLength;
   int XSize;
   int YSize;
   int ZSize;
   int BitsAllocated;
   int BitsStored;
   int HighBitPosition;
   int SamplesPerPixel;
   int PixelSize;
   bool PixelSign;
   int SwapCode;

   bool IsRaw;
   bool IsJPEG2000;
   bool IsJPEGLS;
   bool IsJPEGLossless;
   bool IsJPEGLossy;
   bool IsJPEG;
   bool IsRLELossless;

   RLEFramesInfo *RLEInfo;
   JPEGFragmentsInfo *JPEGInfo;

   // For handling color stage
   int PlanarConfiguration;
   bool IsMonochrome;
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

};
} // end namespace gdcm

//-----------------------------------------------------------------------------
#endif
