/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmPixelConvert.h
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


#ifndef GDCMPIXELCONVERT_H
#define GDCMPIXELCONVERT_H

#include "gdcmCommon.h"
#include "gdcmRLEFramesInfo.h"
#include "gdcmJPEGFragmentsInfo.h"
#include "gdcmException.h"
#include "gdcmHeader.h"

namespace gdcm
{
/*
 * \brief Utility container for gathering the various forms the pixel data
 *        migth take during the user demanded processes.
 */
class GDCM_EXPORT PixelConvert
{
public:
   PixelConvert();
   ~PixelConvert();

   //// Getter accessors:
   uint8_t* GetRGB() { return RGB; }
   size_t   GetRGBSize() { return RGBSize; }
   uint8_t* GetDecompressed() { return Decompressed; }
   size_t   GetDecompressedSize() { return DecompressedSize; }
   uint8_t* GetLutRGBA() { return LutRGBA; }

   //// Predicates:
   bool IsDecompressedRGB();

   void Print( std::string indent = "", std::ostream &os = std::cout );

// In progress
   void GrabInformationsFromHeader( Header* header );
   bool ReadAndDecompressPixelData( std::ifstream* fp );
   void Squeeze();
   bool BuildRGBImage();

private:
   // Use the fp:
   bool ReadAndDecompressRLEFragment(
                  uint8_t* subDecompressed,
                  long fragmentSize,
                  long decompressedSegmentSize,
                  std::ifstream* fp );
   void ReadAndDecompress12BitsTo16Bits( std::ifstream* fp ) throw ( FormatError );
   bool ReadAndDecompressRLEFile( std::ifstream* fp );
   bool ReadAndDecompressJPEGFile( std::ifstream* fp );
   void BuildLUTRGBA( std::ifstream* fp );

   // In place (within Decompressed and with no fp access) decompression
   // or convertion:
   void BuildLUTRGBA();
   bool DecompressRLE16BitsFromRLE8Bits( int NumberOfFrames );
   void ConvertSwapZone();
   void ConvertReorderEndianity();
   bool ConvertReArrangeBits() throw ( FormatError );
   void ConvertRGBPlanesToRGBPixels();
   void ConvertYcBcRPlanesToRGBPixels();
   void ConvertHandleColor();

   void ComputeDecompressedAndRGBSizes();
   void AllocateRGB();
   void AllocateDecompressed();

// Variables
   /// Pixel data represented as RGB after LUT color interpretation.
   uint8_t* RGB;
   /// Size of \ref RGB image.
   size_t   RGBSize;
   /// Pixel data after decompression and bit/byte rearrangement.
   uint8_t* Decompressed;
   /// Size of \ref Decompressed image.
   size_t   DecompressedSize;
   /// \brief Red/Green/Blue/Alpha LookUpTable build out of the
   ///        Red/Green/Blue LUT descriptors (see \ref BuildLUTRGBA ).
   uint8_t* LutRGBA;

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

   bool IsDecompressed;
   bool IsJPEG2000;
   bool IsJPEGLossless;
   bool IsRLELossless;

   RLEFramesInfo* RLEInfo;
   JPEGFragmentsInfo* JPEGInfo;

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
   uint8_t* LutRedData;
   uint8_t* LutGreenData;
   uint8_t* LutBlueData;

};
} // end namespace gdcm

//-----------------------------------------------------------------------------
#endif
