/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmFile.h
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

#ifndef GDCMFILE_H
#define GDCMFILE_H

#include "gdcmCommon.h"
#include "gdcmHeader.h"
#include "gdcmPixelConvert.h"

namespace gdcm 
{
//-----------------------------------------------------------------------------
/*
 * In addition to Dicom header exploration, this class is designed
 * for accessing the image/volume content. One can also use it to
 * write Dicom/ACR-NEMA/RAW files.
 */
class GDCM_EXPORT File
{
public:
   File( Header* header );
   File( std::string const& filename );
 
   virtual ~File();

   /// Accessor to \ref Header
   Header* GetHeader() { return HeaderInternal; }

   /// Accessor to \ref ImageDataSize
   size_t GetImageDataSize() { return ImageDataSize; };

   /// Accessor to \ref ImageDataSizeRaw
   size_t GetImageDataSizeRaw() { return ImageDataSizeRaw; };

   /// Accessor to \ref PixelConverter
   PixelConvert* GetPixelConverter() { return PixelConverter; };

   uint8_t* GetImageData();
   size_t GetImageDataIntoVector(void* destination, size_t maxSize);
   uint8_t* GetImageDataRaw();

   // see also Header::SetImageDataSize ?!?         
   bool SetImageData (uint8_t* data, size_t expectedSize);

   // Write pixels of ONE image on hard drive
   // No test is made on processor "endianity"
   // The user must call his reader correctly
   bool WriteRawData  (std::string const& fileName);
   bool WriteDcmImplVR(std::string const& fileName);
   bool WriteDcmExplVR(std::string const& fileName);
   bool WriteAcr      (std::string const& fileName);

   virtual bool SetEntryByNumber(std::string const& content,
                                 uint16_t group, uint16_t element)
   { 
      HeaderInternal->SetEntryByNumber(content,group,element);
      return true;
   }
   uint8_t* GetLutRGBA();
     
protected:
   bool WriteBase(std::string const& fileName, FileType type);

private:
   void Initialise();

   void SaveInitialValues();    // will belong to the future PixelData class
   void RestoreInitialValues(); // will belong to the future PixelData class
   void DeleteInitialValues();  // will belong to the future PixelData class 
   uint8_t* GetDecompressed();
   int ComputeDecompressedPixelDataSizeFromHeader();

private:
// members variables:

   /// Header to use to load the file
   Header *HeaderInternal;

   /// \brief Whether the underlying \ref Header was loaded by
   ///  the constructor or passed to the constructor. When false
   ///  the destructor is in charge of deletion.
   bool SelfHeader;
   
   /// Wether already parsed or not
   bool Parsed;
      
   /// Utility pixel converter
   PixelConvert* PixelConverter;

/// FIXME
// --------------- Will be moved to a PixelData class
//

   /// \brief to hold the Pixels (when read)
   uint8_t* Pixel_Data;  // (was PixelData)
   
   /// \brief Size (in bytes) of required memory to hold the Gray Level pixels
   ///        represented in this file. This is used when the user DOESN'T want
   ///        the RGB pixels image when it's stored as a PALETTE COLOR image
   size_t ImageDataSizeRaw;
   
   /// \brief Size (in bytes) of requited memory to hold the the pixels
   ///        of this image in it's RGB convertion either from:
   ///        - Plane R, Plane G, Plane B 
   ///        - Grey Plane + Palette Color
   ///        - YBR Pixels (or from RGB Pixels, as well) 
   size_t ImageDataSize;
       
  /// \brief ==1  if GetImageDataRaw was used
  ///        ==0  if GetImageData    was used
  ///        ==-1 if ImageData never read                       
   int PixelRead;

  /// \brief length of the last allocated area devoided to receive Pixels
  ///        ( to allow us not to (free + new) if un necessary )     
   size_t LastAllocatedPixelDataLength; 

  // Initial values of some fields that can be modified during reading process
  // if user asked to transform gray level + LUT image into RGB image
     
  /// \brief Samples Per Pixel           (0x0028,0x0002), as found on disk
   std::string InitialSpp;
  /// \brief Photometric Interpretation  (0x0028,0x0004), as found on disk
   std::string InitialPhotInt;
  /// \brief Planar Configuration        (0x0028,0x0006), as found on disk   
   std::string InitialPlanConfig;
    
  // Initial values of some fields that can be modified during reading process
  // if the image was a 'strange' ACR-NEMA 
  // (Bits Allocated=12, High Bit not equal to Bits stored +1) 
  /// \brief Bits Allocated              (0x0028,0x0100), as found on disk
   std::string InitialBitsAllocated;
  /// \brief High Bit                    (0x0028,0x0102), as found on disk
   std::string InitialHighBit;
  
  // some DocEntry that can be moved out of the H table during reading process
  // if user asked to transform gray level + LUT image into RGB image
  // We keep a pointer on them for a future use.
     
  /// \brief Red Palette Color Lookup Table Descriptor   0028 1101 as read
  DocEntry* InitialRedLUTDescr;  
  /// \brief Green Palette Color Lookup Table Descriptor 0028 1102 as read
  DocEntry* InitialGreenLUTDescr;
  /// \brief Blue Palette Color Lookup Table Descriptor  0028 1103 as read
  DocEntry* InitialBlueLUTDescr;
  
  /// \brief Red Palette Color Lookup Table Data         0028 1201 as read
  DocEntry* InitialRedLUTData;  
  /// \brief Green Palette Color Lookup Table Data       0028 1202 as read
  DocEntry* InitialGreenLUTData;
  /// \brief Blue Palette Color Lookup Table Data        0028 1203 as read
  DocEntry* InitialBlueLUTData;
  
//
// --------------- end of future PixelData class
//  

};
} // end namespace gdcm

//-----------------------------------------------------------------------------
#endif
