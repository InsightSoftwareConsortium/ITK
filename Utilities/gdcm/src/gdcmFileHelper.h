/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmFileHelper.h
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

#ifndef GDCMFILEHELPER_H
#define GDCMFILEHELPER_H

#include "gdcmBase.h"
#include <iostream>

namespace gdcm 
{
class File;
class ValEntry;
class BinEntry;
class SeqEntry;
class PixelReadConvert;
class PixelWriteConvert;
class DocEntryArchive;
//-----------------------------------------------------------------------------
/**
 * \brief In addition to Dicom header exploration, this class is designed
 * for accessing the image/volume content. One can also use it to
 * write Dicom/ACR-NEMA/RAW files.
 */
class GDCM_EXPORT FileHelper : public Base
{
public:
   enum FileMode
   {
      WMODE_RAW,
      WMODE_RGB
   };
     
public:
   FileHelper( );
   FileHelper( File *header );
   FileHelper( std::string const &filename );
 
   virtual ~FileHelper();

   void Print(std::ostream &os = std::cout, std::string const &indent = ""); 

   /// Accessor to \ref File
   File *GetFile() { return FileInternal; }

   // File methods
   bool SetValEntry(std::string const &content,
                    uint16_t group, uint16_t elem);
   bool SetBinEntry(uint8_t *content, int lgth,
                    uint16_t group, uint16_t elem);

   ValEntry *InsertValEntry(std::string const &content,
                            uint16_t group, uint16_t elem);
   BinEntry *InsertBinEntry(uint8_t *binArea, int lgth,
                            uint16_t group, uint16_t elem);
   SeqEntry *InsertSeqEntry(uint16_t group, uint16_t elem);

   // File helpers
   size_t GetImageDataSize();
   size_t GetImageDataRawSize();

   uint8_t *GetImageData();
   uint8_t *GetImageDataRaw();
   size_t GetImageDataIntoVector(void *destination, size_t maxSize);

   void SetImageData(uint8_t *data, size_t expectedSize);

   // User data
   void SetUserData(uint8_t *data, size_t expectedSize);
   uint8_t *GetUserData();
   size_t GetUserDataSize();
   // RBG data (from file)
   uint8_t *GetRGBData();
   size_t GetRGBDataSize();
   // RAW data (from file)
   uint8_t *GetRawData();
   size_t GetRawDataSize();

   // LUT
   uint8_t* GetLutRGBA();

   // Write mode

   /// \brief Tells the writer we want to write a Raw File (no header)
  void SetWriteModeToRaw()           { SetWriteMode(WMODE_RAW);  };
   /// \brief Tells the writer we want to write RGB image when possible
   ///        (as opposite to 'Grey pixels + Palettes color')
   void SetWriteModeToRGB()           { SetWriteMode(WMODE_RGB);  };
   /// \brief Sets the Write Mode ( )
   void SetWriteMode(FileMode mode)   { WriteMode = mode;         };
   /// \brief Gets the Write Mode ( )
   FileMode GetWriteMode()            { return WriteMode;         };

   // Write format

   /// \brief Tells the writer we want to write as Implicit VR
   void SetWriteTypeToDcmImplVR()     { SetWriteType(ImplicitVR); };
   /// \brief Tells the writer we want to write as Explicit VR
   void SetWriteTypeToDcmExplVR()     { SetWriteType(ExplicitVR); };
   /// \brief Tells the writer we want to write as ACR-NEMA
   void SetWriteTypeToAcr()           { SetWriteType(ACR);        };
   /// \brief Tells the writer we want to write as LibIDO
   void SetWriteTypeToAcrLibido()     { SetWriteType(ACR_LIBIDO); };
   /// \brief Tells the writer which format want to write
   /// (ImplicitVR, ExplicitVR, ACR, ACR_LIBIDO)
   void SetWriteType(FileType format) { WriteType = format;       };
   /// \brief Gets the format we want to write
   ///   (ImplicitVR, ExplicitVR, ACR, ACR_LIBIDO)
   FileType GetWriteType()            { return WriteType;         };

   // Write pixels of ONE image on hard drive
   // No test is made on processor "endianness"
   // The user must call his reader correctly
   bool WriteRawData  (std::string const &fileName);
   bool WriteDcmImplVR(std::string const &fileName);
   bool WriteDcmExplVR(std::string const &fileName);
   bool WriteAcr      (std::string const &fileName);
   bool Write         (std::string const &fileName);

protected:
   bool CheckWriteIntegrity();

   void SetWriteToRaw();
   void SetWriteToRGB();
   void RestoreWrite();

   void SetWriteFileTypeToACR();
   void SetWriteFileTypeToExplicitVR();
   void SetWriteFileTypeToImplicitVR();
   void RestoreWriteFileType();

   void SetWriteToLibido();
   void SetWriteToNoLibido();
   void RestoreWriteOfLibido();

   ValEntry *CopyValEntry(uint16_t group, uint16_t elem);
   BinEntry *CopyBinEntry(uint16_t group, uint16_t elem, 
                          const std::string &vr);

private:
   void Initialize();

   uint8_t *GetRaw();

// members variables:
   /// gdcm::File to use to load the file
   File *FileInternal;

   /// \brief Whether the underlying \ref gdcm::File was loaded by
   ///  the constructor or passed to the constructor. When false
   ///  the destructor is in charge of deletion.
   bool SelfHeader;
   
   /// Wether already parsed or not
   bool Parsed;

   // Utility pixel converter
   /// \brief Pointer to the PixelReadConverter
   PixelReadConvert *PixelReadConverter;
   /// \brief Pointer to the PixelWriteConverter
   PixelWriteConvert *PixelWriteConverter;

   // Utility header archive
   /// \brief Pointer to the DocEntryArchive (used while writting process)
   DocEntryArchive *Archive;

   // Write variables
   /// \brief (WMODE_RAW, WMODE_RGB)
   FileMode WriteMode;
   /// \brief (ImplicitVR, ExplicitVR, ACR, ACR_LIBIDO)
   FileType WriteType;
};
} // end namespace gdcm

//-----------------------------------------------------------------------------
#endif
