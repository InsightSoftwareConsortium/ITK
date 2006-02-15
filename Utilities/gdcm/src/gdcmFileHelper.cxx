/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmFileHelper.cxx
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

#include "gdcmFileHelper.h"
#include "gdcmGlobal.h"
#include "gdcmTS.h"
#include "gdcmDocument.h"
#include "gdcmDebug.h"
#include "gdcmUtil.h"
#include "gdcmBinEntry.h"
#include "gdcmValEntry.h"
#include "gdcmSeqEntry.h"
#include "gdcmSQItem.h"
#include "gdcmContentEntry.h"
#include "gdcmFile.h"
#include "gdcmPixelReadConvert.h"
#include "gdcmPixelWriteConvert.h"
#include "gdcmDocEntryArchive.h"
#include "gdcmDictSet.h"

#include <fstream>

/*
// ----------------------------- WARNING -------------------------

These lines will be moved to the document-to-be 'User's Guide'

// To read an image, user needs a gdcm::File
gdcm::File *f = new gdcm::File(fileName);
// or (advanced) :
// user may also decide he doesn't want to load some parts of the header
gdcm::File *f = new gdcm::File();
f->SetFileName(fileName);
   f->SetLoadMode(LD_NOSEQ);             // or      
   f->SetLoadMode(LD_NOSHADOW);          // or
   f->SetLoadMode(LD_NOSEQ | LD_NOSHADOW); // or
   f->SetLoadMode(LD_NOSHADOWSEQ);
f->Load();

// user can now check some values
std::string v = f->GetEntryValue(groupNb,ElementNb);

// to get the pixels, user needs a gdcm::FileHelper
gdcm::FileHelper *fh = new gdcm::FileHelper(f);
// user may ask not to convert Palette to RGB
uint8_t *pixels = fh->GetImageDataRaw();
int imageLength = fh->GetImageDataRawSize();
// He can now use the pixels, create a new image, ...
uint8_t *userPixels = ...

To re-write the image, user re-uses the gdcm::FileHelper

fh->SetImageData( userPixels, userPixelsLength);
fh->SetTypeToRaw(); // Even if it was possible to convert Palette to RGB
                     // (WriteMode is set)
 
fh->SetWriteTypeToDcmExpl(); // he wants Explicit Value Representation
                              // Little Endian is the default
                              // no other value is allowed
                                (-->SetWriteType(ExplicitVR);)
                                   -->WriteType = ExplicitVR;
fh->Write(newFileName);      // overwrites the file, if any

// or :
fh->WriteDcmExplVR(newFileName);


// ----------------------------- WARNING -------------------------

These lines will be moved to the document-to-be 'Developer's Guide'

WriteMode : WMODE_RAW / WMODE_RGB
WriteType : ImplicitVR, ExplicitVR, ACR, ACR_LIBIDO

fh1->Write(newFileName);
   SetWriteFileTypeToImplicitVR() / SetWriteFileTypeToExplicitVR();
   (modifies TransferSyntax)
   SetWriteToRaw(); / SetWriteToRGB();
      (modifies, when necessary : photochromatic interpretation, 
         samples per pixel, Planar configuration, 
         bits allocated, bits stored, high bit -ACR 24 bits-
         Pixels element VR, pushes out the LUT )
   CheckWriteIntegrity();
      (checks user given pixels length)
   FileInternal->Write(fileName,WriteType)
   fp = opens file(fileName);
   ComputeGroup0002Length( );
   BitsAllocated 12->16
      RemoveEntryNoDestroy(palettes, etc)
      Document::WriteContent(fp, writetype);
   RestoreWrite();
      (moves back to the File all the archived elements)
   RestoreWriteFileType();
      (pushes back group 0002, with TransferSyntax)
*/




namespace gdcm 
{
//-------------------------------------------------------------------------
// Constructor / Destructor
/**
 * \brief Constructor dedicated to deal with the *pixels* area of a ACR/DICOMV3
 *        file (gdcm::File only deals with the ... header)
 *        Opens (in read only and when possible) an existing file and checks
 *        for DICOM compliance. Returns NULL on failure.
 *        It will be up to the user to load the pixels into memory
 *        ( GetImageDataSize() + GetImageData() methods)
 * \note  the in-memory representation of all available tags found in
 *        the DICOM header is post-poned to first header information access.
 *        This avoid a double parsing of public part of the header when
 *        one sets an a posteriori shadow dictionary (efficiency can be
 *        seen as a side effect).   
 */
FileHelper::FileHelper( )
{ 
   FileInternal = new File( );
   SelfHeader = true;
   Initialize();
}

/**
 * \brief Constructor dedicated to deal with the *pixels* area of a ACR/DICOMV3
 *        file (File only deals with the ... header)
 *        Opens (in read only and when possible) an existing file and checks
 *        for DICOM compliance. Returns NULL on failure.
 *        It will be up to the user to load the pixels into memory
 *        ( GetImageDataSize() + GetImageData() methods)
 * \note  the in-memory representation of all available tags found in
 *        the DICOM header is post-poned to first header information access.
 *        This avoid a double parsing of public part of the header when
 *        user sets an a posteriori shadow dictionary (efficiency can be
 *        seen as a side effect).   
 * @param header already built Header
 */
FileHelper::FileHelper(File *header)
{
   FileInternal = header;
   SelfHeader = false;
   Initialize();
   if ( FileInternal->IsReadable() )
   {
      PixelReadConverter->GrabInformationsFromFile( FileInternal );
   }
}

#ifndef GDCM_LEGACY_REMOVE
/**
 * \brief DEPRECATED : use SetFilename() + SetLoadMode() + Load() methods
 *        Constructor dedicated to deal with the *pixels* area of a ACR/DICOMV3
 *        file (gdcm::File only deals with the ... header)
 *        Opens (in read only and when possible) an existing file and checks
 *        for DICOM compliance. Returns NULL on failure.
 *        It will be up to the user to load the pixels into memory
 * \note  the in-memory representation of all available tags found in
 *        the DICOM header is post-poned to first header information access.
 *        This avoid a double parsing of public part of the header when
 *        one sets an a posteriori shadow dictionary (efficiency can be
 *        seen as a side effect).   
 * @param filename file to be opened for parsing
 * @deprecated  use SetFilename() + Load() methods
 */
FileHelper::FileHelper(std::string const &filename )
{
   FileInternal = new File( );
   FileInternal->SetFileName( filename );
   FileInternal->Load();
   SelfHeader = true;
   Initialize();
   if ( FileInternal->IsReadable() )
   {
      PixelReadConverter->GrabInformationsFromFile( FileInternal );
   }
}
#endif

/**
 * \brief canonical destructor
 * \note  If the header (gdcm::File) was created by the FileHelper constructor,
 *        it is destroyed by the FileHelper
 */
FileHelper::~FileHelper()
{ 
   if ( PixelReadConverter )
   {
      delete PixelReadConverter;
   }
   if ( PixelWriteConverter )
   {
      delete PixelWriteConverter;
   }
   if ( Archive )
   {
      delete Archive;
   }

   if ( SelfHeader )
   {
      delete FileInternal;
   }
   FileInternal = 0;
}

//-----------------------------------------------------------------------------
// Public

/**
 * \brief Sets the LoadMode of the internal gdcm::File as a boolean string. 
 *        NO_SEQ, NO_SHADOW, NO_SHADOWSEQ
 *... (nothing more, right now)
 *        WARNING : before using NO_SHADOW, be sure *all* your files
 *        contain accurate values in the 0x0000 element (if any) 
 *        of *each* Shadow Group. The parser will fail if the size is wrong !
 * @param   loadMode Load mode to be used    
 */
void FileHelper::SetLoadMode(int loadMode) 
{ 
   GetFile()->SetLoadMode( loadMode ); 
}
/**
 * \brief Sets the LoadMode of the internal gdcm::File
 * @param  fileName name of the file to be open  
 */
void FileHelper::SetFileName(std::string const &fileName)
{
   FileInternal->SetFileName( fileName );
}

/**
 * \brief   Loader  
 * @return false if file cannot be open or no swap info was found,
 *         or no tag was found.
 */
bool FileHelper::Load()
{ 
   if ( !FileInternal->Load() )
      return false;

   PixelReadConverter->GrabInformationsFromFile( FileInternal );
   return true;
}

/**
 * \brief   Accesses an existing DocEntry (i.e. a Dicom Element)
 *          through it's (group, element) and modifies it's content with
 *          the given value.
 * @param   content new value (string) to substitute with
 * @param   group  group number of the Dicom Element to modify
 * @param   elem element number of the Dicom Element to modify
 * \return  false if DocEntry not found
 */
bool FileHelper::SetValEntry(std::string const &content,
                             uint16_t group, uint16_t elem)
{ 
   return FileInternal->SetValEntry(content, group, elem);
}


/**
 * \brief   Accesses an existing DocEntry (i.e. a Dicom Element)
 *          through it's (group, element) and modifies it's content with
 *          the given value.
 * @param   content new value (void*  -> uint8_t*) to substitute with
 * @param   lgth new value length
 * @param   group  group number of the Dicom Element to modify
 * @param   elem element number of the Dicom Element to modify
 * \return  false if DocEntry not found
 */
bool FileHelper::SetBinEntry(uint8_t *content, int lgth,
                             uint16_t group, uint16_t elem)
{
   return FileInternal->SetBinEntry(content, lgth, group, elem);
}

/**
 * \brief   Modifies the value of a given DocEntry (Dicom entry)
 *          when it exists. Creates it with the given value when unexistant.
 * @param   content (string)value to be set
 * @param   group   Group number of the Entry 
 * @param   elem  Element number of the Entry
 * \return  pointer to the modified/created Dicom entry (NULL when creation
 *          failed).
 */ 
ValEntry *FileHelper::InsertValEntry(std::string const &content,
                                     uint16_t group, uint16_t elem)
{
   return FileInternal->InsertValEntry(content, group, elem);
}

/**
 * \brief   Modifies the value of a given DocEntry (Dicom entry)
 *          when it exists. Creates it with the given value when unexistant.
 *          A copy of the binArea is made to be kept in the Document.
 * @param   binArea (binary)value to be set
 * @param   lgth new value length
 * @param   group   Group number of the Entry 
 * @param   elem  Element number of the Entry
 * \return  pointer to the modified/created Dicom entry (NULL when creation
 *          failed).
 */
BinEntry *FileHelper::InsertBinEntry(uint8_t *binArea, int lgth,
                                     uint16_t group, uint16_t elem)
{
   return FileInternal->InsertBinEntry(binArea, lgth, group, elem);
}

/**
 * \brief   Modifies the value of a given DocEntry (Dicom entry)
 *          when it exists. Creates it, empty (?!) when unexistant.
 * @param   group   Group number of the Entry 
 * @param   elem  Element number of the Entry
 * \return  pointer to the modified/created Dicom entry (NULL when creation
 *          failed).
 */
SeqEntry *FileHelper::InsertSeqEntry(uint16_t group, uint16_t elem)
{
   return FileInternal->InsertSeqEntry(group, elem);
}

/**
 * \brief   Get the size of the image data
 *          If the image can be RGB (with a lut or by default), the size 
 *          corresponds to the RGB image
 *         (use GetImageDataRawSize if you want to be sure to get *only*
 *          the size of the pixels)
 * @return  The image size
 */
size_t FileHelper::GetImageDataSize()
{
   if ( PixelWriteConverter->GetUserData() )
   {
      return PixelWriteConverter->GetUserDataSize();
   }
   return PixelReadConverter->GetRGBSize();
}

/**
 * \brief   Get the size of the image data
 *          If the image could be converted to RGB using a LUT, 
 *          this transformation is not taken into account by GetImageDataRawSize
 *          (use GetImageDataSize if you wish)
 * @return  The raw image size
 */
size_t FileHelper::GetImageDataRawSize()
{
   if ( PixelWriteConverter->GetUserData() )
   {
      return PixelWriteConverter->GetUserDataSize();
   }
   return PixelReadConverter->GetRawSize();
}

/**
 * \brief   - Allocates necessary memory,
 *          - Reads the pixels from disk (uncompress if necessary),
 *          - Transforms YBR pixels, if any, into RGB pixels,
 *          - Transforms 3 planes R, G, B, if any, into a single RGB Plane
 *          - Transforms single Grey plane + 3 Palettes into a RGB Plane
 *          - Copies the pixel data (image[s]/volume[s]) to newly allocated zone.
 * @return  Pointer to newly allocated pixel data.
 *          NULL if alloc fails 
 */
uint8_t *FileHelper::GetImageData()
{
   if ( PixelWriteConverter->GetUserData() )
   {
      return PixelWriteConverter->GetUserData();
   }

   if ( ! GetRaw() )
   {
      // If the decompression failed nothing can be done.
      return 0;
   }

   if ( FileInternal->HasLUT() && PixelReadConverter->BuildRGBImage() )
   {
      return PixelReadConverter->GetRGB();
   }
   else
   {
      // When no LUT or LUT conversion fails, return the Raw
      return PixelReadConverter->GetRaw();
   }
}

/**
 * \brief   Allocates necessary memory, 
 *          Transforms YBR pixels (if any) into RGB pixels
 *          Transforms 3 planes R, G, B  (if any) into a single RGB Plane
 *          Copies the pixel data (image[s]/volume[s]) to newly allocated zone. 
 *          DOES NOT transform Grey plane + 3 Palettes into a RGB Plane
 * @return  Pointer to newly allocated pixel data.
 *          NULL if alloc fails 
 */
uint8_t *FileHelper::GetImageDataRaw ()
{
   return GetRaw();
}

#ifndef GDCM_LEGACY_REMOVE
/**
 * \brief   Useless function, since PixelReadConverter forces us 
 *          copy the Pixels anyway.  
 *          Reads the pixels from disk (uncompress if necessary),
 *          Transforms YBR pixels, if any, into RGB pixels
 *          Transforms 3 planes R, G, B, if any, into a single RGB Plane
 *          Transforms single Grey plane + 3 Palettes into a RGB Plane   
 *          Copies at most MaxSize bytes of pixel data to caller allocated
 *          memory space.
 * \warning This function allows people that want to build a volume
 *          from an image stack *not to* have, first to get the image pixels, 
 *          and then move them to the volume area.
 *          It's absolutely useless for any VTK user since vtk chooses 
 *          to invert the lines of an image, that is the last line comes first
 *          (for some axis related reasons?). Hence he will have 
 *          to load the image line by line, starting from the end.
 *          VTK users have to call GetImageData
 *     
 * @param   destination Address (in caller's memory space) at which the
 *          pixel data should be copied
 * @param   maxSize Maximum number of bytes to be copied. When MaxSize
 *          is not sufficient to hold the pixel data the copy is not
 *          executed (i.e. no partial copy).
 * @return  On success, the number of bytes actually copied. Zero on
 *          failure e.g. MaxSize is lower than necessary.
 */
size_t FileHelper::GetImageDataIntoVector (void *destination, size_t maxSize)
{
   if ( ! GetRaw() )
   {
      // If the decompression failed nothing can be done.
      return 0;
   }

   if ( FileInternal->HasLUT() && PixelReadConverter->BuildRGBImage() )
   {
      if ( PixelReadConverter->GetRGBSize() > maxSize )
      {
         gdcmWarningMacro( "Pixel data bigger than caller's expected MaxSize");
         return 0;
      }
      memcpy( destination,
              (void*)PixelReadConverter->GetRGB(),
              PixelReadConverter->GetRGBSize() );
      return PixelReadConverter->GetRGBSize();
   }

   // Either no LUT conversion necessary or LUT conversion failed
   if ( PixelReadConverter->GetRawSize() > maxSize )
   {
      gdcmWarningMacro( "Pixel data bigger than caller's expected MaxSize");
      return 0;
   }
   memcpy( destination,
           (void *)PixelReadConverter->GetRaw(),
           PixelReadConverter->GetRawSize() );
   return PixelReadConverter->GetRawSize();
}
#endif

/**
 * \brief   Points the internal pointer to the callers inData
 *          image representation, BUT WITHOUT COPYING THE DATA.
 *          'image' Pixels are presented as C-like 2D arrays : line per line.
 *          'volume'Pixels are presented as C-like 3D arrays : plane per plane 
 * \warning Since the pixels are not copied, it is the caller's responsability
 *          not to deallocate its data before gdcm uses them (e.g. with
 *          the Write() method )
 * @param inData user supplied pixel area (uint8_t* is just for the compiler.
 *               user is allowed to pass any kind of pixelsn since the size is
 *               given in bytes) 
 * @param expectedSize total image size, *in Bytes*
 */
void FileHelper::SetImageData(uint8_t *inData, size_t expectedSize)
{
   SetUserData(inData, expectedSize);
}

/**
 * \brief   Set the image data defined by the user
 * \warning When writting the file, this data are get as default data to write
 * @param inData user supplied pixel area (uint8_t* is just for the compiler.
 *               user is allowed to pass any kind of pixels since the size is
 *               given in bytes) 
 * @param expectedSize total image size, *in Bytes* 
 */
void FileHelper::SetUserData(uint8_t *inData, size_t expectedSize)
{
   PixelWriteConverter->SetUserData(inData, expectedSize);
}

/**
 * \brief   Get the image data defined by the user
 * \warning When writting the file, this data are get as default data to write
 */
uint8_t *FileHelper::GetUserData()
{
   return PixelWriteConverter->GetUserData();
}

/**
 * \brief   Get the image data size defined by the user
 * \warning When writting the file, this data are get as default data to write
 */
size_t FileHelper::GetUserDataSize()
{
   return PixelWriteConverter->GetUserDataSize();
}

/**
 * \brief   Get the image data from the file.
 *          If a LUT is found, the data are expanded to be RGB
 */
uint8_t *FileHelper::GetRGBData()
{
   return PixelReadConverter->GetRGB();
}

/**
 * \brief   Get the image data size from the file.
 *          If a LUT is found, the data are expanded to be RGB
 */
size_t FileHelper::GetRGBDataSize()
{
   return PixelReadConverter->GetRGBSize();
}

/**
 * \brief   Get the image data from the file.
 *          Even when a LUT is found, the data are not expanded to RGB!
 */
uint8_t *FileHelper::GetRawData()
{
   return PixelReadConverter->GetRaw();
}

/**
 * \brief   Get the image data size from the file.
 *          Even when a LUT is found, the data are not expanded to RGB!
 */
size_t FileHelper::GetRawDataSize()
{
   return PixelReadConverter->GetRawSize();
}

/**
 * \brief Access to the underlying \ref PixelReadConverter RGBA LUT
 */
uint8_t* FileHelper::GetLutRGBA()
{
   if ( PixelReadConverter->GetLutRGBA() ==0 )
      PixelReadConverter->BuildLUTRGBA();
   return PixelReadConverter->GetLutRGBA();
}

/**
 * \brief Access to the underlying \ref PixelReadConverter RGBA LUT Item Number
 */
int FileHelper::GetLutItemNumber()
{
   return PixelReadConverter->GetLutItemNumber();
}

/**
 * \brief Access to the underlying \ref PixelReadConverter RGBA LUT Item Size
 */
int FileHelper::GetLutItemSize()
{
   return PixelReadConverter->GetLutItemSize();
}

/**
 * \brief Writes on disk A SINGLE Dicom file
 *        NO test is performed on  processor "Endiannity".
 *        It's up to the user to call his Reader properly
 * @param fileName name of the file to be created
 *                 (any already existing file is over written)
 * @return false if write fails
 */
bool FileHelper::WriteRawData(std::string const &fileName)
{
   std::ofstream fp1(fileName.c_str(), std::ios::out | std::ios::binary );
   if (!fp1)
   {
      gdcmWarningMacro( "Fail to open (write) file:" << fileName.c_str());
      return false;
   }

   if ( PixelWriteConverter->GetUserData() )
   {
      fp1.write( (char *)PixelWriteConverter->GetUserData(), 
                 PixelWriteConverter->GetUserDataSize() );
   }
   else if ( PixelReadConverter->GetRGB() )
   {
      fp1.write( (char *)PixelReadConverter->GetRGB(), 
                 PixelReadConverter->GetRGBSize());
   }
   else if ( PixelReadConverter->GetRaw() )
   {
      fp1.write( (char *)PixelReadConverter->GetRaw(), 
                 PixelReadConverter->GetRawSize());
   }
   else
   {
      gdcmErrorMacro( "Nothing written." );
   }

   fp1.close();

   return true;
}

/**
 * \brief Writes on disk A SINGLE Dicom file, 
 *        using the Implicit Value Representation convention
 *        NO test is performed on  processor "Endianity".
 * @param fileName name of the file to be created
 *                 (any already existing file is overwritten)
 * @return false if write fails
 */

bool FileHelper::WriteDcmImplVR (std::string const &fileName)
{
   SetWriteTypeToDcmImplVR();
   return Write(fileName);
}

/**
* \brief Writes on disk A SINGLE Dicom file, 
 *        using the Explicit Value Representation convention
 *        NO test is performed on  processor "Endiannity". 
 * @param fileName name of the file to be created
 *                 (any already existing file is overwritten)
 * @return false if write fails
 */

bool FileHelper::WriteDcmExplVR (std::string const &fileName)
{
   SetWriteTypeToDcmExplVR();
   return Write(fileName);
}

/**
 * \brief Writes on disk A SINGLE Dicom file, 
 *        using the ACR-NEMA convention
 *        NO test is performed on  processor "Endiannity".
 *        (a l'attention des logiciels cliniques 
 *        qui ne prennent en entrée QUE des images ACR ...
 * \warning if a DICOM_V3 header is supplied,
 *         groups < 0x0008 and shadow groups are ignored
 * \warning NO TEST is performed on processor "Endiannity".
 * @param fileName name of the file to be created
 *                 (any already existing file is overwritten)
 * @return false if write fails
 */

bool FileHelper::WriteAcr (std::string const &fileName)
{
   SetWriteTypeToAcr();
   return Write(fileName);
}

/**
 * \brief Writes on disk A SINGLE Dicom file, 
 * @param fileName name of the file to be created
 *                 (any already existing file is overwritten)
 * @return false if write fails
 */
bool FileHelper::Write(std::string const &fileName)
{
   switch(WriteType)
   {
      case ImplicitVR:
         SetWriteFileTypeToImplicitVR();
         break;
      case Unknown:  // should never happen; ExplicitVR is the default value
      case ExplicitVR:
         SetWriteFileTypeToExplicitVR();
         break;
      case ACR:
      case ACR_LIBIDO:
      // NOTHING is done here just for LibIDO.
      // Just to avoid further trouble if user creates a file ex-nihilo,
      // wants to write it as an ACR-NEMA file,
      // and forgets to create any Entry belonging to group 0008
      // (shame on him !)
      // We add Recognition Code (RET)
        if ( ! FileInternal->GetValEntry(0x0008, 0x0010) )
            FileInternal->InsertValEntry("", 0x0008, 0x0010);
         SetWriteFileTypeToACR();
        // SetWriteFileTypeToImplicitVR(); // ACR IS implicit VR !
         break;
   }
   CheckMandatoryElements();

   // --------------------------------------------------------------
   // Special Patch to allow gdcm to re-write ACR-LibIDO formated images
   //
   // if recognition code tells us we dealt with a LibIDO image
   // we reproduce on disk the switch between lineNumber and columnNumber
   // just before writting ...
   /// \todo the best trick would be *change* the recognition code
   ///       but pb expected if user deals with, e.g. COMPLEX images
   
   if ( WriteType == ACR_LIBIDO )
   {
      SetWriteToLibido();
   }
   else
   {
      SetWriteToNoLibido();
   }
   // ----------------- End of Special Patch ----------------
  
   switch(WriteMode)
   {
      case WMODE_RAW :
         SetWriteToRaw(); // modifies and pushes to the archive, when necessary
         break;
      case WMODE_RGB :
         SetWriteToRGB(); // modifies and pushes to the archive, when necessary
         break;
   }

   bool check = CheckWriteIntegrity(); // verifies length
   if (check)
   {
      check = FileInternal->Write(fileName,WriteType);
   }

   RestoreWrite();
   RestoreWriteFileType();
   RestoreWriteMandatory();

   // --------------------------------------------------------------
   // Special Patch to allow gdcm to re-write ACR-LibIDO formated images
   // 
   // ...and we restore the header to be Dicom Compliant again 
   // just after writting
   RestoreWriteOfLibido();
   // ----------------- End of Special Patch ----------------

   return check;
}

//-----------------------------------------------------------------------------
// Protected
/**
 * \brief Checks the write integrity
 *
 * The tests made are :
 *  - verify the size of the image to write with the possible write
 *    when the user set an image data
 * @return true if check is successfull
 */
bool FileHelper::CheckWriteIntegrity()
{
   if ( PixelWriteConverter->GetUserData() )
   {
      int numberBitsAllocated = FileInternal->GetBitsAllocated();
      if ( numberBitsAllocated == 0 || numberBitsAllocated == 12 )
      {
         gdcmWarningMacro( "numberBitsAllocated changed from " 
                          << numberBitsAllocated << " to 16 " 
                          << " for consistency purpose" );
         numberBitsAllocated = 16;
      }

      size_t decSize = FileInternal->GetXSize()
                     * FileInternal->GetYSize() 
                     * FileInternal->GetZSize()
                     * FileInternal->GetSamplesPerPixel()
                     * ( numberBitsAllocated / 8 );
      size_t rgbSize = decSize;
      if ( FileInternal->HasLUT() )
         rgbSize = decSize * 3;

      switch(WriteMode)
      {
         case WMODE_RAW :
            if ( decSize!=PixelWriteConverter->GetUserDataSize() )
            {
               gdcmWarningMacro( "Data size (Raw) is incorrect. Should be " 
                           << decSize << " / Found :" 
                           << PixelWriteConverter->GetUserDataSize() );
               return false;
            }
            break;
         case WMODE_RGB :
            if ( rgbSize!=PixelWriteConverter->GetUserDataSize() )
            {
               gdcmWarningMacro( "Data size (RGB) is incorrect. Should be " 
                          << decSize << " / Found " 
                          << PixelWriteConverter->GetUserDataSize() );
               return false;
            }
            break;
      }
   }   
   return true;
}

/**
 * \brief Updates the File to write RAW data (as opposed to RGB data)
 *       (modifies, when necessary, photochromatic interpretation, 
 *       bits allocated, Pixels element VR)
 */ 
void FileHelper::SetWriteToRaw()
{
   if ( FileInternal->GetNumberOfScalarComponents() == 3 
    && !FileInternal->HasLUT() )
   {
      SetWriteToRGB();
   } 
   else
   {
      ValEntry *photInt = CopyValEntry(0x0028,0x0004);
      if (FileInternal->HasLUT() )
      {
         photInt->SetValue("PALETTE COLOR ");
      }
      else
      {
         photInt->SetValue("MONOCHROME2 ");
      }

      PixelWriteConverter->SetReadData(PixelReadConverter->GetRaw(),
                                       PixelReadConverter->GetRawSize());

      std::string vr = "OB";
      if ( FileInternal->GetBitsAllocated()>8 )
         vr = "OW";
      if ( FileInternal->GetBitsAllocated()==24 ) // For RGB ACR files 
         vr = "OB";
      BinEntry *pixel = 
         CopyBinEntry(GetFile()->GetGrPixel(),GetFile()->GetNumPixel(),vr);
      pixel->SetValue(GDCM_BINLOADED);
      pixel->SetBinArea(PixelWriteConverter->GetData(),false);
      pixel->SetLength(PixelWriteConverter->GetDataSize());

      Archive->Push(photInt);
      Archive->Push(pixel);
   }
}

/**
 * \brief Updates the File to write RGB data (as opposed to RAW data)
 *       (modifies, when necessary, photochromatic interpretation, 
 *       samples per pixel, Planar configuration, 
 *       bits allocated, bits stored, high bit -ACR 24 bits-
 *       Pixels element VR, pushes out the LUT, )
 */ 
void FileHelper::SetWriteToRGB()
{
   if ( FileInternal->GetNumberOfScalarComponents()==3 )
   {
      PixelReadConverter->BuildRGBImage();
      
      ValEntry *spp = CopyValEntry(0x0028,0x0002);
      spp->SetValue("3 ");

      ValEntry *planConfig = CopyValEntry(0x0028,0x0006);
      planConfig->SetValue("0 ");

      ValEntry *photInt = CopyValEntry(0x0028,0x0004);
      photInt->SetValue("RGB ");

      if ( PixelReadConverter->GetRGB() )
      {
         PixelWriteConverter->SetReadData(PixelReadConverter->GetRGB(),
                                          PixelReadConverter->GetRGBSize());
      }
      else // Raw data
      {
         PixelWriteConverter->SetReadData(PixelReadConverter->GetRaw(),
                                          PixelReadConverter->GetRawSize());
      }

      std::string vr = "OB";
      if ( FileInternal->GetBitsAllocated()>8 )
         vr = "OW";
      if ( FileInternal->GetBitsAllocated()==24 ) // For RGB ACR files 
         vr = "OB";
      BinEntry *pixel = 
         CopyBinEntry(GetFile()->GetGrPixel(),GetFile()->GetNumPixel(),vr);
      pixel->SetValue(GDCM_BINLOADED);
      pixel->SetBinArea(PixelWriteConverter->GetData(),false);
      pixel->SetLength(PixelWriteConverter->GetDataSize());

      Archive->Push(spp);
      Archive->Push(planConfig);
      Archive->Push(photInt);
      Archive->Push(pixel);

      // Remove any LUT
      Archive->Push(0x0028,0x1101);
      Archive->Push(0x0028,0x1102);
      Archive->Push(0x0028,0x1103);
      Archive->Push(0x0028,0x1201);
      Archive->Push(0x0028,0x1202);
      Archive->Push(0x0028,0x1203);

      // push out Palette Color Lookup Table UID, if any
      Archive->Push(0x0028,0x1199);

      // For old '24 Bits' ACR-NEMA
      // Thus, we have a RGB image and the bits allocated = 24 and 
      // samples per pixels = 1 (in the read file)
      if ( FileInternal->GetBitsAllocated()==24 ) 
      {
         ValEntry *bitsAlloc = CopyValEntry(0x0028,0x0100);
         bitsAlloc->SetValue("8 ");

         ValEntry *bitsStored = CopyValEntry(0x0028,0x0101);
         bitsStored->SetValue("8 ");

         ValEntry *highBit = CopyValEntry(0x0028,0x0102);
         highBit->SetValue("7 ");

         Archive->Push(bitsAlloc);
         Archive->Push(bitsStored);
         Archive->Push(highBit);
      }
   }
   else
   {
      SetWriteToRaw();
   }
}

/**
 * \brief Restore the File write mode  
 */ 
void FileHelper::RestoreWrite()
{
   Archive->Restore(0x0028,0x0002);
   Archive->Restore(0x0028,0x0004);
   Archive->Restore(0x0028,0x0006);
   Archive->Restore(GetFile()->GetGrPixel(),GetFile()->GetNumPixel());

   // For old ACR-NEMA (24 bits problem)
   Archive->Restore(0x0028,0x0100);
   Archive->Restore(0x0028,0x0101);
   Archive->Restore(0x0028,0x0102);

   // For the LUT
   Archive->Restore(0x0028,0x1101);
   Archive->Restore(0x0028,0x1102);
   Archive->Restore(0x0028,0x1103);
   Archive->Restore(0x0028,0x1201);
   Archive->Restore(0x0028,0x1202);
   Archive->Restore(0x0028,0x1203);

   // For the Palette Color Lookup Table UID
   Archive->Restore(0x0028,0x1203); 


   // group 0002 may be pushed out for ACR-NEMA writting purposes 
   Archive->Restore(0x0002,0x0000);
   Archive->Restore(0x0002,0x0001);
   Archive->Restore(0x0002,0x0002);
   Archive->Restore(0x0002,0x0003);
   Archive->Restore(0x0002,0x0010);
   Archive->Restore(0x0002,0x0012);
   Archive->Restore(0x0002,0x0013);
   Archive->Restore(0x0002,0x0016);
   Archive->Restore(0x0002,0x0100);
   Archive->Restore(0x0002,0x0102);
}

/**
 * \brief Pushes out the whole group 0002
 *        FIXME : better, set a flag to tell the writer not to write it ...
 *        FIXME : method should probably have an other name !
 *                SetWriteFileTypeToACR is NOT opposed to 
 *                SetWriteFileTypeToExplicitVR and SetWriteFileTypeToImplicitVR
 */ 
void FileHelper::SetWriteFileTypeToACR()
{
   Archive->Push(0x0002,0x0000);
   Archive->Push(0x0002,0x0001);
   Archive->Push(0x0002,0x0002);
   Archive->Push(0x0002,0x0003);
   Archive->Push(0x0002,0x0010);
   Archive->Push(0x0002,0x0012);
   Archive->Push(0x0002,0x0013);
   Archive->Push(0x0002,0x0016);
   Archive->Push(0x0002,0x0100);
   Archive->Push(0x0002,0x0102);
}

/**
 * \brief Sets in the File the TransferSyntax to 'Explicit VR Little Endian"   
 */ 
void FileHelper::SetWriteFileTypeToExplicitVR()
{
   std::string ts = Util::DicomString( 
      Global::GetTS()->GetSpecialTransferSyntax(TS::ExplicitVRLittleEndian) );

   ValEntry *tss = CopyValEntry(0x0002,0x0010);
   tss->SetValue(ts);

   Archive->Push(tss);
}

/**
 * \brief Sets in the File the TransferSyntax to 'Implicit VR Little Endian"   
 */ 
void FileHelper::SetWriteFileTypeToImplicitVR()
{
   std::string ts = Util::DicomString(
      Global::GetTS()->GetSpecialTransferSyntax(TS::ImplicitVRLittleEndian) );

   ValEntry *tss = CopyValEntry(0x0002,0x0010);
   tss->SetValue(ts);

   Archive->Push(tss);
}


/**
 * \brief Restore in the File the initial group 0002
 */ 
void FileHelper::RestoreWriteFileType()
{
}

/**
 * \brief Set the Write not to Libido format
 */ 
void FileHelper::SetWriteToLibido()
{
   ValEntry *oldRow = dynamic_cast<ValEntry *>
                (FileInternal->GetDocEntry(0x0028, 0x0010));
   ValEntry *oldCol = dynamic_cast<ValEntry *>
                (FileInternal->GetDocEntry(0x0028, 0x0011));
   
   if ( oldRow && oldCol )
   {
      std::string rows, columns; 

      ValEntry *newRow=new ValEntry(oldRow->GetDictEntry());
      ValEntry *newCol=new ValEntry(oldCol->GetDictEntry());

      newRow->Copy(oldCol);
      newCol->Copy(oldRow);

      newRow->SetValue(oldCol->GetValue());
      newCol->SetValue(oldRow->GetValue());

      Archive->Push(newRow);
      Archive->Push(newCol);
   }

   ValEntry *libidoCode = CopyValEntry(0x0008,0x0010);
   libidoCode->SetValue("ACRNEMA_LIBIDO_1.1");
   Archive->Push(libidoCode);
}

/**
 * \brief Set the Write not to No Libido format
 */ 
void FileHelper::SetWriteToNoLibido()
{
   ValEntry *recCode = dynamic_cast<ValEntry *>
                (FileInternal->GetDocEntry(0x0008,0x0010));
   if ( recCode )
   {
      if ( recCode->GetValue() == "ACRNEMA_LIBIDO_1.1" )
      {
         ValEntry *libidoCode = CopyValEntry(0x0008,0x0010);
         libidoCode->SetValue("");
         Archive->Push(libidoCode);
      }
   }
}

/**
 * \brief Restore the Write format
 */ 
void FileHelper::RestoreWriteOfLibido()
{
   Archive->Restore(0x0028,0x0010);
   Archive->Restore(0x0028,0x0011);
   Archive->Restore(0x0008,0x0010);

   // Restore 'LibIDO-special' entries, if any
   Archive->Restore(0x0028,0x0015);
   Archive->Restore(0x0028,0x0016);
   Archive->Restore(0x0028,0x0017);
   Archive->Restore(0x0028,0x00199);
}

/**
 * \brief Duplicates a ValEntry or creates it.
 * @param   group   Group number of the Entry 
 * @param   elem  Element number of the Entry
 * \return  pointer to the new Val Entry (NULL when creation failed).          
 */ 
ValEntry *FileHelper::CopyValEntry(uint16_t group, uint16_t elem)
{
   DocEntry *oldE = FileInternal->GetDocEntry(group, elem);
   ValEntry *newE;

   if ( oldE )
   {
      newE = new ValEntry(oldE->GetDictEntry());
      newE->Copy(oldE);
   }
   else
   {
      newE = GetFile()->NewValEntry(group, elem);
   }

   return newE;
}

/**
 * \brief   Duplicates a BinEntry or creates it.
 * @param   group   Group number of the Entry 
 * @param   elem  Element number of the Entry
 * @param   vr  Value Representation of the Entry
 *          FIXME : what is it used for?
 * \return  pointer to the new Bin Entry (NULL when creation failed).
 */ 
BinEntry *FileHelper::CopyBinEntry(uint16_t group, uint16_t elem,
                                   const std::string &vr)
{
   DocEntry *oldE = FileInternal->GetDocEntry(group, elem);
   BinEntry *newE;

   if ( oldE ) 
      if ( oldE->GetVR()!=vr )
         oldE = NULL;

   if ( oldE )
   {
      newE = new BinEntry(oldE->GetDictEntry());
      newE->Copy(oldE);
   }
   else
   {
      newE = GetFile()->NewBinEntry(group, elem, vr);
   }

   return newE;
}

/**
 * \brief   This method is called automatically, just before writting
 *         in order to produce a 'True Dicom V3' image
 *         We cannot know *how* the user made the File (reading an old ACR-NEMA
 *         file or a not very clean DICOM file ...) 
 *          
 *          Just before writting :
 *             - we check the Entries
 *             - we create the mandatory entries if they are missing
 *             - we modify the values if necessary
 *             - we push the sensitive entries to the Archive
 *          The writing process will restore the entries as they where before 
 *          entering FileHelper::CheckMandatoryElements, so the user will always
 *          see the entries just as he left them.
 * 
 * \todo : - warn the user if we had to add some entries :
 *         even if a mandatory entry is missing, we add it, with a default value
 *         (we don't want to give up the writting process if user forgot to
 *         specify Lena's Patient ID, for instance ...)
 *         - read the whole PS 3.3 Part of DICOM  (890 pages)
 *         and write a *full* checker (probably one method per Modality ...)
 *         Any contribution is welcome. 
 *         - write a user callable full checker, to allow post reading
 *         and/or pre writting image consistency check.           
 */ 
 
void FileHelper::CheckMandatoryElements()
{
   // just to remember : 'official' 0002 group
   if ( WriteType != ACR && WriteType != ACR_LIBIDO )
   {
     // Group 000002 (Meta Elements) already pushed out
  
   //0002 0000 UL 1 Meta Group Length
   //0002 0001 OB 1 File Meta Information Version
   //0002 0002 UI 1 Media Stored SOP Class UID
   //0002 0003 UI 1 Media Stored SOP Instance UID
   //0002 0010 UI 1 Transfer Syntax UID
   //0002 0012 UI 1 Implementation Class UID
   //0002 0013 SH 1 Implementation Version Name
   //0002 0016 AE 1 Source Application Entity Title
   //0002 0100 UI 1 Private Information Creator
   //0002 0102 OB 1 Private Information
  
   // Create them if not found
   // Always modify the value
   // Push the entries to the archive.
      ValEntry *e_0002_0000 = CopyValEntry(0x0002,0x0000);
      e_0002_0000->SetValue("0"); // for the moment
      Archive->Push(e_0002_0000);
  
      BinEntry *e_0002_0001 = CopyBinEntry(0x0002,0x0001, "OB");
      e_0002_0001->SetBinArea((uint8_t*)Util::GetFileMetaInformationVersion(),
                               false);
      e_0002_0001->SetLength(2);
      Archive->Push(e_0002_0001);

   // 'Media Stored SOP Class UID' 
      ValEntry *e_0002_0002 = CopyValEntry(0x0002,0x0002);
      // [Secondary Capture Image Storage]
      e_0002_0002->SetValue("1.2.840.10008.5.1.4.1.1.7"); 
      Archive->Push(e_0002_0002);
 
   // 'Media Stored SOP Instance UID'   
      ValEntry *e_0002_0003 = CopyValEntry(0x0002,0x0003);
      e_0002_0003->SetValue(Util::CreateUniqueUID());
      Archive->Push(e_0002_0003); 

   // 'Implementation Class UID'
      ValEntry *e_0002_0012 = CopyValEntry(0x0002,0x0012);
      e_0002_0012->SetValue(Util::CreateUniqueUID());
      Archive->Push(e_0002_0012); 

   // 'Implementation Version Name'
      ValEntry *e_0002_0013 = CopyValEntry(0x0002,0x0013);
      std::string version = "GDCM ";
      version += Util::GetVersion();
      e_0002_0013->SetValue(version);
      Archive->Push(e_0002_0013);

   //'Source Application Entity Title' Not Mandatory
   //ValEntry *e_0002_0016 = CopyValEntry(0x0002,0x0016);
   //   e_0002_0016->SetValue("1.2.840.10008.5.1.4.1.1.7");
   //   Archive->Push(e_0002_0016);
   }

   // Push out 'LibIDO-special' entries, if any
   Archive->Push(0x0028,0x0015);
   Archive->Push(0x0028,0x0016);
   Archive->Push(0x0028,0x0017);
   Archive->Push(0x0028,0x00199);

   // Deal with the pb of (Bits Stored = 12)
   // - we're gonna write the image as Bits Stored = 16
   if ( FileInternal->GetEntryValue(0x0028,0x0100) ==  "12")
   {
      ValEntry *e_0028_0100 = CopyValEntry(0x0028,0x0100);
      e_0028_0100->SetValue("16");
      Archive->Push(e_0028_0100);
   }

   // Check if user wasn't drunk ;-)

   itksys_ios::ostringstream s;
   // check 'Bits Allocated' vs decent values
   int nbBitsAllocated = FileInternal->GetBitsAllocated();
   if ( nbBitsAllocated == 0 || nbBitsAllocated > 32)
   {
      ValEntry *e_0028_0100 = CopyValEntry(0x0028,0x0100);
      e_0028_0100->SetValue("16");
      Archive->Push(e_0028_0100); 
      gdcmWarningMacro("(0028,0100) changed from "
         << nbBitsAllocated << " to 16 for consistency purpose");
      nbBitsAllocated = 16; 
   }
   // check 'Bits Stored' vs 'Bits Allocated'   
   int nbBitsStored = FileInternal->GetBitsStored();
   if ( nbBitsStored == 0 || nbBitsStored > nbBitsAllocated )
   {
      s << nbBitsAllocated;
      ValEntry *e_0028_0101 = CopyValEntry(0x0028,0x0101);
      e_0028_0101->SetValue( s.str() );
      Archive->Push(e_0028_0101);
      gdcmWarningMacro("(0028,0101) changed from "
                       << nbBitsStored << " to " << nbBitsAllocated
                       << " for consistency purpose" );
      nbBitsStored = nbBitsAllocated; 
    }
   // check 'Hight Bit Position' vs 'Bits Allocated' and 'Bits Stored'
   int highBitPosition = FileInternal->GetHighBitPosition();
   if ( highBitPosition == 0 || 
        highBitPosition > nbBitsAllocated-1 ||
        highBitPosition < nbBitsStored-1  )
   {
      ValEntry *e_0028_0102 = CopyValEntry(0x0028,0x0102);

      s << nbBitsStored - 1; 
      e_0028_0102->SetValue( s.str() );
      Archive->Push(e_0028_0102);
      gdcmWarningMacro("(0028,0102) changed from "
                       << highBitPosition << " to " << nbBitsAllocated-1
                       << " for consistency purpose");
   }
  // --- Check UID-related Entries ---

   // If 'SOP Class UID' exists ('true DICOM' image)
   // we create the 'Source Image Sequence' SeqEntry
   // to hold informations about the Source Image

   ValEntry *e_0008_0016 = FileInternal->GetValEntry(0x0008, 0x0016);
   if ( e_0008_0016 != 0 )
   {
      // Create 'Source Image Sequence' SeqEntry
      SeqEntry *sis = new SeqEntry (
            Global::GetDicts()->GetDefaultPubDict()->GetEntry(0x0008, 0x2112) );
      SQItem *sqi = new SQItem(1);
      // (we assume 'SOP Instance UID' exists too) 
      // create 'Referenced SOP Class UID'
      ValEntry *e_0008_1150 = new ValEntry(
            Global::GetDicts()->GetDefaultPubDict()->GetEntry(0x0008, 0x1150) );
      e_0008_1150->SetValue( e_0008_0016->GetValue());
      sqi->AddEntry(e_0008_1150);
      
      // create 'Referenced SOP Instance UID'
      ValEntry *e_0008_0018 = FileInternal->GetValEntry(0x0008, 0x0018);
      ValEntry *e_0008_1155 = new ValEntry(
            Global::GetDicts()->GetDefaultPubDict()->GetEntry(0x0008, 0x1155) );
      e_0008_1155->SetValue( e_0008_0018->GetValue());
      sqi->AddEntry(e_0008_1155);

      sis->AddSQItem(sqi,1); 
      // temporarily replaces any previous 'Source Image Sequence' 
      Archive->Push(sis);
 
      // 'Image Type' (The written image is no longer an 'ORIGINAL' one)
      ValEntry *e_0008_0008 = CopyValEntry(0x0008,0x0008);
      e_0008_0008->SetValue("DERIVED\\PRIMARY");
      Archive->Push(e_0008_0008);
   } 
   else
   {
      // There was no 'SOP Class UID'.
      // the source image was NOT a true Dicom one.
      // We consider the image is a 'Secondary Capture' one
      // SOP Class UID
      e_0008_0016  =  new ValEntry( 
            Global::GetDicts()->GetDefaultPubDict()->GetEntry(0x0008, 0x0016) );
      // [Secondary Capture Image Storage]
      e_0008_0016 ->SetValue("1.2.840.10008.5.1.4.1.1.7"); 
      Archive->Push(e_0008_0016); 
   }

// ---- The user will never have to take any action on the following ----.

   // new value for 'SOP Instance UID'
   ValEntry *e_0008_0018 = new ValEntry(
         Global::GetDicts()->GetDefaultPubDict()->GetEntry(0x0008, 0x0018) );
   e_0008_0018->SetValue( Util::CreateUniqueUID() );
   Archive->Push(e_0008_0018);

   // Instance Creation Date
   ValEntry *e_0008_0012 = CopyValEntry(0x0008,0x0012);
   std::string date = Util::GetCurrentDate();
   e_0008_0012->SetValue(date.c_str());
   Archive->Push(e_0008_0012);
 
   // Instance Creation Time
   ValEntry *e_0008_0013 = CopyValEntry(0x0008,0x0013);
   std::string time = Util::GetCurrentTime();
   e_0008_0013->SetValue(time.c_str());
   Archive->Push(e_0008_0013);

// ----- Add Mandatory Entries if missing ---

// Entries whose type is 1 are mandatory, with a mandatory value
// Entries whose type is 1c are mandatory-inside-a-Sequence
// Entries whose type is 2 are mandatory, with a optional value
// Entries whose type is 2c are mandatory-inside-a-Sequence
// Entries whose type is 3 are optional

   // 'Serie Instance UID'
   // Keep the value if exists
   // The user is allowed to create his own Series, 
   // keeping the same 'Serie Instance UID' for various images
   // The user shouldn't add any image to a 'Manufacturer Serie'
   // but there is no way no to allowed him to do that 
   ValEntry *e_0020_000e = FileInternal->GetValEntry(0x0020, 0x000e);
   if ( !e_0020_000e )
   {
      e_0020_000e = new ValEntry(
           Global::GetDicts()->GetDefaultPubDict()->GetEntry(0x0020, 0x000e) );
      e_0020_000e->SetValue(Util::CreateUniqueUID() );
      Archive->Push(e_0020_000e);
   } 

   // 'Study Instance UID'
   // Keep the value if exists
   // The user is allowed to create his own Study, 
   //          keeping the same 'Study Instance UID' for various images
   // The user may add images to a 'Manufacturer Study',
   //          adding new series to an already existing Study 
   ValEntry *e_0020_000d = FileInternal->GetValEntry(0x0020, 0x000d);
   if ( !e_0020_000d )
   {
      e_0020_000d = new ValEntry(
            Global::GetDicts()->GetDefaultPubDict()->GetEntry(0x0020, 0x000d) );
      e_0020_000d->SetValue(Util::CreateUniqueUID() );
      Archive->Push(e_0020_000d);
   }

   // Modality : if missing we set it to 'OTher'
   ValEntry *e_0008_0060 = FileInternal->GetValEntry(0x0008, 0x0060);
   if ( !e_0008_0060 )
   {
      e_0008_0060 = new ValEntry(
            Global::GetDicts()->GetDefaultPubDict()->GetEntry(0x0008, 0x0060) );
      e_0008_0060->SetValue("OT");
      Archive->Push(e_0008_0060);
   } 

   // Manufacturer : if missing we set it to 'GDCM Factory'
   ValEntry *e_0008_0070 = FileInternal->GetValEntry(0x0008, 0x0070);
   if ( !e_0008_0070 )
   {
      e_0008_0070 = new ValEntry(
            Global::GetDicts()->GetDefaultPubDict()->GetEntry(0x0008, 0x0070) );
      e_0008_0070->SetValue("GDCM Factory");
      Archive->Push(e_0008_0070);
   } 

   // Institution Name : if missing we set it to 'GDCM Hospital'
   ValEntry *e_0008_0080 = FileInternal->GetValEntry(0x0008, 0x0080);
   if ( !e_0008_0080 )
   {
      e_0008_0080 = new ValEntry(
            Global::GetDicts()->GetDefaultPubDict()->GetEntry(0x0008, 0x0080) );
      e_0008_0080->SetValue("GDCM Hospital");
      Archive->Push(e_0008_0080);
   } 

   // Patient's Name : if missing, we set it to 'GDCM^Patient'
   ValEntry *e_0010_0010 = FileInternal->GetValEntry(0x0010, 0x0010);
   if ( !e_0010_0010 )
   {
      e_0010_0010 = new ValEntry(
            Global::GetDicts()->GetDefaultPubDict()->GetEntry(0x0010, 0x0010) );
      e_0010_0010->SetValue("GDCM^Patient");
      Archive->Push(e_0010_0010);
   } 

   // Patient's Birth Date : 'type 2' entry -> must exist, value not mandatory
   ValEntry *e_0010_0030 = FileInternal->GetValEntry(0x0010, 0x0030);
   if ( !e_0010_0030 )
   {
      e_0010_0030 = new ValEntry(
            Global::GetDicts()->GetDefaultPubDict()->GetEntry(0x0010, 0x0030) );
      e_0010_0030->SetValue("");
      Archive->Push(e_0010_0030);
   }

   // Patient's Sex :'type 2' entry -> must exist, value not mandatory
   ValEntry *e_0010_0040 = FileInternal->GetValEntry(0x0010, 0x0040);
   if ( !e_0010_0040 )
   {
      e_0010_0040 = new ValEntry(
            Global::GetDicts()->GetDefaultPubDict()->GetEntry(0x0010, 0x0040) );
      e_0010_0040->SetValue("");
      Archive->Push(e_0010_0040);
   }

   // Referring Physician's Name :'type 2' entry -> must exist, value not mandatory
   ValEntry *e_0008_0090 = FileInternal->GetValEntry(0x0008, 0x0090);
   if ( !e_0008_0090 )
   {
      e_0008_0090 = new ValEntry(
            Global::GetDicts()->GetDefaultPubDict()->GetEntry(0x0008, 0x0090) );
      e_0008_0090->SetValue("");
      Archive->Push(e_0008_0090);
   }

    // Pixel Spacing : defaulted to 1.0\1.0
   ValEntry *e_0028_0030 = FileInternal->GetValEntry(0x0028, 0x0030);
   if ( !e_0028_0030 )
   {
      e_0028_0030 = new ValEntry(
            Global::GetDicts()->GetDefaultPubDict()->GetEntry(0x0028, 0x0030) );
      e_0028_0030->SetValue("1.0\\1.0");
      Archive->Push(e_0028_0030);
   }  
} 
 
/**
 * \brief Restore in the File the initial group 0002
 */
void FileHelper::RestoreWriteMandatory()
{
   // group 0002 may be pushed out for ACR-NEMA writting purposes 
   Archive->Restore(0x0002,0x0000);
   Archive->Restore(0x0002,0x0001);
   Archive->Restore(0x0002,0x0002);
   Archive->Restore(0x0002,0x0003);
   Archive->Restore(0x0002,0x0010);
   Archive->Restore(0x0002,0x0012);
   Archive->Restore(0x0002,0x0013);
   Archive->Restore(0x0002,0x0016);
   Archive->Restore(0x0002,0x0100);
   Archive->Restore(0x0002,0x0102);

   Archive->Restore(0x0008,0x0012);
   Archive->Restore(0x0008,0x0013);
   Archive->Restore(0x0008,0x0016);
   Archive->Restore(0x0008,0x0018);
   Archive->Restore(0x0008,0x0060);
   Archive->Restore(0x0008,0x0070);
   Archive->Restore(0x0008,0x0080);
   Archive->Restore(0x0008,0x0090);
   Archive->Restore(0x0008,0x2112);

   Archive->Restore(0x0010,0x0010);
   Archive->Restore(0x0010,0x0030);
   Archive->Restore(0x0010,0x0040);

   Archive->Restore(0x0020,0x000d);
   Archive->Restore(0x0020,0x000e);

}

//-----------------------------------------------------------------------------
// Private
/**
 * \brief Factorization for various forms of constructors.
 */
void FileHelper::Initialize()
{
   UserFunction = 0;

   WriteMode = WMODE_RAW;
   WriteType = ExplicitVR;

   PixelReadConverter  = new PixelReadConvert;
   PixelWriteConverter = new PixelWriteConvert;
   Archive = new DocEntryArchive( FileInternal );
}

/**
 * \brief Reads/[decompresses] the pixels, 
 *        *without* making RGB from Palette Colors 
 * @return the pixels area, whatever its type 
 *         (uint8_t is just for prototyping : feel free to Cast it) 
 */ 
uint8_t *FileHelper::GetRaw()
{
   PixelReadConverter->SetUserFunction( UserFunction );

   uint8_t *raw = PixelReadConverter->GetRaw();
   if ( ! raw )
   {
      // The Raw image migth not be loaded yet:
      std::ifstream *fp = FileInternal->OpenFile();
      PixelReadConverter->ReadAndDecompressPixelData( fp );
      if ( fp ) 
         FileInternal->CloseFile();

      raw = PixelReadConverter->GetRaw();
      if ( ! raw )
      {
         gdcmWarningMacro( "Read/decompress of pixel data apparently went wrong.");
         return 0;
      }
   }
   return raw;
}

//-----------------------------------------------------------------------------
/**
 * \brief   Prints the common part of ValEntry, BinEntry, SeqEntry
 * @param   os ostream we want to print in
 * @param indent (unused)
 */
void FileHelper::Print(std::ostream &os, std::string const &)
{
   FileInternal->SetPrintLevel(PrintLevel);
   FileInternal->Print(os);

   if ( FileInternal->IsReadable() )
   {
      PixelReadConverter->SetPrintLevel(PrintLevel);
      PixelReadConverter->Print(os);
   }
}

//-----------------------------------------------------------------------------
} // end namespace gdcm
