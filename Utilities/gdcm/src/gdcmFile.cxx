  /*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmFile.cxx
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

#include "gdcmFile.h"
#include "gdcmDebug.h"
#include <fstream>

namespace gdcm 
{
typedef std::pair<TagDocEntryHT::iterator,TagDocEntryHT::iterator> IterHT;

//-------------------------------------------------------------------------
// Constructor / Destructor
/**
 * \brief Constructor dedicated to deal with the *pixels* area of a ACR/DICOMV3
 *        file (Header only deals with the ... header)
 *        Opens (in read only and when possible) an existing file and checks
 *        for DICOM compliance. Returns NULL on failure.
 *        It will be up to the user to load the pixels into memory
 *        (see GetImageData, GetImageDataRaw)
 * \note  the in-memory representation of all available tags found in
 *        the DICOM header is post-poned to first header information access.
 *        This avoid a double parsing of public part of the header when
 *        user sets an a posteriori shadow dictionary (efficiency can be
 *        seen as a side effect).   
 * @param header already built Header
 */
File::File(Header *header)
{
   HeaderInternal = header;
   SelfHeader = false;
   Initialise();
}

/**
 * \brief Constructor dedicated to deal with the *pixels* area of a ACR/DICOMV3
 *        file (Header only deals with the ... header)
 *        Opens (in read only and when possible) an existing file and checks
 *        for DICOM compliance. Returns NULL on failure.
 *        It will be up to the user to load the pixels into memory
 *        (see GetImageData, GetImageDataRaw)
 * \note  the in-memory representation of all available tags found in
 *        the DICOM header is post-poned to first header information access.
 *        This avoid a double parsing of public part of the header when
 *        one sets an a posteriori shadow dictionary (efficiency can be
 *        seen as a side effect).   
 * @param filename file to be opened for parsing
 */
File::File(std::string const & filename )
{
   HeaderInternal = new Header( filename );
   SelfHeader = true;
   Initialise();
}

/**
 * \brief Factorization for various forms of constructors.
 */
void File::Initialise()
{
   PixelConverter = NULL; //just in case
   if ( HeaderInternal->IsReadable() )
   {
      ImageDataSizeRaw = ComputeDecompressedPixelDataSizeFromHeader();
      if ( HeaderInternal->HasLUT() )
      {
         ImageDataSize = 3 * ImageDataSizeRaw;
      }
      else
      {
         ImageDataSize = ImageDataSizeRaw;
      }

      PixelConverter = new PixelConvert;  //LEAK !
      PixelConverter->GrabInformationsFromHeader( HeaderInternal );
   }
   SaveInitialValues();
}

/**
 * \brief canonical destructor
 * \note  If the Header was created by the File constructor,
 *        it is destroyed by the File
 */
File::~File()
{ 
   if( SelfHeader )
   {
      delete HeaderInternal;
   }
   HeaderInternal = 0;

   DeleteInitialValues();
   if( PixelConverter )
   {
      //delete PixelConverter;
   }

}

/**
 * \brief Sets some initial values for the Constructor
 * \warning not end user intended
 */
void File::SaveInitialValues()
{ 

   PixelRead  = -1; // no ImageData read yet.
   LastAllocatedPixelDataLength = 0;
   Pixel_Data = 0;

   InitialSpp = "";     
   InitialPhotInt = "";
   InitialPlanConfig = "";
   InitialBitsAllocated = "";
   InitialHighBit = "";
  
   InitialRedLUTDescr   = 0;
   InitialGreenLUTDescr = 0;
   InitialBlueLUTDescr  = 0;
   InitialRedLUTData    = 0;
   InitialGreenLUTData  = 0;
   InitialBlueLUTData   = 0; 
                
   if ( HeaderInternal->IsReadable() )
   {
      // the following values *may* be modified 
      // by File::GetImageDataIntoVectorRaw
      // we save their initial value.
      InitialSpp           = HeaderInternal->GetEntryByNumber(0x0028,0x0002);
      InitialPhotInt       = HeaderInternal->GetEntryByNumber(0x0028,0x0004);
      InitialPlanConfig    = HeaderInternal->GetEntryByNumber(0x0028,0x0006);
      
      InitialBitsAllocated = HeaderInternal->GetEntryByNumber(0x0028,0x0100);
      InitialHighBit       = HeaderInternal->GetEntryByNumber(0x0028,0x0102);

      // the following entries *may* be removed from the H table
      // (NOT deleted ...) by File::GetImageDataIntoVectorRaw  
      // we keep a pointer on them.
      InitialRedLUTDescr   = HeaderInternal->GetDocEntryByNumber(0x0028,0x1101);
      InitialGreenLUTDescr = HeaderInternal->GetDocEntryByNumber(0x0028,0x1102);
      InitialBlueLUTDescr  = HeaderInternal->GetDocEntryByNumber(0x0028,0x1103);

      InitialRedLUTData    = HeaderInternal->GetDocEntryByNumber(0x0028,0x1201);
      InitialGreenLUTData  = HeaderInternal->GetDocEntryByNumber(0x0028,0x1202);
      InitialBlueLUTData   = HeaderInternal->GetDocEntryByNumber(0x0028,0x1203); 
   }
}

/**
 * \brief restores some initial values
 * \warning not end user intended
 */
void File::RestoreInitialValues()
{   
   if ( HeaderInternal->IsReadable() )
   {      
      // the following values *may* have been modified 
      // by File::GetImageDataIntoVectorRaw
      // we restore their initial value.
      if ( InitialSpp != "")
         HeaderInternal->SetEntryByNumber(InitialSpp,0x0028,0x0002);
      if ( InitialPhotInt != "")
         HeaderInternal->SetEntryByNumber(InitialPhotInt,0x0028,0x0004);
      if ( InitialPlanConfig != "")

         HeaderInternal->SetEntryByNumber(InitialPlanConfig,0x0028,0x0006);
      if ( InitialBitsAllocated != "")
          HeaderInternal->SetEntryByNumber(InitialBitsAllocated,0x0028,0x0100);
      if ( InitialHighBit != "")
          HeaderInternal->SetEntryByNumber(InitialHighBit,0x0028,0x0102);
               
      // the following entries *may* be have been removed from the H table
      // (NOT deleted ...) by File::GetImageDataIntoVectorRaw  
      // we restore them.

      if (InitialRedLUTDescr)
         HeaderInternal->AddEntry(InitialRedLUTDescr);
      if (InitialGreenLUTDescr)
         HeaderInternal->AddEntry(InitialGreenLUTDescr);
      if (InitialBlueLUTDescr)
         HeaderInternal->AddEntry(InitialBlueLUTDescr);

      if (InitialRedLUTData)
         HeaderInternal->AddEntry(InitialBlueLUTDescr);
      if (InitialGreenLUTData)
         HeaderInternal->AddEntry(InitialGreenLUTData);
      if (InitialBlueLUTData)
         HeaderInternal->AddEntry(InitialBlueLUTData);
   }
}

/**
 * \brief delete initial values (il they were saved)
 *        of InitialLutDescriptors and InitialLutData
 */
void File::DeleteInitialValues()
{ 

// InitialLutDescriptors and InitialLutData
// will have to be deleted if the don't belong any longer
// to the Header H table when the header is deleted...

   if ( InitialRedLUTDescr )           
      delete InitialRedLUTDescr;
  
   if ( InitialGreenLUTDescr )
      delete InitialGreenLUTDescr;
      
   if ( InitialBlueLUTDescr )      
      delete InitialBlueLUTDescr; 
       
   if ( InitialRedLUTData )      
      delete InitialRedLUTData;
   
   if ( InitialGreenLUTData != NULL)
      delete InitialGreenLUTData;
      
   if ( InitialBlueLUTData != NULL)      
      delete InitialBlueLUTData;      
}

//-----------------------------------------------------------------------------
// Print

//-----------------------------------------------------------------------------
// Public

/**
 * \brief     computes the length (in bytes) we must ALLOCATE to receive the
 *            image(s) pixels (multiframes taken into account) 
 * \warning : it is NOT the group 7FE0 length
 *          (no interest for compressed images).
 */
int File::ComputeDecompressedPixelDataSizeFromHeader()
{
   // see PS 3.3-2003 : C.7.6.3.2.1  
   // 
   //   MONOCHROME1
   //   MONOCHROME2
   //   PALETTE COLOR
   //   RGB
   //   HSV  (Retired)
   //   ARGB (Retired)
   //   CMYK (Retired)
   //   YBR_FULL
   //   YBR_FULL_422 (no LUT, no Palette)
   //   YBR_PARTIAL_422
   //   YBR_ICT
   //   YBR_RCT

   // LUT's
   // ex : gdcm-US-ALOKA-16.dcm
   // 0028|1221 [OW]   [Segmented Red Palette Color Lookup Table Data]
   // 0028|1222 [OW]   [Segmented Green Palette Color Lookup Table Data]  
   // 0028|1223 [OW]   [Segmented Blue Palette Color Lookup Table Data]

   // ex  : OT-PAL-8-face.dcm
   // 0028|1201 [US]   [Red Palette Color Lookup Table Data]
   // 0028|1202 [US]   [Green Palette Color Lookup Table Data]
   // 0028|1203 [US]   [Blue Palette Color Lookup Table Data]

   int numberBitsAllocated = HeaderInternal->GetBitsAllocated();
   // Number of "Bits Allocated" is fixed to 16 when:
   //  - it is not defined (i.e. it's value is 0)
   //  - it's 12, since we will expand the image to 16 bits (see
   //    PixelConvert::ConvertDecompress12BitsTo16Bits() )
   if ( numberBitsAllocated == 0 || numberBitsAllocated == 12 )
   {
      numberBitsAllocated = 16;
   } 

   int DecompressedSize = HeaderInternal->GetXSize()
                        * HeaderInternal->GetYSize() 
                        * HeaderInternal->GetZSize()
                        * ( numberBitsAllocated / 8 )
                        * HeaderInternal->GetSamplesPerPixel();
   
   return DecompressedSize;
}

/**
 * \brief   - Allocates necessary memory, 
 *          - Reads the pixels from disk (uncompress if necessary),
 *          - Transforms YBR pixels, if any, into RGB pixels
 *          - Transforms 3 planes R, G, B, if any, into a single RGB Plane
 *          - Transforms single Grey plane + 3 Palettes into a RGB Plane
 *          - Copies the pixel data (image[s]/volume[s]) to newly allocated zone.
 * @return  Pointer to newly allocated pixel data.
 *          NULL if alloc fails 
 */
uint8_t* File::GetImageData()
{
   if ( ! GetDecompressed() )
   {
      // If the decompression failed nothing can be done.
      return 0;
   }
                                                                                
   uint8_t* pixelData;
   if ( HeaderInternal->HasLUT() && PixelConverter->BuildRGBImage() )
   {
      pixelData = PixelConverter->GetRGB();
   }
   else
   {
      // When no LUT or LUT conversion fails, return the decompressed
      pixelData = PixelConverter->GetDecompressed();
   }

// PIXELCONVERT CLEANME
   // Restore the header in a disk-consistent state
   // (if user asks twice to get the pixels from disk)
   if ( PixelRead != -1 ) // File was "read" before
   {
      RestoreInitialValues();
   }
   if ( PixelConverter->GetRGB() )
   {
      // now, it's an RGB image
      // Lets's write it in the Header
      std::string spp = "3";        // Samples Per Pixel
      HeaderInternal->SetEntryByNumber(spp,0x0028,0x0002);
      std::string rgb = "RGB ";     // Photometric Interpretation
      HeaderInternal->SetEntryByNumber(rgb,0x0028,0x0004);
      std::string planConfig = "0"; // Planar Configuration
      HeaderInternal->SetEntryByNumber(planConfig,0x0028,0x0006);
      PixelRead = 0; // no PixelRaw
   }
   else
   {
      if ( HeaderInternal->HasLUT() )
      {
         // The LUT interpretation failed
         std::string photometricInterpretation = "MONOCHROME1 ";
         HeaderInternal->SetEntryByNumber( photometricInterpretation,
                                           0x0028, 0x0004 );
         PixelRead = 0; // no PixelRaw
      }
      else
      {
         if ( PixelConverter->IsDecompressedRGB() )
         {
            ///////////////////////////////////////////////////
            // now, it's an RGB image
            // Lets's write it in the Header
            // Droping Palette Color out of the Header
            // has been moved to the Write process.
            // TODO : move 'values' modification to the write process
            //      : save also (in order to be able to restore)
            //      : 'high bit' -when not equal to 'bits stored' + 1
            //      : 'bits allocated', when it's equal to 12 ?!
            std::string spp = "3";            // Samples Per Pixel
            std::string photInt = "RGB ";     // Photometric Interpretation
            std::string planConfig = "0";     // Planar Configuration
            HeaderInternal->SetEntryByNumber(spp,0x0028,0x0002);
            HeaderInternal->SetEntryByNumber(photInt,0x0028,0x0004);
            HeaderInternal->SetEntryByNumber(planConfig,0x0028,0x0006);
         }
         PixelRead = 1; // PixelRaw
      } 
   }

   // We say the value *is* loaded.
   GetHeader()->SetEntryByNumber( GDCM_BINLOADED,
      GetHeader()->GetGrPixel(), GetHeader()->GetNumPixel());

   // Will be 7fe0, 0010 in standard case
   GetHeader()->SetEntryBinAreaByNumber( pixelData, 
      GetHeader()->GetGrPixel(), GetHeader()->GetNumPixel()); 
// END PIXELCONVERT CLEANME

   return pixelData;
}

/**
 * \brief
 *          Read the pixels from disk (uncompress if necessary),
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
size_t File::GetImageDataIntoVector (void* destination, size_t maxSize)
{
   if ( ! GetDecompressed() )
   {
      // If the decompression failed nothing can be done.
      return 0;
   }

   if ( HeaderInternal->HasLUT() && PixelConverter->BuildRGBImage() )
   {
      if ( PixelConverter->GetRGBSize() > maxSize )
      {
         dbg.Verbose(0, "File::GetImageDataIntoVector: pixel data bigger"
                        "than caller's expected MaxSize");
         return 0;
      }
      memmove( destination,
               (void*)PixelConverter->GetRGB(),
               PixelConverter->GetRGBSize() );
      return PixelConverter->GetRGBSize();
   }

   // Either no LUT conversion necessary or LUT conversion failed
   if ( PixelConverter->GetDecompressedSize() > maxSize )
   {
      dbg.Verbose(0, "File::GetImageDataIntoVector: pixel data bigger"
                     "than caller's expected MaxSize");
      return 0;
   }
   memmove( destination,
            (void*)PixelConverter->GetDecompressed(),
            PixelConverter->GetDecompressedSize() );
   return PixelConverter->GetDecompressedSize();
}

/**
 * \brief   Allocates necessary memory, 
 *          Transforms YBR pixels (if any) into RGB pixels
 *          Transforms 3 planes R, G, B  (if any) into a single RGB Plane
 *          Copies the pixel data (image[s]/volume[s]) to newly allocated zone. 
 *          DOES NOT transform Grey plane + 3 Palettes into a RGB Plane
 * @return  Pointer to newly allocated pixel data.
 * \        NULL if alloc fails 
 */
uint8_t* File::GetImageDataRaw ()
{
   uint8_t* decompressed = GetDecompressed();
   if ( ! decompressed )
   {
      return 0;
   }

// PIXELCONVERT CLEANME
   // Restore the header in a disk-consistent state
   // (if user asks twice to get the pixels from disk)
   if ( PixelRead != -1 ) // File was "read" before
   {
      RestoreInitialValues();
   }
   if ( PixelConverter->IsDecompressedRGB() )
   {
      ///////////////////////////////////////////////////
      // now, it's an RGB image
      // Lets's write it in the Header
      // Droping Palette Color out of the Header
      // has been moved to the Write process.
      // TODO : move 'values' modification to the write process
      //      : save also (in order to be able to restore)
      //      : 'high bit' -when not equal to 'bits stored' + 1
      //      : 'bits allocated', when it's equal to 12 ?!
      std::string spp = "3";            // Samples Per Pixel
      std::string photInt = "RGB ";     // Photometric Interpretation
      std::string planConfig = "0";     // Planar Configuration
      HeaderInternal->SetEntryByNumber(spp,0x0028,0x0002);
      HeaderInternal->SetEntryByNumber(photInt,0x0028,0x0004);
      HeaderInternal->SetEntryByNumber(planConfig,0x0028,0x0006);
   }

   // We say the value *is* loaded.
   GetHeader()->SetEntryByNumber( GDCM_BINLOADED,
   GetHeader()->GetGrPixel(), GetHeader()->GetNumPixel());
 
   // will be 7fe0, 0010 in standard cases
   GetHeader()->SetEntryBinAreaByNumber( decompressed,
   GetHeader()->GetGrPixel(), GetHeader()->GetNumPixel());
 
   PixelRead = 1; // PixelRaw
// END PIXELCONVERT CLEANME

   return decompressed;
}

uint8_t* File::GetDecompressed()
{
   uint8_t* decompressed = PixelConverter->GetDecompressed();
   if ( ! decompressed )
   {
      // The decompressed image migth not be loaded yet:
      std::ifstream* fp = HeaderInternal->OpenFile();
      PixelConverter->ReadAndDecompressPixelData( fp );
      HeaderInternal->CloseFile();
      decompressed = PixelConverter->GetDecompressed();
      if ( ! decompressed )
      {
        dbg.Verbose(0, "File::GetDecompressed: read/decompress of "
                       "pixel data apparently went wrong.");
         return 0;
      }
   }

   return decompressed;
}

/**
 * \brief   Points the internal Pixel_Data pointer to the callers inData
 *          image representation, BUT WITHOUT COPYING THE DATA.
 *          'image' Pixels are presented as C-like 2D arrays : line per line.
 *          'volume'Pixels are presented as C-like 3D arrays : plane per plane 
 * \warning Since the pixels are not copied, it is the caller's responsability
 *          not to deallocate it's data before gdcm uses them (e.g. with
 *          the Write() method.
 * @param inData user supplied pixel area
 * @param expectedSize total image size, in Bytes
 *
 * @return boolean
 */
bool File::SetImageData(uint8_t* inData, size_t expectedSize)
{
   HeaderInternal->SetImageDataSize( expectedSize );
// FIXME : if already allocated, memory leak !
   Pixel_Data     = inData;
   ImageDataSize = ImageDataSizeRaw = expectedSize;
   PixelRead     = 1;
// FIXME : 7fe0, 0010 IS NOT set ...
   return true;
}

/**
 * \brief Writes on disk A SINGLE Dicom file
 *        NO test is performed on  processor "Endiannity".
 *        It's up to the user to call his Reader properly
 * @param fileName name of the file to be created
 *                 (any already existing file is over written)
 * @return false if write fails
 */

bool File::WriteRawData(std::string const & fileName)
{
  std::ofstream fp1(fileName.c_str(), std::ios::out | std::ios::binary );
   if (!fp1)
   {
      dbg.Verbose(2, "Fail to open (write) file:", fileName.c_str());
      return false;
   }
   fp1.write((char*)Pixel_Data, ImageDataSize);
   fp1.close();

   return true;
}

/**
 * \brief Writes on disk A SINGLE Dicom file, 
 *        using the Implicit Value Representation convention
 *        NO test is performed on  processor "Endiannity".
 * @param fileName name of the file to be created
 *                 (any already existing file is overwritten)
 * @return false if write fails
 */

bool File::WriteDcmImplVR (std::string const & fileName)
{
   return WriteBase(fileName, ImplicitVR);
}

/**
* \brief Writes on disk A SINGLE Dicom file, 
 *        using the Explicit Value Representation convention
 *        NO test is performed on  processor "Endiannity". * @param fileName name of the file to be created
 *                 (any already existing file is overwritten)
 * @return false if write fails
 */

bool File::WriteDcmExplVR (std::string const & fileName)
{
   return WriteBase(fileName, ExplicitVR);
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

bool File::WriteAcr (std::string const & fileName)
{
   return WriteBase(fileName, ACR);
}

//-----------------------------------------------------------------------------
// Protected
/**
 * \brief NOT a end user inteded function
 *        (used by WriteDcmExplVR, WriteDcmImplVR, WriteAcr, etc)
 * @param fileName name of the file to be created
 *                 (any already existing file is overwritten)
 * @param  type file type (ExplicitVR, ImplicitVR, ...)
 * @return false if write fails
 */
bool File::WriteBase (std::string const & fileName, FileType type)
{
   if ( PixelRead == -1 && type != ExplicitVR)
   {
      return false;
   }

   std::ofstream* fp1 = new std::ofstream(fileName.c_str(), 
                              std::ios::out | std::ios::binary);
   if (fp1 == NULL)
   {
      dbg.Verbose(2, "Failed to open (write) File: " , fileName.c_str());
      return false;
   }

   if ( type == ImplicitVR || type == ExplicitVR )
   {
      // writing Dicom File Preamble
      char filePreamble[128];
      memset(filePreamble, 0, 128);
      fp1->write(filePreamble, 128);
      fp1->write("DICM", 4);
   }

   // --------------------------------------------------------------
   // Special Patch to allow gdcm to re-write ACR-LibIDO formated images
   //
   // if recognition code tells us we dealt with a LibIDO image
   // we reproduce on disk the switch between lineNumber and columnNumber
   // just before writting ...
   
   /// \todo the best trick would be *change* the recognition code
   ///       but pb expected if user deals with, e.g. COMPLEX images

   std::string rows, columns; 
   if ( HeaderInternal->GetFileType() == ACR_LIBIDO)
   {
      rows    = HeaderInternal->GetEntryByNumber(0x0028, 0x0010);
      columns = HeaderInternal->GetEntryByNumber(0x0028, 0x0011);

      HeaderInternal->SetEntryByNumber(columns,  0x0028, 0x0010);
      HeaderInternal->SetEntryByNumber(rows   ,  0x0028, 0x0011);
   }
   // ----------------- End of Special Patch ----------------
      
   uint16_t grPixel  = HeaderInternal->GetGrPixel();
   uint16_t numPixel = HeaderInternal->GetNumPixel();;
          
   DocEntry* PixelElement = 
      GetHeader()->GetDocEntryByNumber(grPixel, numPixel);  
 
   if ( PixelRead == 1 )
   {
      // we read pixel 'as is' (no tranformation LUT -> RGB)
      PixelElement->SetLength( ImageDataSizeRaw );
   }
   else if ( PixelRead == 0 )
   {
      // we tranformed GrayLevel pixels + LUT into RGB Pixel
      PixelElement->SetLength( ImageDataSize );
   }
 
   HeaderInternal->Write(fp1, type);

   // --------------------------------------------------------------
   // Special Patch to allow gdcm to re-write ACR-LibIDO formated images
   // 
   // ...and we restore the Header to be Dicom Compliant again 
   // just after writting

   if ( HeaderInternal->GetFileType() == ACR_LIBIDO )
   {
      HeaderInternal->SetEntryByNumber(rows   , 0x0028, 0x0010);
      HeaderInternal->SetEntryByNumber(columns, 0x0028, 0x0011);
   }
   // ----------------- End of Special Patch ----------------
   fp1->close ();
   delete fp1;

   return true;
}

/**
 * \brief Access to the underlying \ref PixelConverter RGBA LUT
 */
uint8_t* File::GetLutRGBA()
{
   return PixelConverter->GetLutRGBA();
}

} // end namespace gdcm

