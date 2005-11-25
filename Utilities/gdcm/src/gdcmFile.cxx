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

//
// --------------  Remember ! ----------------------------------
//
// Image Position (Patient)                            (0020,0032):
// If not found (ACR_NEMA) we try Image Position       (0020,0030)
// If not found (ACR-NEMA), we consider Slice Location (0020,1041)
//                                   or Location       (0020,0050) 
//                                   as the Z coordinate, 
// 0. for all the coordinates if nothing is found
//
// ---------------------------------------------------------------
//
#include "gdcmFile.h"
#include "gdcmGlobal.h"
#include "gdcmUtil.h"
#include "gdcmDebug.h"
#include "gdcmTS.h"
#include "gdcmValEntry.h"
#include "gdcmBinEntry.h"
#include "gdcmSeqEntry.h"
#include "gdcmRLEFramesInfo.h"
#include "gdcmJPEGFragmentsInfo.h"

#include <stdio.h> //sscanf
#include <vector>

namespace gdcm 
{
//-----------------------------------------------------------------------------
// Constructor / Destructor
/**
 * \brief Constructor used when we want to generate dicom files from scratch
 */
File::File():
   Document()
{
   RLEInfo  = new RLEFramesInfo;
   JPEGInfo = new JPEGFragmentsInfo;
   GrPixel  = 0x7fe0;  // to avoid further troubles
   NumPixel = 0x0010;
}

/**
 * \brief  Constructor (DEPRECATED : temporaryly kept not to break the API)
 * @param  filename name of the file whose header we want to analyze
 * @deprecated do not use any longer
 */
File::File( std::string const &filename )
     :Document( )
{    
   RLEInfo  = new RLEFramesInfo;
   JPEGInfo = new JPEGFragmentsInfo;

   SetFileName( filename );
   Load( ); // gdcm::Document is first Loaded, then the 'File part'
}
/**
 * \brief   Loader. (DEPRECATED :  temporaryly kept not to break the API)
 * @param   fileName file to be open for parsing
 * @return false if file cannot be open or no swap info was found,
 *         or no tag was found.
 * @deprecated Use the Load() [ + SetLoadMode() ] + SetFileName() functions instead
 */
bool File::Load( std::string const &fileName ) 
{
   SetFileName( fileName );
   if ( ! this->Document::Load( ) )
      return false;

   return DoTheLoadingJob( );
}


/**
 * \brief   Canonical destructor.
 */
File::~File ()
{
   if ( RLEInfo )
      delete RLEInfo;
   if ( JPEGInfo )
      delete JPEGInfo;
}

//-----------------------------------------------------------------------------
// Public
/**
 * \brief   Loader  
 * @return false if file cannot be open or no swap info was found,
 *         or no tag was found.
 */
bool File::Load( ) 
{
   if ( ! this->Document::Load( ) )
      return false;

    return DoTheLoadingJob( );   
}

/**
 * \brief   Does the Loading Job (internal use only)
 * @return false if file cannot be open or no swap info was found,
 *         or no tag was found.
 */
bool File::DoTheLoadingJob( ) 
{
   // for some ACR-NEMA images GrPixel, NumPixel is *not* 7fe0,0010
   // We may encounter the 'RETired' (0x0028, 0x0200) tag
   // (Image Location") . This entry contains the number of
   // the group that contains the pixel data (hence the "Pixel Data"
   // is found by indirection through the "Image Location").
   // Inside the group pointed by "Image Location" the searched element
   // is conventionally the element 0x0010 (when the norm is respected).
   // When the "Image Location" is missing we default to group 0x7fe0.
   // Note: this IS the right place for the code
 
   // Image Location
   const std::string &imgLocation = GetEntryValue(0x0028, 0x0200);
   if ( imgLocation == GDCM_UNFOUND )
   {
      // default value
      GrPixel = 0x7fe0;
   }
   else
   {
      GrPixel = (uint16_t) atoi( imgLocation.c_str() );
   }   

   // sometimes Image Location value doesn't follow
   // the supposed processor endianness.
   // see gdcmData/cr172241.dcm
   if ( GrPixel == 0xe07f )
   {
      GrPixel = 0x7fe0;
   }

   if ( GrPixel != 0x7fe0 )
   {
      // This is a kludge for old dirty Philips imager.
      NumPixel = 0x1010;
   }
   else
   {
      NumPixel = 0x0010;
   }

   // Now, we know GrPixel and NumPixel.
   // Let's create a VirtualDictEntry to allow a further VR modification
   // and force VR to match with BitsAllocated.
   DocEntry *entry = GetDocEntry(GrPixel, NumPixel); 
   if ( entry != 0 )
   {
      // Compute the RLE or JPEG info
      OpenFile();
      std::string ts = GetTransferSyntax();
      Fp->seekg( entry->GetOffset(), std::ios::beg );
      if ( Global::GetTS()->IsRLELossless(ts) ) 
         ComputeRLEInfo();
      else if ( Global::GetTS()->IsJPEG(ts) )
         ComputeJPEGFragmentInfo();
      CloseFile();

      // Create a new BinEntry to change the the DictEntry
      // The changed DictEntry will have 
      // - a correct PixelVR OB or OW)
      // - a VM to "PXL"
      // - the name to "Pixel Data"
      BinEntry *oldEntry = dynamic_cast<BinEntry *>(entry);
      if(oldEntry)
      {
         std::string PixelVR;
         // 8 bits allocated is a 'O Bytes' , as well as 24 (old ACR-NEMA RGB)
         // more than 8 (i.e 12, 16) is a 'O Words'
         if ( GetBitsAllocated() == 8 || GetBitsAllocated() == 24 ) 
            PixelVR = "OB";
         else
            PixelVR = "OW";

         // Change only made if usefull
         if( PixelVR != oldEntry->GetVR() )
         {
            DictEntry* newDict = NewVirtualDictEntry(GrPixel,NumPixel,
                                                     PixelVR,"1","Pixel Data");

            BinEntry *newEntry = new BinEntry(newDict);
            newEntry->Copy(entry);
            newEntry->SetBinArea(oldEntry->GetBinArea(),oldEntry->IsSelfArea());
            oldEntry->SetSelfArea(false);

            RemoveEntry(oldEntry);
            AddEntry(newEntry);
         }
      }
   }
   return true;
}

/**
 * \brief  This predicate, based on hopefully reasonable heuristics,
 *         decides whether or not the current File was properly parsed
 *         and contains the mandatory information for being considered as
 *         a well formed and usable Dicom/Acr File.
 * @return true when File is the one of a reasonable Dicom/Acr file,
 *         false otherwise. 
 */
bool File::IsReadable()
{
   if( !Document::IsReadable() )
   {
      return false;
   }

   const std::string &res = GetEntryValue(0x0028, 0x0005);
   if ( res != GDCM_UNFOUND && atoi(res.c_str()) > 4 )
   {
      return false; // Image Dimensions
   }
   if ( !GetDocEntry(0x0028, 0x0100) )
   {
      return false; // "Bits Allocated"
   }
   if ( !GetDocEntry(0x0028, 0x0101) )
   {
      return false; // "Bits Stored"
   }
   if ( !GetDocEntry(0x0028, 0x0102) )
   {
      return false; // "High Bit"
   }
   if ( !GetDocEntry(0x0028, 0x0103) )
   {
      return false; // "Pixel Representation" i.e. 'Sign'
   }

   return true;
}

/**
 * \brief gets the info from 0020,0013 : Image Number else 0.
 * @return image number
 */
int File::GetImageNumber()
{
   // The function i atoi() takes the address of an area of memory as
   // parameter and converts the string stored at that location to an integer
   // using the external decimal to internal binary conversion rules. This may
   // be preferable to sscanf() since atoi() is a much smaller, simpler and
   // faster function. sscanf() can do all possible conversions whereas
   // atoi() can only do single decimal integer conversions.
   //0020 0013 IS REL Image Number
   std::string strImNumber = GetEntryValue(0x0020,0x0013);
   if ( strImNumber != GDCM_UNFOUND )
   {
      return atoi( strImNumber.c_str() );
   }
   return 0;   //Hopeless
}

/**
 * \brief gets the info from 0008,0060 : Modality
 * @return Modality Type
 */
ModalityType File::GetModality()
{
   // 0008 0060 CS ID Modality
   std::string strModality = GetEntryValue(0x0008,0x0060);
   if ( strModality != GDCM_UNFOUND )
   {
           if ( strModality.find("AU") < strModality.length()) return AU;
      else if ( strModality.find("AS") < strModality.length()) return AS;
      else if ( strModality.find("BI") < strModality.length()) return BI;
      else if ( strModality.find("CF") < strModality.length()) return CF;
      else if ( strModality.find("CP") < strModality.length()) return CP;
      else if ( strModality.find("CR") < strModality.length()) return CR;
      else if ( strModality.find("CT") < strModality.length()) return CT;
      else if ( strModality.find("CS") < strModality.length()) return CS;
      else if ( strModality.find("DD") < strModality.length()) return DD;
      else if ( strModality.find("DF") < strModality.length()) return DF;
      else if ( strModality.find("DG") < strModality.length()) return DG;
      else if ( strModality.find("DM") < strModality.length()) return DM;
      else if ( strModality.find("DS") < strModality.length()) return DS;
      else if ( strModality.find("DX") < strModality.length()) return DX;
      else if ( strModality.find("ECG") < strModality.length()) return ECG;
      else if ( strModality.find("EPS") < strModality.length()) return EPS;
      else if ( strModality.find("FA") < strModality.length()) return FA;
      else if ( strModality.find("FS") < strModality.length()) return FS;
      else if ( strModality.find("HC") < strModality.length()) return HC;
      else if ( strModality.find("HD") < strModality.length()) return HD;
      else if ( strModality.find("LP") < strModality.length()) return LP;
      else if ( strModality.find("LS") < strModality.length()) return LS;
      else if ( strModality.find("MA") < strModality.length()) return MA;
      else if ( strModality.find("MR") < strModality.length()) return MR;
      else if ( strModality.find("NM") < strModality.length()) return NM;
      else if ( strModality.find("OT") < strModality.length()) return OT;
      else if ( strModality.find("PT") < strModality.length()) return PT;
      else if ( strModality.find("RF") < strModality.length()) return RF;
      else if ( strModality.find("RG") < strModality.length()) return RG;
      else if ( strModality.find("RTDOSE")   < strModality.length()) return RTDOSE;
      else if ( strModality.find("RTIMAGE")  < strModality.length()) return RTIMAGE;
      else if ( strModality.find("RTPLAN")   < strModality.length()) return RTPLAN;
      else if ( strModality.find("RTSTRUCT") < strModality.length()) return RTSTRUCT;
      else if ( strModality.find("SM") < strModality.length()) return SM;
      else if ( strModality.find("ST") < strModality.length()) return ST;
      else if ( strModality.find("TG") < strModality.length()) return TG;
      else if ( strModality.find("US") < strModality.length()) return US;
      else if ( strModality.find("VF") < strModality.length()) return VF;
      else if ( strModality.find("XA") < strModality.length()) return XA;
      else if ( strModality.find("XC") < strModality.length()) return XC;

      else
      {
         /// \todo throw error return value ???
         /// specified <> unknown in our database
         return Unknow;
      }
   }
   return Unknow;
}

/**
 * \brief   Retrieve the number of columns of image.
 * @return  The encountered size when found, 0 by default.
 *          0 means the file is NOT USABLE. The caller will have to check
 */
int File::GetXSize()
{
   const std::string &strSize = GetEntryValue(0x0028,0x0011);
   if ( strSize == GDCM_UNFOUND )
   {
      return 0;
   }
   return atoi( strSize.c_str() );
}

/**
 * \brief   Retrieve the number of lines of image.
 * \warning The defaulted value is 1 as opposed to File::GetXSize()
 * @return  The encountered size when found, 1 by default 
 *          (The ACR-NEMA file contains a Signal, not an Image).
 */
int File::GetYSize()
{
   const std::string &strSize = GetEntryValue(0x0028,0x0010);
   if ( strSize != GDCM_UNFOUND )
   {
      return atoi( strSize.c_str() );
   }
   if ( IsDicomV3() )
   {
      return 0;
   }

   // The Rows (0028,0010) entry was optional for ACR/NEMA. It might
   // hence be a signal (1D image). So we default to 1:
   return 1;
}

/**
 * \brief   Retrieve the number of planes of volume or the number
 *          of frames of a multiframe.
 * \warning When present we consider the "Number of Frames" as the third
 *          dimension. When Missing we consider the third dimension as
 *          being the ACR-NEMA "Planes" tag content.
 * @return  The encountered size when found, 1 by default (single image).
 */
int File::GetZSize()
{
   // Both  DicomV3 and ACR/Nema consider the "Number of Frames"
   // as the third dimension.
   const std::string &strSize = GetEntryValue(0x0028,0x0008);
   if ( strSize != GDCM_UNFOUND )
   {
      return atoi( strSize.c_str() );
   }

   // We then consider the "Planes" entry as the third dimension 
   const std::string &strSize2 = GetEntryValue(0x0028,0x0012);
   if ( strSize2 != GDCM_UNFOUND )
   {
      return atoi( strSize2.c_str() );
   }

   return 1;
}

/**
  * \brief gets the info from 0028,0030 : Pixel Spacing
  *             else 1.0
  * @return X dimension of a pixel
  */
float File::GetXSpacing()
{
   float xspacing = 1.0;
   float yspacing = 1.0;
   const std::string &strSpacing = GetEntryValue(0x0028,0x0030);

   if( strSpacing == GDCM_UNFOUND )
   {
      gdcmWarningMacro( "Unfound Pixel Spacing (0028,0030)" );
      return 1.;
   }

   int nbValues;
   if( ( nbValues = sscanf( strSpacing.c_str(), 
         "%f \\%f ", &yspacing, &xspacing)) != 2 )
   {
      // if no values, xspacing is set to 1.0
      if( nbValues == 0 )
         xspacing = 1.0;
      // if single value is found, xspacing is defaulted to yspacing
      if( nbValues == 1 )
         xspacing = yspacing;

      if ( xspacing == 0.0 )
         xspacing = 1.0;

      return xspacing;

   }

   // to avoid troubles with David Clunie's-like images
   if ( xspacing == 0. && yspacing == 0.)
      return 1.;

   if ( xspacing == 0.)
   {
      gdcmWarningMacro("gdcmData/CT-MONO2-8-abdo.dcm problem");
      // seems to be a bug in the header ...
      nbValues = sscanf( strSpacing.c_str(), "%f \\0\\%f ", &yspacing, &xspacing);
      gdcmAssertMacro( nbValues == 2 );
   }

   return xspacing;
}

/**
  * \brief gets the info from 0028,0030 : Pixel Spacing
  *             else 1.0
  * @return Y dimension of a pixel
  */
float File::GetYSpacing()
{
   float yspacing = 1.;
   std::string strSpacing = GetEntryValue(0x0028,0x0030);
  
   if ( strSpacing == GDCM_UNFOUND )
   {
      gdcmWarningMacro("Unfound Pixel Spacing (0028,0030)");
      return 1.;
    }

   // if sscanf cannot read any float value, it won't affect yspacing
   int nbValues = sscanf( strSpacing.c_str(), "%f ", &yspacing);

   // if no values, yspacing is set to 1.0
   if( nbValues == 0 )
      yspacing = 1.0;

   if ( yspacing == 0.0 )
      yspacing = 1.0;

   return yspacing;
} 

/**
 * \brief gets the info from 0018,0088 : Space Between Slices
 *                 else from 0018,0050 : Slice Thickness
 *                 else 1.0
 * @return Z dimension of a voxel-to be
 */
float File::GetZSpacing()
{
   // Spacing Between Slices : distance entre le milieu de chaque coupe
   // Les coupes peuvent etre :
   //   jointives     (Spacing between Slices = Slice Thickness)
   //   chevauchantes (Spacing between Slices < Slice Thickness)
   //   disjointes    (Spacing between Slices > Slice Thickness)
   // Slice Thickness : epaisseur de tissus sur laquelle est acquis le signal
   //   ca interesse le physicien de l'IRM, pas le visualisateur de volumes ...
   //   Si le Spacing Between Slices est Missing, 
   //   on suppose que les coupes sont jointives
   
   const std::string &strSpacingBSlices = GetEntryValue(0x0018,0x0088);

   if ( strSpacingBSlices == GDCM_UNFOUND )
   {
      gdcmWarningMacro("Unfound Spacing Between Slices (0018,0088)");
      const std::string &strSliceThickness = GetEntryValue(0x0018,0x0050);       
      if ( strSliceThickness == GDCM_UNFOUND )
      {
         gdcmWarningMacro("Unfound Slice Thickness (0018,0050)");
         return 1.;
      }
      else
      {
         // if no 'Spacing Between Slices' is found, 
         // we assume slices join together
         // (no overlapping, no interslice gap)
         // if they don't, we're fucked up
         return (float)atof( strSliceThickness.c_str() );
      }
   }
   //else
   return (float)atof( strSpacingBSlices.c_str() );
}

/**
 * \brief gets the info from 0020,0032 : Image Position Patient
 *                 else from 0020,0030 : Image Position (RET)
 *                 else 0.
 * @return up-left image corner X position
 */
float File::GetXOrigin()
{
   float xImPos, yImPos, zImPos;  
   std::string strImPos = GetEntryValue(0x0020,0x0032);

   if ( strImPos == GDCM_UNFOUND )
   {
      gdcmWarningMacro( "Unfound Image Position Patient (0020,0032)");
      strImPos = GetEntryValue(0x0020,0x0030); // For ACR-NEMA images
      if ( strImPos == GDCM_UNFOUND )
      {
         gdcmWarningMacro( "Unfound Image Position (RET) (0020,0030)");
         return 0.;
      }
   }

   if( sscanf( strImPos.c_str(), "%f \\%f \\%f ", &xImPos, &yImPos, &zImPos) != 3 )
   {
      return 0.;
   }

   return xImPos;
}

/**
 * \brief gets the info from 0020,0032 : Image Position Patient
 *                 else from 0020,0030 : Image Position (RET)
 *                 else 0.
 * @return up-left image corner Y position
 */
float File::GetYOrigin()
{
   float xImPos, yImPos, zImPos;
   std::string strImPos = GetEntryValue(0x0020,0x0032);

   if ( strImPos == GDCM_UNFOUND)
   {
      gdcmWarningMacro( "Unfound Image Position Patient (0020,0032)");
      strImPos = GetEntryValue(0x0020,0x0030); // For ACR-NEMA images
      if ( strImPos == GDCM_UNFOUND )
      {
         gdcmWarningMacro( "Unfound Image Position (RET) (0020,0030)");
         return 0.;
      }  
   }

   if( sscanf( strImPos.c_str(), "%f \\%f \\%f ", &xImPos, &yImPos, &zImPos) != 3 )
   {
      return 0.;
   }

   return yImPos;
}

/**
 * \brief gets the info from 0020,0032 : Image Position Patient
 *                 else from 0020,0030 : Image Position (RET)
 *                 else from 0020,1041 : Slice Location
 *                 else from 0020,0050 : Location
 *                 else 0.
 * @return up-left image corner Z position
 */
float File::GetZOrigin()
{
   float xImPos, yImPos, zImPos; 
   std::string strImPos = GetEntryValue(0x0020,0x0032);

   if ( strImPos != GDCM_UNFOUND )
   {
      if( sscanf( strImPos.c_str(), "%f \\%f \\%f ", &xImPos, &yImPos, &zImPos) != 3)
      {
         gdcmWarningMacro( "Wrong Image Position Patient (0020,0032)");
         return 0.;  // bug in the element 0x0020,0x0032
      }
      else
      {
         return zImPos;
      }
   }

   strImPos = GetEntryValue(0x0020,0x0030); // For ACR-NEMA images
   if ( strImPos != GDCM_UNFOUND )
   {
      if( sscanf( strImPos.c_str(), 
          "%f \\%f \\%f ", &xImPos, &yImPos, &zImPos ) != 3 )
      {
         gdcmWarningMacro( "Wrong Image Position (RET) (0020,0030)");
         return 0.;  // bug in the element 0x0020,0x0032
      }
      else
      {
         return zImPos;
      }
   }

   std::string strSliceLocation = GetEntryValue(0x0020,0x1041); // for *very* old ACR-NEMA images
   if ( strSliceLocation != GDCM_UNFOUND )
   {
      if( sscanf( strSliceLocation.c_str(), "%f ", &zImPos) != 1)
      {
         gdcmWarningMacro( "Wrong Slice Location (0020,1041)");
         return 0.;  // bug in the element 0x0020,0x1041
      }
      else
      {
         return zImPos;
      }
   }
   gdcmWarningMacro( "Unfound Slice Location (0020,1041)");

   std::string strLocation = GetEntryValue(0x0020,0x0050);
   if ( strLocation != GDCM_UNFOUND )
   {
      if( sscanf( strLocation.c_str(), "%f ", &zImPos) != 1)
      {
         gdcmWarningMacro( "Wrong Location (0020,0050)");
         return 0.;  // bug in the element 0x0020,0x0050
      }
      else
      {
         return zImPos;
      }
   }
   gdcmWarningMacro( "Unfound Location (0020,0050)");  

   return 0.; // Hopeless
}

/**
  * \brief gets the info from 0020,0037 : Image Orientation Patient
  * (needed to organize DICOM files based on their x,y,z position)
  * @param iop adress of the (6)float aray to receive values
  * @return cosines of image orientation patient
  */
void File::GetImageOrientationPatient( float iop[6] )
{
   std::string strImOriPat;
   //iop is supposed to be float[6]
   iop[0] = iop[4] = 1.;
   iop[2] = iop[3] = iop[5] = 0.;

   // 0020 0037 DS REL Image Orientation (Patient)
   if ( (strImOriPat = GetEntryValue(0x0020,0x0037)) != GDCM_UNFOUND )
   {
      if( sscanf( strImOriPat.c_str(), "%f \\ %f \\%f \\%f \\%f \\%f ", 
          &iop[0], &iop[1], &iop[2], &iop[3], &iop[4], &iop[5]) != 6 )
      {
         gdcmWarningMacro( "Wrong Image Orientation Patient (0020,0037). Less than 6 values were found." );
      }
   }
   //For ACR-NEMA
   // 0020 0035 DS REL Image Orientation (RET)
   else if ( (strImOriPat = GetEntryValue(0x0020,0x0035)) != GDCM_UNFOUND )
   {
      if( sscanf( strImOriPat.c_str(), "%f \\ %f \\%f \\%f \\%f \\%f ", 
          &iop[0], &iop[1], &iop[2], &iop[3], &iop[4], &iop[5]) != 6 )
      {
         gdcmWarningMacro( "wrong Image Orientation Patient (0020,0035). Less than 6 values were found." );
      }
   }
}

/**
 * \brief   Retrieve the number of Bits Stored (actually used)
 *          (as opposed to number of Bits Allocated)
 * @return  The encountered number of Bits Stored, 0 by default.
 *          0 means the file is NOT USABLE. The caller has to check it !
 */
int File::GetBitsStored()
{
   std::string strSize = GetEntryValue( 0x0028, 0x0101 );
   if ( strSize == GDCM_UNFOUND )
   {
      gdcmWarningMacro("(0028,0101) is supposed to be mandatory");
      return 0;  // It's supposed to be mandatory
                 // the caller will have to check
   }
   return atoi( strSize.c_str() );
}

/**
 * \brief   Retrieve the number of Bits Allocated
 *          (8, 12 -compacted ACR-NEMA files, 16, ...)
 * @return  The encountered number of Bits Allocated, 0 by default.
 *          0 means the file is NOT USABLE. The caller has to check it !
 */
int File::GetBitsAllocated()
{
   std::string strSize = GetEntryValue(0x0028,0x0100);
   if ( strSize == GDCM_UNFOUND  )
   {
      gdcmWarningMacro( "(0028,0100) is supposed to be mandatory");
      return 0; // It's supposed to be mandatory
                // the caller will have to check
   }
   return atoi( strSize.c_str() );
}

/**
 * \brief   Retrieve the high bit position.
 * \warning The method defaults to 0 when information is Missing.
 *          The responsability of checking this value is left to the caller.
 * @return  The high bit positin when present. 0 when Missing.
 */
int File::GetHighBitPosition()
{
   std::string strSize = GetEntryValue( 0x0028, 0x0102 );
   if ( strSize == GDCM_UNFOUND )
   {
      gdcmWarningMacro( "(0028,0102) is supposed to be mandatory");
      return 0;
   }
   return atoi( strSize.c_str() );
}

/**
 * \brief   Retrieve the number of Samples Per Pixel
 *          (1 : gray level, 3 : RGB -1 or 3 Planes-)
 * @return  The encountered number of Samples Per Pixel, 1 by default.
 *          (Gray level Pixels)
 */
int File::GetSamplesPerPixel()
{
   const std::string &strSize = GetEntryValue(0x0028,0x0002);
   if ( strSize == GDCM_UNFOUND )
   {
      gdcmWarningMacro( "(0028,0002) is supposed to be mandatory");
      return 1; // Well, it's supposed to be mandatory ...
                // but sometimes it's missing : *we* assume Gray pixels
   }
   return atoi( strSize.c_str() );
}

/**
 * \brief   Retrieve the Planar Configuration for RGB images
 *          (0 : RGB Pixels , 1 : R Plane + G Plane + B Plane)
 * @return  The encountered Planar Configuration, 0 by default.
 */
int File::GetPlanarConfiguration()
{
   std::string strSize = GetEntryValue(0x0028,0x0006);
   if ( strSize == GDCM_UNFOUND )
   {
      gdcmWarningMacro( "Not found : Planar Configuration (0028,0006)");
      return 0;
   }
   return atoi( strSize.c_str() );
}

/**
 * \brief   Return the size (in bytes) of a single pixel of data.
 * @return  The size in bytes of a single pixel of data; 0 by default
 *          0 means the file is NOT USABLE; the caller will have to check
 */
int File::GetPixelSize()
{
   // 0028 0100 US IMG Bits Allocated
   // (in order no to be messed up by old RGB images)
   //   if (File::GetEntryValue(0x0028,0x0100) == "24")
   //      return 3;

   std::string pixelType = GetPixelType();
   if ( pixelType ==  "8U" || pixelType == "8S" )
   {
      return 1;
   }
   if ( pixelType == "16U" || pixelType == "16S")
   {
      return 2;
   }
   if ( pixelType == "32U" || pixelType == "32S")
   {
      return 4;
   }
   if ( pixelType == "FD" )
   {
      return 8;
   }
   gdcmWarningMacro( "Unknown pixel type");
   return 0;
}

/**
 * \brief   Build the Pixel Type of the image.
 *          Possible values are:
 *          - 8U  unsigned  8 bit,
 *          - 8S    signed  8 bit,
 *          - 16U unsigned 16 bit,
 *          - 16S   signed 16 bit,
 *          - 32U unsigned 32 bit,
 *          - 32S   signed 32 bit,
 *          - FD floating double 64 bits (Not kosher DICOM, but so usefull!)
 * \warning 12 bit images appear as 16 bit.
 *          24 bit images appear as 8 bit
 * @return  0S if nothing found. NOT USABLE file. The caller has to check
 */
std::string File::GetPixelType()
{
   std::string bitsAlloc = GetEntryValue(0x0028, 0x0100); // Bits Allocated
   if ( bitsAlloc == GDCM_UNFOUND )
   {
      gdcmWarningMacro( "Missing  Bits Allocated (0028,0100)");
      bitsAlloc = "16"; // default and arbitrary value, not to polute the output
   }

   if ( bitsAlloc == "64" )
   {
      return "FD";
   }
   else if ( bitsAlloc == "12" )
   {
      // It will be unpacked
      bitsAlloc = "16";
   }
   else if ( bitsAlloc == "24" )
   {
      // (in order no to be messed up
      bitsAlloc = "8";  // by old RGB images)
   }

   std::string sign = GetEntryValue(0x0028, 0x0103);//"Pixel Representation"

   if (sign == GDCM_UNFOUND )
   {
      gdcmWarningMacro( "Missing Pixel Representation (0028,0103)");
      sign = "U"; // default and arbitrary value, not to polute the output
   }
   else if ( sign == "0" )
   {
      sign = "U";
   }
   else
   {
      sign = "S";
   }
   return bitsAlloc + sign;
}

/**
 * \brief   Check whether the pixels are signed or UNsigned data.
 * \warning The method defaults to false (UNsigned) when information is Missing.
 *          The responsability of checking this value is left to the caller.
 * @return  True when signed, false when UNsigned
 */
bool File::IsSignedPixelData()
{
   std::string strSize = GetEntryValue( 0x0028, 0x0103 );
   if ( strSize == GDCM_UNFOUND )
   {
      gdcmWarningMacro( "(0028,0103) is supposed to be mandatory");
      return false;
   }
   int sign = atoi( strSize.c_str() );
   if ( sign == 0 ) 
   {
      return false;
   }
   return true;
}

/**
 * \brief   Check whether this a monochrome picture or not by accessing
 *          the "Photometric Interpretation" tag ( 0x0028, 0x0004 ).
 * @return  true when "MONOCHROME1" or "MONOCHROME2". False otherwise.
 */
bool File::IsMonochrome()
{
   const std::string &PhotometricInterp = GetEntryValue( 0x0028, 0x0004 );
   if (  Util::DicomStringEqual(PhotometricInterp, "MONOCHROME1")
      || Util::DicomStringEqual(PhotometricInterp, "MONOCHROME2") )
   {
      return true;
   }
   if ( PhotometricInterp == GDCM_UNFOUND )
   {
      gdcmWarningMacro( "Not found : Photometric Interpretation (0028,0004)");
   }
   return false;
}

/**
 * \brief   Check whether this a "PALETTE COLOR" picture or not by accessing
 *          the "Photometric Interpretation" tag ( 0x0028, 0x0004 ).
 * @return  true when "PALETTE COLOR". False otherwise.
 */
bool File::IsPaletteColor()
{
   std::string PhotometricInterp = GetEntryValue( 0x0028, 0x0004 );
   if (   PhotometricInterp == "PALETTE COLOR " )
   {
      return true;
   }
   if ( PhotometricInterp == GDCM_UNFOUND )
   {
      gdcmWarningMacro( "Not found : Palette color (0028,0004)");
   }
   return false;
}

/**
 * \brief   Check whether this a "YBR_FULL" color picture or not by accessing
 *          the "Photometric Interpretation" tag ( 0x0028, 0x0004 ).
 * @return  true when "YBR_FULL". False otherwise.
 */
bool File::IsYBRFull()
{
   std::string PhotometricInterp = GetEntryValue( 0x0028, 0x0004 );
   if (   PhotometricInterp == "YBR_FULL" )
   {
      return true;
   }
   if ( PhotometricInterp == GDCM_UNFOUND )
   {
      gdcmWarningMacro( "Not found : YBR Full (0028,0004)");
   }
   return false;
}

/**
  * \brief tells us if LUT are used
  * \warning Right now, 'Segmented xxx Palette Color Lookup Table Data'
  *          are NOT considered as LUT, since nobody knows
  *          how to deal with them
  *          Please warn me if you know sbdy that *does* know ... jprx
  * @return true if LUT Descriptors and LUT Tables were found 
  */
bool File::HasLUT()
{
   // Check the presence of the LUT Descriptors, and LUT Tables    
   // LutDescriptorRed    
   if ( !GetDocEntry(0x0028,0x1101) )
   {
      return false;
   }
   // LutDescriptorGreen 
   if ( !GetDocEntry(0x0028,0x1102) )
   {
      return false;
   }
   // LutDescriptorBlue 
   if ( !GetDocEntry(0x0028,0x1103) )
   {
      return false;
   }
   // Red Palette Color Lookup Table Data
   if ( !GetDocEntry(0x0028,0x1201) )
   {
      return false;
   }
   // Green Palette Color Lookup Table Data       
   if ( !GetDocEntry(0x0028,0x1202) )
   {
      return false;
   }
   // Blue Palette Color Lookup Table Data      
   if ( !GetDocEntry(0x0028,0x1203) )
   {
      return false;
   }

   // FIXME : (0x0028,0x3006) : LUT Data (CTX dependent)
   //         NOT taken into account, but we don't know how to use it ...   
   return true;
}

/**
  * \brief gets the info from 0028,1101 : Lookup Table Desc-Red
  *             else 0
  * @return Lookup Table number of Bits , 0 by default
  *          when (0028,0004),Photometric Interpretation = [PALETTE COLOR ]
  * @ return bit number of each LUT item 
  */
int File::GetLUTNbits()
{
   std::vector<std::string> tokens;
   int lutNbits;

   //Just hope Lookup Table Desc-Red = Lookup Table Desc-Red
   //                                = Lookup Table Desc-Blue
   // Consistency already checked in GetLUTLength
   std::string lutDescription = GetEntryValue(0x0028,0x1101);
   if ( lutDescription == GDCM_UNFOUND )
   {
      return 0;
   }

   tokens.clear(); // clean any previous value
   Util::Tokenize ( lutDescription, tokens, "\\" );
   //LutLength=atoi(tokens[0].c_str());
   //LutDepth=atoi(tokens[1].c_str());

   lutNbits = atoi( tokens[2].c_str() );
   tokens.clear();

   return lutNbits;
}

/**
 *\brief gets the info from 0028,1052 : Rescale Intercept
 * @return Rescale Intercept
 */
float File::GetRescaleIntercept()
{
   float resInter = 0.;
   /// 0028 1052 DS IMG Rescale Intercept
   const std::string &strRescInter = GetEntryValue(0x0028,0x1052);
   if ( strRescInter != GDCM_UNFOUND )
   {
      if( sscanf( strRescInter.c_str(), "%f ", &resInter) != 1 )
      {
         // bug in the element 0x0028,0x1052
         gdcmWarningMacro( "Rescale Intercept (0028,1052) is empty." );
      }
   }

   return resInter;
}

/**
 *\brief   gets the info from 0028,1053 : Rescale Slope
 * @return Rescale Slope
 */
float File::GetRescaleSlope()
{
   float resSlope = 1.;
   //0028 1053 DS IMG Rescale Slope
   std::string strRescSlope = GetEntryValue(0x0028,0x1053);
   if ( strRescSlope != GDCM_UNFOUND )
   {
      if( sscanf( strRescSlope.c_str(), "%f ", &resSlope) != 1)
      {
         // bug in the element 0x0028,0x1053
         gdcmWarningMacro( "Rescale Slope (0028,1053) is empty.");
      }
   }

   return resSlope;
}

/**
 * \brief This function is intended to user who doesn't want 
 *   to have to manage a LUT and expects to get an RBG Pixel image
 *   (or a monochrome one ...) 
 * \warning to be used with GetImagePixels()
 * @return 1 if Gray level, 3 if Color (RGB, YBR or PALETTE COLOR)
 */
int File::GetNumberOfScalarComponents()
{
   if ( GetSamplesPerPixel() == 3 )
   {
      return 3;
   }
      
   // 0028 0100 US IMG Bits Allocated
   // (in order no to be messed up by old RGB images)
   if ( GetEntryValue(0x0028,0x0100) == "24" )
   {
      return 3;
   }
       
   std::string strPhotometricInterpretation = GetEntryValue(0x0028,0x0004);

   if ( ( strPhotometricInterpretation == "PALETTE COLOR ") )
   {
      if ( HasLUT() )// PALETTE COLOR is NOT enough
      {
         return 3;
      }
      else
      {
         return 1;
      }
   }

   // beware of trailing space at end of string      
   // DICOM tags are never of odd length
   if ( strPhotometricInterpretation == GDCM_UNFOUND   || 
        Util::DicomStringEqual(strPhotometricInterpretation, "MONOCHROME1") ||
        Util::DicomStringEqual(strPhotometricInterpretation, "MONOCHROME2") )
   {
      return 1;
   }
   else
   {
      // we assume that *all* kinds of YBR are dealt with
      return 3;
   }
}

/**
 * \brief This function is intended to user that DOESN'T want 
 *  to get RGB pixels image when it's stored as a PALETTE COLOR image
 *   - the (vtk) user is supposed to know how deal with LUTs - 
 * \warning to be used with GetImagePixelsRaw()
 * @return 1 if Gray level, 3 if Color (RGB or YBR - NOT 'PALETTE COLOR' -)
 */
int File::GetNumberOfScalarComponentsRaw()
{
   // 0028 0100 US IMG Bits Allocated
   // (in order no to be messed up by old RGB images)
   if ( File::GetEntryValue(0x0028,0x0100) == "24" )
   {
      return 3;
   }

   // we assume that *all* kinds of YBR are dealt with
   return GetSamplesPerPixel();
}

/**
 * \brief   Recover the offset (from the beginning of the file) 
 *          of *image* pixels (not *icone image* pixels, if any !)
 * @return Pixel Offset
 */
size_t File::GetPixelOffset()
{
   DocEntry *pxlElement = GetDocEntry(GrPixel, NumPixel);
   if ( pxlElement )
   {
      return pxlElement->GetOffset();
   }
   else
   {
      gdcmDebugMacro( "Big trouble : Pixel Element ("
                      << std::hex << GrPixel<<","<< NumPixel<< ") NOT found" );
      return 0;
   }
}

/**
 * \brief   Recover the pixel area length (in Bytes)
 * @return Pixel Element Length, as stored in the header
 *         (NOT the memory space necessary to hold the Pixels 
 *          -in case of embeded compressed image-)
 *         0 : NOT USABLE file. The caller has to check.
 */
size_t File::GetPixelAreaLength()
{
   DocEntry *pxlElement = GetDocEntry(GrPixel, NumPixel);
   if ( pxlElement )
   {
      return pxlElement->GetLength();
   }
   else
   {
      gdcmDebugMacro( "Big trouble : Pixel Element ("
                      << std::hex << GrPixel<<","<< NumPixel<< ") NOT found" );
      return 0;
   }
}

/**
 * \brief Adds the characteristics of a new element we want to anonymize
 *
 */
void File::AddAnonymizeElement (uint16_t group, uint16_t elem, 
                                std::string const &value) 
{ 
   Element el;
   el.Group = group;
   el.Elem  = elem;
   el.Value = value;
   AnonymizeList.push_back(el); 
}

/**
 * \brief Overwrites in the file the values of the DicomElements
 *       held in the list 
 */
void File::AnonymizeNoLoad()
{
   std::fstream *fp = new std::fstream(Filename.c_str(), 
                              std::ios::in | std::ios::out | std::ios::binary);
 
   // TODO : FIXME
   // how to white out disk space if longer than 50 ?
   
   
   gdcm::DocEntry *d;
   uint32_t offset;
   uint32_t lgth;
   uint32_t valLgth = 0;
   std::string *spaces;
   for (ListElements::iterator it = AnonymizeList.begin();  
                               it != AnonymizeList.end();
                             ++it)
   { 
      d = GetDocEntry( (*it).Group, (*it).Elem);

      if ( d == NULL)
         continue;

      if ( dynamic_cast<BinEntry *>(d)
        || dynamic_cast<SeqEntry *>(d) )
         continue;

      offset = d->GetOffset();
      lgth =   d->GetLength();
      if (valLgth < lgth)
      {
         spaces = new std::string( lgth-valLgth, ' ');
         (*it).Value = (*it).Value + *spaces;
         delete spaces;
      }
      fp->seekp( offset, std::ios::beg );
      fp->write( (*it).Value.c_str(), lgth );
     
   }
   fp->close();
   delete fp;
}

/**
 * \brief anonymize a File (removes Patient's personal info passed with
 *        AddAnonymizeElement()
 */
bool File::AnonymizeFile()
{
   // If Anonymisation list is empty, let's perform some basic anonymization
   if ( AnonymizeList.begin() == AnonymizeList.end() )
   {
      // If exist, replace by spaces
      SetValEntry ("  ",0x0010, 0x2154); // Telephone   
      SetValEntry ("  ",0x0010, 0x1040); // Adress
      SetValEntry ("  ",0x0010, 0x0020); // Patient ID

      DocEntry* patientNameHE = GetDocEntry (0x0010, 0x0010);
  
      if ( patientNameHE ) // we replace it by Study Instance UID (why not ?)
      {
         std::string studyInstanceUID =  GetEntryValue (0x0020, 0x000d);
         if ( studyInstanceUID != GDCM_UNFOUND )
         {
            SetValEntry(studyInstanceUID, 0x0010, 0x0010);
         }
         else
         {
            SetValEntry("anonymised", 0x0010, 0x0010);
         }
      }
   }
   else
   {
      gdcm::DocEntry *d;
      for (ListElements::iterator it = AnonymizeList.begin();  
                                  it != AnonymizeList.end();
                                ++it)
      {  
         d = GetDocEntry( (*it).Group, (*it).Elem);

         if ( d == NULL)
            continue;

         if ( dynamic_cast<BinEntry *>(d)
           || dynamic_cast<SeqEntry *>(d) )
            continue;

         SetValEntry ((*it).Value, (*it).Group, (*it).Elem);
      }
}

  // In order to make definitively impossible any further identification
  // remove or replace all the stuff that contains a Date

//0008 0012 DA ID Instance Creation Date
//0008 0020 DA ID Study Date
//0008 0021 DA ID Series Date
//0008 0022 DA ID Acquisition Date
//0008 0023 DA ID Content Date
//0008 0024 DA ID Overlay Date
//0008 0025 DA ID Curve Date
//0008 002a DT ID Acquisition Datetime
//0018 9074 DT ACQ Frame Acquisition Datetime
//0018 9151 DT ACQ Frame Reference Datetime
//0018 a002 DT ACQ Contribution Date Time
//0020 3403 SH REL Modified Image Date (RET)
//0032 0032 DA SDY Study Verified Date
//0032 0034 DA SDY Study Read Date
//0032 1000 DA SDY Scheduled Study Start Date
//0032 1010 DA SDY Scheduled Study Stop Date
//0032 1040 DA SDY Study Arrival Date
//0032 1050 DA SDY Study Completion Date
//0038 001a DA VIS Scheduled Admission Date
//0038 001c DA VIS Scheduled Discharge Date
//0038 0020 DA VIS Admitting Date
//0038 0030 DA VIS Discharge Date
//0040 0002 DA PRC Scheduled Procedure Step Start Date
//0040 0004 DA PRC Scheduled Procedure Step End Date
//0040 0244 DA PRC Performed Procedure Step Start Date
//0040 0250 DA PRC Performed Procedure Step End Date
//0040 2004 DA PRC Issue Date of Imaging Service Request
//0040 4005 DT PRC Scheduled Procedure Step Start Date and Time
//0040 4011 DT PRC Expected Completion Date and Time
//0040 a030 DT PRC Verification Date Time
//0040 a032 DT PRC Observation Date Time
//0040 a120 DT PRC DateTime
//0040 a121 DA PRC Date
//0040 a13a DT PRC Referenced Datetime
//0070 0082 DA ??? Presentation Creation Date
//0100 0420 DT ??? SOP Autorization Date and Time
//0400 0105 DT ??? Digital Signature DateTime
//2100 0040 DA PJ Creation Date
//3006 0008 DA SSET Structure Set Date
//3008 0024 DA ??? Treatment Control Point Date
//3008 0054 DA ??? First Treatment Date
//3008 0056 DA ??? Most Recent Treatment Date
//3008 0162 DA ??? Safe Position Exit Date
//3008 0166 DA ??? Safe Position Return Date
//3008 0250 DA ??? Treatment Date
//300a 0006 DA RT RT Plan Date
//300a 022c DA RT Air Kerma Rate Reference Date
//300e 0004 DA RT Review Date

   return true;
}

/**
 * \brief Performs some consistency checking on various 'File related' 
 *       (as opposed to 'DicomDir related') entries 
 *       then writes in a file all the (Dicom Elements) included the Pixels 
 * @param fileName file name to write to
 * @param filetype Type of the File to be written 
 *          (ACR, ExplicitVR, ImplicitVR)
 */
bool File::Write(std::string fileName, FileType filetype)
{
   std::ofstream *fp = new std::ofstream(fileName.c_str(), 
                                         std::ios::out | std::ios::binary);
   if (*fp == NULL)
   {
      gdcmWarningMacro("Failed to open (write) File: " << fileName.c_str());
      return false;
   }

   // Entry : 0002|0000 = group length -> recalculated
   ValEntry *e0002 = GetValEntry(0x0002,0x0000);
   if( e0002 )
   {
      itksys_ios::ostringstream sLen;
      sLen << ComputeGroup0002Length(filetype);
      e0002->SetValue(sLen.str());
   }

   // Bits Allocated
   if ( GetEntryValue(0x0028,0x0100) ==  "12")
   {
      SetValEntry("16", 0x0028,0x0100);
   }

   int i_lgPix = GetEntryLength(GrPixel, NumPixel);
   if (i_lgPix != -2)
   {
      // no (GrPixel, NumPixel) element
      std::string s_lgPix = Util::Format("%d", i_lgPix+12);
      s_lgPix = Util::DicomString( s_lgPix.c_str() );
      InsertValEntry(s_lgPix,GrPixel, 0x0000);
   }

   // FIXME : should be nice if we could move it to File
   //         (or in future gdcmPixelData class)

   // Drop Palette Color, if necessary
   if ( GetEntryValue(0x0028,0x0002).c_str()[0] == '3' )
   {
      // if SamplesPerPixel = 3, sure we don't need any LUT !   
      // Drop 0028|1101, 0028|1102, 0028|1103
      // Drop 0028|1201, 0028|1202, 0028|1203

      DocEntry *e = GetDocEntry(0x0028,0x01101);
      if (e)
      {
         RemoveEntryNoDestroy(e);
      }
      e = GetDocEntry(0x0028,0x1102);
      if (e)
      {
         RemoveEntryNoDestroy(e);
      }
      e = GetDocEntry(0x0028,0x1103);
      if (e)
      {
         RemoveEntryNoDestroy(e);
      }
      e = GetDocEntry(0x0028,0x01201);
      if (e)
      {
         RemoveEntryNoDestroy(e);
      }
      e = GetDocEntry(0x0028,0x1202);
      if (e)
      {
         RemoveEntryNoDestroy(e);
      }
      e = GetDocEntry(0x0028,0x1203);
      if (e)
      {
          RemoveEntryNoDestroy(e);
      }
   }

   Document::WriteContent(fp, filetype);

   fp->close();
   delete fp;

   return true;
}

//-----------------------------------------------------------------------------
// Protected
/**
 * \brief Initialize a default DICOM File that should contain all the
 *        field require by other reader. DICOM standard does not 
 *        explicitely defines those fields, heuristic has been choosen.
 *        This is not perfect as we are writting a CT image...
 */
void File::InitializeDefaultFile()
{
   std::string date = Util::GetCurrentDate();
   std::string time = Util::GetCurrentTime();
   std::string uid  = Util::CreateUniqueUID();
   std::string uidMedia = uid;
   std::string uidInst  = uid;
   std::string uidClass = Util::CreateUniqueUID();
   std::string uidStudy = Util::CreateUniqueUID();
   std::string uidSerie = Util::CreateUniqueUID();

   // Meta Element Group Length
   InsertValEntry("146 ",                      0x0002, 0x0000);
   // Media Storage SOP Class UID (CT Image Storage)
   InsertValEntry("1.2.840.10008.5.1.4.1.1.2", 0x0002, 0x0002);
   // Media Storage SOP Instance UID
   InsertValEntry(uidMedia.c_str(),            0x0002, 0x0003);
   // Transfer Syntax UID (Explicit VR Little Endian)
   InsertValEntry("1.2.840.10008.1.2.1 ",      0x0002, 0x0010);
   // META Implementation Class UID
   InsertValEntry(uidClass.c_str(),            0x0002, 0x0012);
   // Source Application Entity Title
   InsertValEntry("GDCM",                      0x0002, 0x0016);

   // Instance Creation Date
   InsertValEntry(date.c_str(),                0x0008, 0x0012);
   // Instance Creation Time
   InsertValEntry(time.c_str(),                0x0008, 0x0013);
   // SOP Class UID
   InsertValEntry("1.2.840.10008.5.1.4.1.1.2", 0x0008, 0x0016);
   // SOP Instance UID
   InsertValEntry(uidInst.c_str(),             0x0008, 0x0018);
   // Modality    
   InsertValEntry("CT",                        0x0008, 0x0060);
   // Manufacturer
   InsertValEntry("GDCM",                      0x0008, 0x0070);
   // Institution Name
   InsertValEntry("GDCM",                      0x0008, 0x0080);
   // Institution Address
   InsertValEntry("http://www-creatis.insa-lyon.fr/Public/Gdcm", 0x0008, 0x0081);

   // Patient's Name
   InsertValEntry("GDCM",                      0x0010, 0x0010);
   // Patient ID
   InsertValEntry("GDCMID",                    0x0010, 0x0020);

   // Study Instance UID
   InsertValEntry(uidStudy.c_str(),            0x0020, 0x000d);
   // Series Instance UID
   InsertValEntry(uidSerie.c_str(),            0x0020, 0x000e);
   // StudyID
   InsertValEntry("1",                         0x0020, 0x0010);
   // SeriesNumber
   InsertValEntry("1",                         0x0020, 0x0011);

   // Samples per pixel 1 or 3
   InsertValEntry("1",                         0x0028, 0x0002);
   // photochromatic interpretation
   InsertValEntry("MONOCHROME1",               0x0028, 0x0004);
   // nbRows
   InsertValEntry("0",                         0x0028, 0x0010);
   // nbCols
   InsertValEntry("0",                         0x0028, 0x0011);
   // BitsAllocated 8 or 12 or 16
   InsertValEntry("8",                         0x0028, 0x0100);
   // BitsStored    <= BitsAllocated
   InsertValEntry("8",                         0x0028, 0x0101);
   // HighBit       <= BitsAllocated - 1
   InsertValEntry("7",                         0x0028, 0x0102);
   // Pixel Representation 0(unsigned) or 1(signed)
   InsertValEntry("0",                         0x0028, 0x0103);

   // default value
   // Special case this is the image (not a string)
   GrPixel = 0x7fe0;
   NumPixel = 0x0010;
   InsertBinEntry(0, 0, GrPixel, NumPixel);
}

//-----------------------------------------------------------------------------
// Private
/**
 * \brief Parse pixel data from disk of [multi-]fragment RLE encoding.
 *        Compute the RLE extra information and store it in \ref RLEInfo
 *        for later pixel retrieval usage.
 */
void File::ComputeRLEInfo()
{
   std::string ts = GetTransferSyntax();
   if ( !Global::GetTS()->IsRLELossless(ts) ) 
   {
      return;
   }

   // Encoded pixel data: for the time being we are only concerned with
   // Jpeg or RLE Pixel data encodings.
   // As stated in PS 3.5-2003, section 8.2 p44:
   // "If sent in Encapsulated Format (i.e. other than the Native Format) the
   //  value representation OB is used".
   // Hence we expect an OB value representation. Concerning OB VR,
   // the section PS 3.5-2003, section A.4.c p 58-59, states:
   // "For the Value Representations OB and OW, the encoding shall meet the
   //   following specifications depending on the Data element tag:"
   //   [...snip...]
   //    - the first item in the sequence of items before the encoded pixel
   //      data stream shall be basic offset table item. The basic offset table
   //      item value, however, is not required to be present"
   ReadAndSkipEncapsulatedBasicOffsetTable();

   // Encapsulated RLE Compressed Images (see PS 3.5-2003, Annex G)
   // Loop on the individual frame[s] and store the information
   // on the RLE fragments in a RLEFramesInfo.
   // Note: - when only a single frame is present, this is a
   //         classical image.
   //       - when more than one frame are present, then we are in 
   //         the case of a multi-frame image.
   long frameLength;
   while ( (frameLength = ReadTagLength(0xfffe, 0xe000)) )
   { 
      // Parse the RLE Header and store the corresponding RLE Segment
      // Offset Table information on fragments of this current Frame.
      // Note that the fragment pixels themselves are not loaded
      // (but just skipped).
      long frameOffset = Fp->tellg();

      uint32_t nbRleSegments = ReadInt32();
      if ( nbRleSegments > 16 )
      {
         // There should be at most 15 segments (refer to RLEFrame class)
         gdcmWarningMacro( "Too many segments.");
      }
 
      uint32_t rleSegmentOffsetTable[16];
      for( int k = 1; k <= 15; k++ )
      {
         rleSegmentOffsetTable[k] = ReadInt32();
      }

      // Deduce from both the RLE Header and the frameLength the
      // fragment length, and again store this info in a
      // RLEFramesInfo.
      long rleSegmentLength[15];
      // skipping (not reading) RLE Segments
      if ( nbRleSegments > 1)
      {
         for(unsigned int k = 1; k <= nbRleSegments-1; k++)
         {
             rleSegmentLength[k] =  rleSegmentOffsetTable[k+1]
                                  - rleSegmentOffsetTable[k];
             SkipBytes(rleSegmentLength[k]);
          }
       }

       rleSegmentLength[nbRleSegments] = frameLength 
                                      - rleSegmentOffsetTable[nbRleSegments];
       SkipBytes(rleSegmentLength[nbRleSegments]);

       // Store the collected info
       RLEFrame *newFrame = new RLEFrame;
       newFrame->SetNumberOfFragments(nbRleSegments);
       for( unsigned int uk = 1; uk <= nbRleSegments; uk++ )
       {
          newFrame->SetOffset(uk,frameOffset + rleSegmentOffsetTable[uk]);
          newFrame->SetLength(uk,rleSegmentLength[uk]);
       }
       RLEInfo->AddFrame(newFrame);
   }

   // Make sure that at the end of the item we encounter a 'Sequence
   // Delimiter Item':
   if ( !ReadTag(0xfffe, 0xe0dd) )
   {
      gdcmWarningMacro( "No sequence delimiter item at end of RLE item sequence");
   }
}

/**
 * \brief Parse pixel data from disk of [multi-]fragment Jpeg encoding.
 *        Compute the jpeg extra information (fragment[s] offset[s] and
 *        length) and store it[them] in \ref JPEGInfo for later pixel
 *        retrieval usage.
 */
void File::ComputeJPEGFragmentInfo()
{
   // If you need to, look for comments of ComputeRLEInfo().
   std::string ts = GetTransferSyntax();
   if ( ! Global::GetTS()->IsJPEG(ts) )
   {
      return;
   }

   ReadAndSkipEncapsulatedBasicOffsetTable();

   // Loop on the fragments[s] and store the parsed information in a
   // JPEGInfo.
   long fragmentLength;
   while ( (fragmentLength = ReadTagLength(0xfffe, 0xe000)) )
   { 
      long fragmentOffset = Fp->tellg();

       // Store the collected info
       JPEGFragment *newFragment = new JPEGFragment;
       newFragment->SetOffset(fragmentOffset);
       newFragment->SetLength(fragmentLength);
       JPEGInfo->AddFragment(newFragment);

       SkipBytes(fragmentLength);
   }

   // Make sure that at the end of the item we encounter a 'Sequence
   // Delimiter Item':
   if ( !ReadTag(0xfffe, 0xe0dd) )
   {
      gdcmWarningMacro( "No sequence delimiter item at end of JPEG item sequence");
   }
}

/**
 * \brief   Assuming the internal file pointer \ref Document::Fp 
 *          is placed at the beginning of a tag check whether this
 *          tag is (TestGroup, TestElement).
 * \warning On success the internal file pointer \ref Document::Fp
 *          is modified to point after the tag.
 *          On failure (i.e. when the tag wasn't the expected tag
 *          (TestGroup, TestElement) the internal file pointer
 *          \ref Document::Fp is restored to it's original position.
 * @param   testGroup   The expected group of the tag.
 * @param   testElement The expected Element of the tag.
 * @return  True on success, false otherwise.
 */
bool File::ReadTag(uint16_t testGroup, uint16_t testElement)
{
   long positionOnEntry = Fp->tellg();
   long currentPosition = Fp->tellg();          // On debugging purposes

   // Read the Item Tag group and element, and make
   // sure they are what we expected:
   uint16_t itemTagGroup;
   uint16_t itemTagElement;
   try
   {
      itemTagGroup   = ReadInt16();
      itemTagElement = ReadInt16();
   }
   catch ( FormatError e )
   {
      return false;
   }
   if ( itemTagGroup != testGroup || itemTagElement != testElement )
   {
      gdcmWarningMacro( "Wrong Item Tag found:"
       << "   We should have found tag ("
       << std::hex << testGroup << "," << testElement << ")" << std::endl
       << "   but instead we encountered tag ("
       << std::hex << itemTagGroup << "," << itemTagElement << ")"
       << "  at address: " << "  0x(" << (unsigned int)currentPosition  << ")" 
       ) ;
      Fp->seekg(positionOnEntry, std::ios::beg);

      return false;
   }
   return true;
}

/**
 * \brief   Assuming the internal file pointer \ref Document::Fp 
 *          is placed at the beginning of a tag (TestGroup, TestElement),
 *          read the length associated to the Tag.
 * \warning On success the internal file pointer \ref Document::Fp
 *          is modified to point after the tag and it's length.
 *          On failure (i.e. when the tag wasn't the expected tag
 *          (TestGroup, TestElement) the internal file pointer
 *          \ref Document::Fp is restored to it's original position.
 * @param   testGroup   The expected group of the tag.
 * @param   testElement The expected Element of the tag.
 * @return  On success returns the length associated to the tag. On failure
 *          returns 0.
 */
uint32_t File::ReadTagLength(uint16_t testGroup, uint16_t testElement)
{

   if ( !ReadTag(testGroup, testElement) )
   {
      return 0;
   }
                                                                                
   //// Then read the associated Item Length
   long currentPosition = Fp->tellg();
   uint32_t itemLength  = ReadInt32();
   {
      gdcmWarningMacro( "Basic Item Length is: "
        << itemLength << std::endl
        << "  at address: " << std::hex << (unsigned int)currentPosition);
   }
   return itemLength;
}

/**
 * \brief When parsing the Pixel Data of an encapsulated file, read
 *        the basic offset table (when present, and BTW dump it).
 */
void File::ReadAndSkipEncapsulatedBasicOffsetTable()
{
   //// Read the Basic Offset Table Item Tag length...
   uint32_t itemLength = ReadTagLength(0xfffe, 0xe000);

   // When present, read the basic offset table itself.
   // Notes: - since the presence of this basic offset table is optional
   //          we can't rely on it for the implementation, and we will simply
   //          trash it's content (when present).
   //        - still, when present, we could add some further checks on the
   //          lengths, but we won't bother with such fuses for the time being.
   if ( itemLength != 0 )
   {
      char *basicOffsetTableItemValue = new char[itemLength + 1];
      Fp->read(basicOffsetTableItemValue, itemLength);

#ifdef GDCM_DEBUG
      for (unsigned int i=0; i < itemLength; i += 4 )
      {
         uint32_t individualLength = str2num( &basicOffsetTableItemValue[i],
                                              uint32_t);
         gdcmWarningMacro( "Read one length: " << 
                          std::hex << individualLength );
      }
#endif //GDCM_DEBUG

      delete[] basicOffsetTableItemValue;
   }
}

//-----------------------------------------------------------------------------
// Print

//-----------------------------------------------------------------------------
} // end namespace gdcm
