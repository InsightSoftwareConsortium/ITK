/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmBinEntry.cxx
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

#include "gdcmBinEntry.h"
#include "gdcmContentEntry.h"
#include "gdcmUtil.h"
#include "gdcmDebug.h"  //hidden way to include sstream

#include <fstream>
#include <iostream> // for std::ios_base, since <ios> does not exist on gcc/Solaris

namespace gdcm 
{
//-----------------------------------------------------------------------------
// Constructor / Destructor
/**
 * \brief   Constructor from a given BinEntry
 */
BinEntry::BinEntry(DictEntry *e) 
         :ContentEntry(e)
{
   BinArea = 0;
   SelfArea = true;
}

/**
 * \brief   Constructor from a given BinEntry
 * @param   e Pointer to existing Doc entry
 */
BinEntry::BinEntry(DocEntry *e) 
        : ContentEntry(e->GetDictEntry())
{
   Copy(e);

   BinArea = 0;
   SelfArea = true;
}

/**
 * \brief   Canonical destructor.
 */
BinEntry::~BinEntry()
{
   if (BinArea && SelfArea)
   {
      delete[] BinArea;
      BinArea = 0; // let's be carefull !
   }
}

//-----------------------------------------------------------------------------
// Public
/**
 * \brief   canonical Writer
 * @param fp already open file pointer
 * @param filetype type of the file (ACR, ImplicitVR, ExplicitVR, ...)
*/
void BinEntry::WriteContent(std::ofstream *fp, FileType filetype)
{ 
   DocEntry::WriteContent(fp, filetype);
   uint8_t* binArea8 = BinArea; //safe notation
   size_t lgr = GetLength();
   if (BinArea) // the binArea was *actually* loaded
   {
   /// \todo  Probabely, the same operation will have to be done when we want 
   ///        to write image with Big Endian Transfert Syntax, 
   ///        and we are working on Little Endian Processor

#if defined(GDCM_WORDS_BIGENDIAN) || defined(GDCM_FORCE_BIGENDIAN_EMULATION)
      /// \todo FIXME Right now, we only care of Pixels element
      ///       we should deal with *all* the BinEntries
      ///       well not really since we are not interpreting values read...

      // 8 Bits Pixels *are* OB, 16 Bits Pixels *are* OW
      // -value forced while Reading process-
      if (GetGroup() == 0x7fe0 && GetVR() == "OW")
      {     
         uint16_t *binArea16 = (uint16_t*)binArea8;
         binary_write (*fp, binArea16, lgr );
      }
      else
      { 
         // For any other VR, BinEntry is re-written as-is
         binary_write (*fp, binArea8, lgr );
      }
#else
      binary_write ( *fp, binArea8, lgr ); // Elem value
#endif //GDCM_WORDS_BIGENDIAN

   }
   else
   {
      // nothing was loaded, but we need to skip space on disc
      fp->seekp(lgr, std::ios::cur);
   }
}

/**
 * \brief Sets the value (non string) of the current Dicom Header Entry
 */
void BinEntry::SetBinArea( uint8_t *area, bool self )  
{ 
   if (BinArea && SelfArea)
      delete[] BinArea;

   BinArea = area;
   SelfArea=self;
}

//-----------------------------------------------------------------------------
// Protected

//-----------------------------------------------------------------------------
// Private
   
//-----------------------------------------------------------------------------
// Print
/**
 * \brief   Prints a BinEntry (Dicom entry)
 * @param   os ostream we want to print in
 * @param indent Indentation string to be prepended during printing
 */
void BinEntry::Print(std::ostream &os, std::string const & )
{
   os << "B ";
   DocEntry::Print(os);
   itksys_ios::ostringstream s;
   void* binArea = GetBinArea();
   if (binArea)
   {
      s << " [" << GetValue()
        << "; length = " << GetLength() << "]";
   }
   else
   {
      if ( GetLength() == 0 )
      {
         s << " []";
      }
      else 
      {
         s << " [" <<GetValue() << "]";
      }         
   }
   os << s.str();
}

//-----------------------------------------------------------------------------
} // end namespace gdcm
