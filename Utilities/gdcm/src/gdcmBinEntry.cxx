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
#include "gdcmDebug.h"
#include <fstream>
#include <ios> // for std::ios_base

namespace gdcm 
{

//-----------------------------------------------------------------------------
// Constructor / Destructor

/**
 * \brief   Constructor from a given BinEntry
 */
BinEntry::BinEntry(DictEntry* e) : ValEntry(e)
{
   BinArea = 0;
}

/**
 * \brief   Constructor from a given BinEntry
 * @param   e Pointer to existing Doc entry
 */
BinEntry::BinEntry(DocEntry* e) : ValEntry(e->GetDictEntry())
{
   UsableLength = e->GetLength();
   ReadLength   = e->GetReadLength();
   ImplicitVR   = e->IsImplicitVR();
   Offset       = e->GetOffset();
   PrintLevel   = e->GetPrintLevel();
   //FIXME
   //SQDepthLevel = e->GetDepthLevel();

   BinArea = 0; // let's be carefull !
}

/**
 * \brief   Canonical destructor.
 */
BinEntry::~BinEntry()
{
   if (BinArea)
   {
      delete[] BinArea;
      BinArea = 0; // let's be carefull !
   }
}


//-----------------------------------------------------------------------------
// Print
/*
 * \brief   canonical Printer
 */
 
void BinEntry::Print(std::ostream &os)
{
   DocEntry::Print(os);
   std::ostringstream s;
   void* binArea = GetBinArea();
   if (binArea)
   {
      //s << " [" << GDCM_BINLOADED 
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
         //s << " [gdcm::Binary data NOT loaded]";
         s << " [" <<GetValue() << "]";
      }
         
   }
   os << s.str();
}

/*
 * \brief   canonical Writer
 * @param fp already open file pointer
 * @param filetype type of the file to be written
*/
void BinEntry::Write(std::ofstream* fp, FileType filetype)
{
   DocEntry::Write(fp, filetype);
   void* binArea = GetBinArea();
   int lgr = GetLength();
   if (binArea)
   {
      // there is a 'non string' LUT, overlay, etc
      fp->write ( (char*)binArea, lgr ); // Elem value
      //assert( strlen((char*)binArea) == lgr );

   }
   else
   {
    // nothing was loaded, but we need to skip space on disc
      fp->seekp(lgr, std::ios_base::cur);
   }
}
//-----------------------------------------------------------------------------
// Public


/// \brief Sets the value (non string) of the current Dicom Header Entry
void BinEntry::SetBinArea( uint8_t* area )  
{ 
   if (BinArea)
      delete[] BinArea;
   BinArea = area;  
}

//-----------------------------------------------------------------------------
// Protected

//-----------------------------------------------------------------------------
// Private
   
//-----------------------------------------------------------------------------
} // end namespace gdcm
