/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmDocEntry.cxx
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

#include "gdcmDocEntry.h"
#include "gdcmTS.h"
#include "gdcmVR.h"
#include "gdcmGlobal.h"
#include "gdcmUtil.h"
#include "gdcmDebug.h"

#include <iomanip> // for std::ios::left, ...
#include <fstream>

namespace gdcm 
{
//-----------------------------------------------------------------------------

// Constructor / Destructor
/**
 * \brief   Constructor from a given DictEntry
 * @param   in Pointer to existing dictionary entry
 */
DocEntry::DocEntry(DictEntry *in)
{
   ImplicitVR = false;
   DicomDict  = in;
   SetKey( in->GetKey( ) );
   Offset     = 0 ; // To avoid further missprinting

   // init some variables
   ReadLength = 0;
   Length = 0;
}

//-----------------------------------------------------------------------------
// Public
/**
 * \brief   Writes the common part of any ValEntry, BinEntry, SeqEntry
 * @param fp already open ofstream pointer
 * @param filetype type of the file (ACR, ImplicitVR, ExplicitVR, ...)
 */
void DocEntry::WriteContent(std::ofstream *fp, FileType filetype)
{
   uint32_t ffff  = 0xffffffff;
   uint16_t group = GetGroup();
   VRKey vr       = GetVR();
   uint16_t el    = GetElement();
   uint32_t lgth  = GetLength();

   if ( group == 0xfffe && el == 0x0000 )
   {
     // Fix in order to make some MR PHILIPS images e-film readable
     // see gdcmData/gdcm-MR-PHILIPS-16-Multi-Seq.dcm:
     // we just *always* ignore spurious fffe|0000 tag !   
      return;
   }
   //
   // ----------- Writes the common part
   //
   binary_write( *fp, group); //group number
   binary_write( *fp, el);    //element number

   // Dicom V3 group 0x0002 is *always* Explicit VR !
   if ( filetype == ExplicitVR || group == 0x0002 )
   {
      // Special case of delimiters:
      if (group == 0xfffe)
      {
         // Delimiters have NO Value Representation
         // Hence we skip writing the VR.
         //
         // In order to avoid further troubles, we choose to write them
         // as 'no-length' Item Delimitors (we pad by writing 0xffffffff)
         // We shall force the end of a given SeqItem by writting 
         //  a Item Delimitation Item (fffe, e00d)

         uint32_t ff = 0xffffffff;
         binary_write(*fp, ff);
         return;
      }

      uint16_t zero = 0;
      uint16_t shortLgr = (uint16_t)lgth;

      if (vr == GDCM_UNKNOWN)
      {
         // GDCM_UNKNOWN was stored in the Entry VR;
         // deal with Entry as if TS were Implicit VR
 
         // FIXME : troubles expected on big endian processors :
         // let lgth = 0x00001234
         // we write 34 12 00 00 on little endian proc (OK)
         // we write 12 34 00 00 on big endian proc (KO)          
         //binary_write(*fp, shortLgr);
         //binary_write(*fp, zero);

         binary_write(*fp, lgth);
      }
      else
      {
         binary_write(*fp, vr);
         gdcmAssertMacro( vr.size() == 2 );
                  
         if ( (vr == "OB") || (vr == "OW") || (vr == "SQ") /*|| (vr == "UN")*/ )
         {
            binary_write(*fp, zero);
            if (vr == "SQ")
            {
               // we set SQ length to ffffffff
               // and  we shall write a Sequence Delimitor Item 
               // at the end of the Sequence! 
               binary_write(*fp, ffff);
            }
            else
            {
               binary_write(*fp, lgth);
            }
         }
         else
         {
            binary_write(*fp, shortLgr);
         }
      }
   } 
   else // IMPLICIT VR 
   { 
      if (vr == "SQ")
      {
         binary_write(*fp, ffff);
      }
      else
      {
         binary_write(*fp, lgth);
      }
   }
}

/**
 * \brief   Gets the full length of the elementary DocEntry (not only value
 *          length) depending on the VR.
 */
uint32_t DocEntry::GetFullLength()
{
   uint32_t l = GetReadLength();
   if ( IsImplicitVR() )
   {
      l = l + 8;  // 2 (gr) + 2 (el) + 4 (lgth) 
   }
   else
   {
      if ( GetVR()=="OB" || GetVR()=="OW" || GetVR()=="SQ" )
      {
         l = l + 12; // 2 (gr) + 2 (el) + 2 (vr) + 2 (unused) + 4 (lgth)
      }
      else
      {
         l = l + 8;  // 2 (gr) + 2 (el) + 2 (vr) + 2 (lgth)
      }
   }
   return l;
}

/**
 * \brief   tells us if entry is the last one of a 'no length' SequenceItem 
 *          (fffe,e00d) 
 */
bool DocEntry::IsItemDelimitor()
{
   return (GetGroup() == 0xfffe && GetElement() == 0xe00d);
}

/**
 * \brief   tells us if entry is the first one of an Item 
 *          (fffe,e000) 
 */
bool DocEntry::IsItemStarter()
{
   return (GetGroup() == 0xfffe && GetElement() == 0xe000);
}

/**
 * \brief   tells us if entry is the last one of a 'no length' Sequence 
 *          (fffe,e0dd) 
 */
bool DocEntry::IsSequenceDelimitor()
{
   return (GetGroup() == 0xfffe && GetElement() == 0xe0dd);
}

/**
 * \brief   Copies all the attributes from an other DocEntry 
 * @param doc entry to copy from
 */
void DocEntry::Copy(DocEntry *doc)
{
   Length     = doc->Length;
   ReadLength = doc->ReadLength;
   ImplicitVR = doc->ImplicitVR;
   Offset     = doc->Offset;
}

//-----------------------------------------------------------------------------
// Protected

//-----------------------------------------------------------------------------
// Private

//-----------------------------------------------------------------------------
// Print
/**
 * \brief   Prints the common part of ValEntry, BinEntry, SeqEntry
 * @param   os ostream we want to print in
 * @param indent Indentation string to be prepended during printing
 */
void DocEntry::Print(std::ostream &os, std::string const & )
{
   size_t o;
   std::string st;
   TSKey v;
   std::string d2, vr;
   itksys_ios::ostringstream s;
   uint32_t lgth;

   o  = GetOffset();
   vr = GetVR();
   if ( vr==GDCM_UNKNOWN )
      vr="  ";

   s << DictEntry::TranslateToKey(GetGroup(),GetElement()); 

   if (PrintLevel >= 2)
   {
      s << " lg : ";
      lgth = GetReadLength(); // ReadLength, as opposed to (usable) Length
      if (lgth == 0xffffffff)
      {
         st = " ffff ";
         s.setf(std::ios::left);
         s << std::setw(4);  
         s << "    x(ffff) ";
         s.setf(std::ios::left);
         s << std::setw(8) << "-1"; 
      }
      else
      {
         st = Util::Format("x(%x)",lgth); // we may keep it
         s.setf(std::ios::left);
         s << std::setw(11-st.size()) << " ";
         s << st << " ";
         s.setf(std::ios::left);
         s << std::setw(8) << lgth; 
      }
      s << " Off.: ";
      st = Util::Format("x(%x)",o);  // we may keep it
      s << std::setw(11-st.size()) << " ";
      s << st << " ";
      s << std::setw(8) << o; 
   }
   if (PrintLevel >= 1)
      s << " ";

   s << "[" << vr  << "] ";

   std::string name;
   if ( GetElement() == 0x0000 )
      name = "Group Length";
   else
      name = GetName();

   if (PrintLevel >= 1)
   {
      s.setf(std::ios::left);
      s << std::setw(66-name.length()) << " ";
   }
    
   s << "[" << name << "]";
   os << s.str();      
}

//-----------------------------------------------------------------------------
} // end namespace gdcm
