/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmValEntry.cxx
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

#include "gdcmValEntry.h"
#include "gdcmVR.h"
#include "gdcmTS.h"
#include "gdcmGlobal.h"
#include "gdcmUtil.h"
#include "gdcmDebug.h"
#include "gdcmDocument.h"

#include <fstream>
#include <ctype.h>  // for isdigit
#include <stdlib.h> // for atoi

namespace gdcm 
{
//-----------------------------------------------------------------------------
#define MAX_SIZE_PRINT_ELEMENT_VALUE 0x7fffffff
uint32_t ValEntry::MaxSizePrintEntry = MAX_SIZE_PRINT_ELEMENT_VALUE;
//-----------------------------------------------------------------------------
// Constructor / Destructor
/**
 * \brief   Constructor from a given DictEntry
 * @param   e Pointer to existing dictionary entry
 */
ValEntry::ValEntry(DictEntry *e) 
        : ContentEntry(e)
{
}

/**
 * \brief   Constructor from a given DocEntry
 * @param   e Pointer to existing Doc entry
 */
ValEntry::ValEntry(DocEntry *e)
        : ContentEntry(e->GetDictEntry())
{
   Copy(e);
}

/**
 * \brief   Canonical destructor.
 */
ValEntry::~ValEntry ()
{
}

//-----------------------------------------------------------------------------
// Public
/**
 * \brief   Writes the std::string representable' value of a ValEntry
 * @param fp already open ofstream pointer
 * @param filetype type of the file (ACR, ImplicitVR, ExplicitVR, ...)
 */
void ValEntry::WriteContent(std::ofstream *fp, FileType filetype)
{
   DocEntry::WriteContent(fp, filetype);

   if ( GetGroup() == 0xfffe )
   {
      return; //delimitors have NO value
   }

   const VRKey &vr = GetVR();
   unsigned int lgth = GetLength();
   (void)lgth;
   if (vr == "US" || vr == "SS")
   {
      // some 'Short integer' fields may be multivaluated
      // each single value is separated from the next one by '\'
      // we split the string and write each value as a short int
      std::vector<std::string> tokens;
      tokens.erase(tokens.begin(),tokens.end()); // clean any previous value
      Util::Tokenize (GetValue(), tokens, "\\");
      for (unsigned int i=0; i<tokens.size();i++)
      {
         uint16_t val_uint16 = atoi(tokens[i].c_str());
         binary_write( *fp, val_uint16);
      }
      tokens.clear();
      return;
   }
   if (vr == "UL" || vr == "SL")
   {
      // Some 'Integer' fields may be multivaluated (multiple instances 
      // of integer). But each single integer value is separated from the
      // next one by '\' (backslash character). Hence we split the string
      // along the '\' and write each value as an int:
      std::vector<std::string> tokens;
      tokens.erase(tokens.begin(),tokens.end()); // clean any previous value
      Util::Tokenize (GetValue(), tokens, "\\");
      for (unsigned int i=0; i<tokens.size();i++)
      {
         uint32_t val_uint32 = atoi(tokens[i].c_str());
         binary_write( *fp, val_uint32);
      }
      tokens.clear();
      return;
   } 

   gdcmAssertMacro( lgth == GetValue().length() );
   binary_write(*fp, GetValue());
} 


/**
 * \brief Header Elements too long will not be printed
 * @param newSize new size
 */ 
void ValEntry::SetMaxSizePrintEntry(long newSize) 
{
   if ( newSize < 0 )
   {
      return;
   }
   if ((uint32_t)newSize >= (uint32_t)0xffffffff )
   {
      ValEntry::MaxSizePrintEntry = 0xffffffff;
      return;
   }
   ValEntry::MaxSizePrintEntry = newSize;
}


/**
 * \brief   Sets the std::string representable' value of a ValEntry
 * @param  val value to set 
 */
void ValEntry::SetValue(std::string const &val)
{
   // Integers have a special treatement for their length:
   size_t l = val.length();
   if ( l != 0) // To avoid to be cheated by 'zero length' integers
   {   
      const VRKey &vr = GetVR();
      if ( vr == "US" || vr == "SS" )
      {
         // for multivaluated items
         l = (Util::CountSubstring(val, "\\") + 1) * 2;
         ContentEntry::SetValue(val);
      }
      else if ( vr == "UL" || vr == "SL" )
      {
         // for multivaluated items
         l = (Util::CountSubstring(val, "\\") + 1) * 4;;
         ContentEntry::SetValue(val);
      }
      else
      {
         std::string finalVal;
         if( GetVR() == "UI" )
         {
            finalVal = Util::DicomString( val.c_str() );
         }
         else
         {
            finalVal = val;
            if( finalVal.size() % 2 )
              {
              finalVal.append( 1, ' ' );
              }
         }
         gdcmAssertMacro( !(finalVal.size() % 2) );

         l = finalVal.length();
         ContentEntry::SetValue(finalVal);
      }
   }
   else
   {
      std::string finalVal = Util::DicomString( val.c_str() );
      gdcmAssertMacro( !(finalVal.size() % 2) );

      l = finalVal.length();
      ContentEntry::SetValue(finalVal);
   }

   SetLength((uint32_t)(l));
}

/**
 * \brief   Compute the full length of the elementary DataEntry (not only value
 *          length) depending on the VR.
 */
uint32_t ValEntry::ComputeFullLength()
{
   return GetFullLength();
}

//-----------------------------------------------------------------------------
// Protected

//-----------------------------------------------------------------------------
// Private

//-----------------------------------------------------------------------------
// Print
/**
 * \brief   Prints the 'std::string representable' value of ValEntry
 * @param   os ostream we want to print in
 * @param indent Indentation string to be prepended during printing
 */
void ValEntry::Print(std::ostream &os, std::string const &)
{
   uint16_t g = GetGroup();
   uint16_t e = GetElement();
   VRKey vr   = GetVR();
   itksys_ios::ostringstream s; 
   std::string st;
   std::string d2;
     
   os << "V ";
   DocEntry::Print(os); 

   if (g == 0xfffe) // delimiters have NO value
   {
      // just to avoid identing all the remaining code     
      return;
   }
   
   TS *ts = Global::GetTS();
    
   TSAtr v  = GetValue();     
   d2 = Util::CreateCleanString(v);  // replace non printable characters by '.'            
   if ( (long)GetLength() <= ValEntry::GetMaxSizePrintEntry()
    || PrintLevel >= 3
    || d2.find(GDCM_NOTLOADED) < d2.length() )
   {
      s << " [" << d2 << "]";
   }
   else
   {
      s << " [gdcm::too long for print (" << GetLength() << ") ]";
   }
   
   // Display the UID value (instead of displaying only the rough code)
   // First 'clean' trailing character (space or zero) 
   if (g == 0x0002)
   {
      // Any more to be displayed ?
      if ( e == 0x0010 || e == 0x0002 )
      {
         if ( v.length() != 0 )  // for brain damaged headers
         {
            if ( ! isdigit((unsigned char)v[v.length()-1]) )
            {
               v.erase(v.length()-1, 1);
            }
         }
         s << "  ==>\t[" << ts->GetValue(v) << "]";
      }
   }
   else
   {
      if (g == 0x0008)
      {
         if ( e == 0x0016 || e == 0x1150 )
         {
            if ( v.length() != 0 )  // for brain damaged headers
            {
               if ( ! isdigit((unsigned char)v[v.length()-1]) )
               {
                  v.erase(v.length()-1, 1);
               }
            }
            s << "  ==>\t[" << ts->GetValue(v) << "]";
         }
      }
      else
      {
         if (g == 0x0004)
         {
            if ( e == 0x1510 || e == 0x1512  )
            {
               if ( v.length() != 0 )  // for brain damaged headers  
               {
                  if ( ! isdigit((unsigned char)v[v.length()-1]) )
                  {
                     v.erase(v.length()-1, 1);  
                  }
               }
              s << "  ==>\t[" << ts->GetValue(v) << "]";
            }
         }     
      }
   }
   //if (e == 0x0000) {        // elem 0x0000 --> group length 
   if ( vr == "UL" || vr == "US" || vr == "SL" || vr == "SS" )
   {
      if (v == "4294967295") // to avoid troubles in convertion 
      {
         st = "ffffffff";
      }
      else
      {
         if ( GetLength() != 0 )
         {
            st = Util::Format(" x(%x)", atoi(v.c_str()));//FIXME
         }
         else
         {
            st = " ";
         }
      }
      s << st;
   }
   os << s.str();
}

//-----------------------------------------------------------------------------
} // end namespace gdcm

