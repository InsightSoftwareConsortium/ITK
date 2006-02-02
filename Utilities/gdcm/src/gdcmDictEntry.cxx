/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmDictEntry.cxx
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

#include "gdcmDictEntry.h"
#include "gdcmDebug.h"
#include "gdcmUtil.h"

#include <iomanip> // for std::ios::left, ...
#include <fstream>
#include <stdio.h> // for sprintf

namespace gdcm 
{
//-----------------------------------------------------------------------------
// Constructor / Destructor
/**
 * \brief   Constructor
 * @param   group      DICOM-Group Number
 * @param   elem       DICOM-Element Number
 * @param   vr         Value Representation
 * @param   vm         Value Multiplicity 
 * @param   name       description of the element
*/

DictEntry::DictEntry(uint16_t group, uint16_t elem,
                     TagName const &vr, 
                     TagName const &vm,
                     TagName const &name)
{
   Group   = group;
   Element = elem;
   VR      = vr;
   VM      = vm;
   Name    = name;
   Key     = TranslateToKey(group, elem);
}

//-----------------------------------------------------------------------------
// Public
/**
 * \brief       If-and only if-the V(alue) R(epresentation)
 * \            is unset then overwrite it.
 * @param vr    New V(alue) R(epresentation) to be set.
 */
void DictEntry::SetVR(TagName const &vr) 
{
   if ( IsVRUnknown() )
   {
      VR = vr;
   }
   else 
   {
      gdcmErrorMacro( "Overwriting VR might compromise a dictionary");
   }
}

/**
 * \brief       If-and only if-the V(alue) M(ultiplicity)
 * \            is unset then overwrite it.
 * @param vm    New V(alue) M(ultiplicity) to be set.
 */
void DictEntry::SetVM(TagName const &vm) 
{
   if ( IsVMUnknown() )
   {
      VM = vm;
   }
   else 
   {
      gdcmErrorMacro( "Overwriting VM might compromise a dictionary");
   }
}

/**
 * \brief   concatenates 2 uint16_t (supposed to be a Dicom group number 
 *                                              and a Dicom element number)
 * @param  group the Dicom group number used to build the tag
 * @param  elem the Dicom element number used to build the tag
 * @return the built tag
 */
TagKey DictEntry::TranslateToKey(uint16_t group, uint16_t elem)
{
   // according to 'Purify', TranslateToKey is one of the most
   // time consuming methods.
   // Let's try to shorten it !
 
   //return Util::Format("%04x|%04x", group, elem); // too much time !
#if FASTTAGKEY
   TagKey r;
   r.tab[0] = group;
   r.tab[1] = elem;
   return r;
#else
   char res[10];
   sprintf(res,"%04x|%04x", group, elem);
   return res;
#endif
}

//-----------------------------------------------------------------------------
// Protected

//-----------------------------------------------------------------------------
// Private

//-----------------------------------------------------------------------------
// Print
/**
 * \brief   Prints an entry of the Dicom DictionaryEntry
 * @param   os ostream we want to print in
 * @param indent Indentation string to be prepended during printing
 */
void DictEntry::Print(std::ostream &os, std::string const & )
{
   std::string vr;
   itksys_ios::ostringstream s;

   vr = GetVR();
   if ( vr==GDCM_UNKNOWN )
      vr="  ";

   s << DictEntry::TranslateToKey(GetGroup(),GetElement()); 
   s << " [" << vr  << "] ";

   if ( PrintLevel >= 1 )
   {
      s.setf(std::ios::left);
      s << std::setw(66-GetName().length()) << " ";
   }

   s << "[" << GetName()<< "]";
   os << s.str() << std::endl;
}

//-----------------------------------------------------------------------------
} // end namespace gdcm

