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

namespace gdcm 
{

//-----------------------------------------------------------------------------
// Constructor / Destructor
/**
 * \brief   Constructor
 * @param   group      DICOM-Group Number
 * @param   element    DICOM-Element Number
 * @param   vr         Value Representatiion
 * @param   fourth    // DO NOT use any longer; 
 *                       NOT part of the Dicom Standard
 * @param   name      description of the element
*/

DictEntry::DictEntry(uint16_t group, uint16_t element,
                     TagName const & vr, TagName const & fourth,
                     TagName const & name)
{
   Group   = group;
   Element = element;
   VR      = vr;
   Fourth  = fourth;
   Name    = name;
   Key     = TranslateToKey(group, element);
}

//-----------------------------------------------------------------------------
// Print

//-----------------------------------------------------------------------------
// Public
/**
 * \brief   concatenates 2 uint16_t (supposed to be a Dicom group number 
 *                                              and a Dicom element number)
 * @param  group the Dicom group number used to build the tag
 * @param  element the Dicom element number used to build the tag
 * @return the built tag
 */
TagKey DictEntry::TranslateToKey(uint16_t group, uint16_t element)
{
   TagKey key = Util::Format("%04x|%04x", group , element);

   return key;
}

//-----------------------------------------------------------------------------
/**
 * \brief       If-and only if-the V(alue) R(epresentation)
 * \            is unset then overwrite it.
 * @param vr    New V(alue) R(epresentation) to be set.
 */
void DictEntry::SetVR(TagName const & vr) 
{
   if ( IsVRUnknown() )
   {
      VR = vr;
   }
   else 
   {
      dbg.Error(true, "DictEntry::SetVR",
                       "Overwriting VR might compromise a dictionary");
   }
}

//-----------------------------------------------------------------------------
// Protected

//-----------------------------------------------------------------------------
// Private

//-----------------------------------------------------------------------------
} // end namespace gdcm

