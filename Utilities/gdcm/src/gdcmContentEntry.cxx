/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmContentEntry.cxx
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

#include "gdcmContentEntry.h"
#include "gdcmVR.h"
#include "gdcmTS.h"
#include "gdcmGlobal.h"
#include "gdcmUtil.h"
#include "gdcmDebug.h"

#include <fstream>

namespace gdcm 
{

//-----------------------------------------------------------------------------
// Constructor / Destructor
/**
 * \brief   Constructor for a given DictEntry
 * @param   e Pointer to existing dictionary entry
 */
ContentEntry::ContentEntry(DictEntry *e) 
            : DocEntry(e)
{
   Value = GDCM_UNFOUND;
}

/**
 * \brief   Constructor for a given DocEntry
 * @param   e Pointer to existing Doc entry
 */
ContentEntry::ContentEntry(DocEntry *e)
            : DocEntry(e->GetDictEntry())
{
   Copy(e);
}

/**
 * \brief   Canonical destructor.
 */
ContentEntry::~ContentEntry ()
{
}

//-----------------------------------------------------------------------------
// Print

//-----------------------------------------------------------------------------
// Public
void ContentEntry::Copy(DocEntry *doc)
{
   DocEntry::Copy(doc);

   ContentEntry *entry = dynamic_cast<ContentEntry *>(doc);
   if(entry)
      Value = entry->Value;
}

//-----------------------------------------------------------------------------
// Protected

//-----------------------------------------------------------------------------
// Private

//-----------------------------------------------------------------------------
} // end namespace gdcm

