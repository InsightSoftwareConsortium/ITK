/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmDicomDirObject.cxx
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

#include "gdcmDicomDirObject.h"
#include "gdcmGlobal.h"
#include "gdcmDebug.h"
#include "gdcmValEntry.h"
#include "gdcmDictSet.h"

namespace gdcm 
{
//-----------------------------------------------------------------------------
/**
 * \brief  Constructor 
 *          
 * @param depth Sequence depth level
 */
  
DicomDirObject::DicomDirObject(int depth) 
          : SQItem (depth)
{
}

/**
 * \brief   Canonical destructor.
 */
DicomDirObject::~DicomDirObject()
{
}

//-----------------------------------------------------------------------------
// Public

//-----------------------------------------------------------------------------
// Protected
/**
 * \brief   add the 'Object' related Dicom Elements to the listEntries
 *          of a partially created DICOMDIR
 * @param elemList Element List to add at the right place
 */
void DicomDirObject::FillObject(ListDicomDirMetaElem const &elemList)
{
  // FillObject fills up the SQItem that will be conneected to the right place
   ListDicomDirMetaElem::const_iterator it;
   uint16_t tmpGr,tmpEl;
   DictEntry *dictEntry;
   ValEntry *entry;
      
   // for all the Elements found in they own part of the DicomDir dict.     
   for(it = elemList.begin(); it != elemList.end(); ++it)
   {
      tmpGr = it->Group;
      tmpEl = it->Elem;
      dictEntry = Global::GetDicts()->GetDefaultPubDict()->GetEntry(tmpGr,tmpEl);
      entry = new ValEntry(dictEntry);
      entry->SetOffset(0); // just to avoid further missprinting
      entry->SetValue(it->Value);

      AddEntry(entry);
   }   
}   

//-----------------------------------------------------------------------------
// Private

//-----------------------------------------------------------------------------
// Print

//-----------------------------------------------------------------------------
} // end namespace gdcm
