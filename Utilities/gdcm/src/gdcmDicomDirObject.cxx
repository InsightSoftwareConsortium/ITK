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
namespace gdcm 
{

//-----------------------------------------------------------------------------
/**
 * \ingroup DicomDirObject
 * \brief  Constructor 
 *          
 * @param ptagHT pointer to the HTable (DicomDirObject needs it 
 *               to build the DocEntries)
 * @param depth Sequence depth level
 */
  
DicomDirObject::DicomDirObject(TagDocEntryHT *ptagHT, int depth) 
          : SQItem (depth)
{
   PtagHT = ptagHT;
}


/**
 * \ingroup DicomDirObject
 * \brief   Canonical destructor.
 */
DicomDirObject::~DicomDirObject()
{
}



//-----------------------------------------------------------------------------
// Public


/**
 * \ingroup DicomDirObject
 * \brief   Builds a hash table (multimap) containing 
 *          pointers to all Header Entries (i.e Dicom Element)
 *          related to this 'object'
 * @return
 */ 
TagDocEntryHT DicomDirObject::GetEntry()
{
   TagDocEntryHT HT;
   DocEntries = GetDocEntries();   
   for(ListDocEntry::iterator i = DocEntries.begin(); 
                              i != DocEntries.end(); ++i)
   {
      HT[(*i)->GetKey()] = *i;
   }
   return HT;
}

//-----------------------------------------------------------------------------
// Protected
/**
 * \brief   add the 'Object' related Dicom Elements to the listEntries
 *          of a partially created DICOMDIR
 */
void DicomDirObject::FillObject(ListDicomDirMetaElem const & elemList)
{
  // FillObject rempli le SQItem qui sera accroche au bon endroit

   ListDicomDirMetaElem::const_iterator it;
   uint16_t tmpGr,tmpEl;
   DictEntry *dictEntry;
   ValEntry *entry;
      
   // for all the Elements found in they own part of the DicomDir dict.     
   for(it = elemList.begin(); it != elemList.end(); ++it)
   {
      tmpGr = it->Group;
      tmpEl = it->Elem;
      dictEntry = Global::GetDicts()->GetDefaultPubDict()->GetDictEntryByNumber(tmpGr,tmpEl);
      entry = new ValEntry(dictEntry);
      entry->SetOffset(0); // just to avoid further missprinting
      entry->SetValue(it->Value);

      // dealing with value length ...
  
      if(dictEntry->GetGroup()==0xfffe) 
      {
         entry->SetLength(entry->GetValue().length());
      }
      else if( dictEntry->GetVR() == "UL" || dictEntry->GetVR() == "SL" ) 
      {
         entry->SetLength(4);
      } 
      else if( dictEntry->GetVR() == "US" || dictEntry->GetVR() == "SS" ) 
      {
         entry->SetLength(2); 
      } 
      else if( dictEntry->GetVR() == "SQ" )
      {
         entry->SetLength(0xffffffff);
      }
      else
      {
         entry->SetLength(entry->GetValue().length()); 
      }                                
      AddDocEntry(entry);
   }   
}   
} // end namespace gdcm

