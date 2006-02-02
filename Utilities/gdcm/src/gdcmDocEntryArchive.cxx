/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmDocEntryArchive.cxx
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

#include "gdcmDocEntryArchive.h"
#include "gdcmDebug.h"
#include "gdcmDocEntry.h"

#include <string>

namespace gdcm 
{
//-----------------------------------------------------------------------------
// Constructor / Destructor
/**
 * \brief Constructor
 */
DocEntryArchive::DocEntryArchive(File *file)
{
   ArchFile = file;
}

/**
 * \brief Destructor
 */
DocEntryArchive::~DocEntryArchive()
{
   ClearArchive();
}

//-----------------------------------------------------------------------------
// Public
/**
 * \brief   Replaces in the Header a DocEntry by the new DocEntry. 
 *          The initial DocEntry is kept in archive.
 * @param   newEntry New entry to substitute to an other entry of the Header
 * @return  FALSE when an other DocEntry is already archived with the same key
 *          TRUE otherwise
 */
bool DocEntryArchive::Push(DocEntry *newEntry)
{
   if ( !newEntry )
      return false;

   uint16_t group = newEntry->GetDictEntry()->GetGroup();
   uint16_t elem  = newEntry->GetDictEntry()->GetElement();
   TagKey key = DictEntry::TranslateToKey(group,elem);

   if ( Archive.find(key) == Archive.end() )
   {
      // Save the old DocEntry if any
      DocEntry *old = ArchFile->GetDocEntry(group, elem);
      Archive[key]  = old;
      if ( old )
         ArchFile->RemoveEntryNoDestroy(old);

      // Set the new DocEntry
      ArchFile->AddEntry(newEntry);

      return true;
   }
   return false;
}

/**
 * \brief   Removes out of the Header a DocEntry.
 *          (it's kept in archive).
 * @param   group   Group number of the Entry to remove
 * @param   elem  Element number of the Entry to remove
 * @return  FALSE when an other DocEntry is already archived with the same key
 *          TRUE otherwise
 */
bool DocEntryArchive::Push(uint16_t group, uint16_t elem)
{
   TagKey key = DictEntry::TranslateToKey(group, elem);

   if ( Archive.find(key)==Archive.end() )
   {
      // Save the old DocEntry if any
      DocEntry *old = ArchFile->GetDocEntry(group, elem);
      Archive[key] = old;
      if ( old )
         ArchFile->RemoveEntryNoDestroy(old);

      return true;
   }
   return false;
}

/**
 * \brief   Restore in the Header the DocEntry specified by (group,element). 
 *          The archive entry is destroyed.
 * @param   group   Group number of the Entry to restore
 * @param   elem  Element number of the Entry to restore
 * @return  FALSE when the key isn't in the archive, 
 *          TRUE otherwise
 */
bool DocEntryArchive::Restore(uint16_t group, uint16_t elem)
{
   TagKey key=DictEntry::TranslateToKey(group, elem);

   TagDocEntryHT::iterator restoreIt=Archive.find(key);
   if ( restoreIt!=Archive.end() )
   {
      // Delete the new value
      DocEntry *rem = ArchFile->GetDocEntry(group, elem);
      if ( rem )
         ArchFile->RemoveEntry(rem);

      // Restore the old value
      if ( Archive[key] )
         ArchFile->AddEntry(Archive[key]);

      Archive.erase(restoreIt);

      return true;
   }
   return false;
}

/**
 * \brief   Removes all DocEntry from the archive, and destroy them.  
 *          The archives entries aren't restored.
 */
void DocEntryArchive::ClearArchive( )
{
   for(TagDocEntryHT::iterator it = Archive.begin();
       it!=Archive.end();
       ++it)
   {
      delete it->second;
   }
   Archive.clear();
}

//-----------------------------------------------------------------------------
// Protected

//-----------------------------------------------------------------------------
// Private

//-----------------------------------------------------------------------------
// Print
/**
 * \brief   Print all 
 * @param   os The output stream to be written to.
 */
void DocEntryArchive::Print(std::ostream &os) 
{
   os << "Elements in archives :" << std::endl;
   for(TagDocEntryHT::iterator it = Archive.begin();
       it!=Archive.end();
       ++it)
   {
      if ( it->second )
         it->second->Print(os);
   }
}

//-----------------------------------------------------------------------------
} // end namespace gdcm
