/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmElementSet.cxx
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

#include "gdcmElementSet.h"
#include "gdcmDebug.h"
#include "gdcmValEntry.h"
#include "gdcmBinEntry.h"
#include "gdcmSeqEntry.h"

namespace gdcm 
{
//-----------------------------------------------------------------------------
// Constructor / Destructor
/**
 * \brief   Constructor for a given ElementSet
 */
//BOZ depthLevel is not usefull anymore
ElementSet::ElementSet(int depthLevel) 
          : DocEntrySet()
{
  (void)depthLevel;
}

/**
 * \brief   Canonical destructor.
 */
ElementSet::~ElementSet() 
{
   ClearEntry();
}

//-----------------------------------------------------------------------------
// Public
/**
  * \brief   Writes the Header Entries (Dicom Elements)
  *          from the H Table
  * @param fp ofstream to write to  
  * @param filetype filetype
  */ 
void ElementSet::WriteContent(std::ofstream *fp, FileType filetype)
{
   for (TagDocEntryHT::const_iterator i = TagHT.begin(); 
                                     i != TagHT.end(); 
                                    ++i)
   {
      i->second->WriteContent(fp, filetype);
   } 
}

/**
 * \brief   add a new Dicom Element pointer to the H Table
 * @param   newEntry entry to add
 */
bool ElementSet::AddEntry(DocEntry *newEntry)
{
   const TagKey &key = newEntry->GetKey();

   if ( TagHT.count(key) == 1 )
   {
      gdcmWarningMacro( "Key already present: " << key );
      return false;
   }
   else
   {
      TagHT.insert(TagDocEntryHT::value_type(newEntry->GetKey(), newEntry));
      return true;
   }
}

/**
 * \brief   Clear the hash table from given entry AND delete the entry.
 * @param   entryToRemove Entry to remove AND delete.
 */
bool ElementSet::RemoveEntry( DocEntry *entryToRemove)
{
   const TagKey &key = entryToRemove->GetKey();
   if ( TagHT.count(key) == 1 )
   {
      TagHT.erase(key);
      //gdcmWarningMacro( "One element erased.");
      delete entryToRemove;
      return true;
   }

   gdcmWarningMacro( "Key not present : " << key);
   return false ;
}

/**
 * \brief   Clear the hash table from given entry BUT keep the entry.
 * @param   entryToRemove Entry to remove.
 */
bool ElementSet::RemoveEntryNoDestroy(DocEntry *entryToRemove)
{
   const TagKey &key = entryToRemove->GetKey();
   if ( TagHT.count(key) == 1 )
   {
      TagHT.erase(key);
      //gdcmWarningMacro( "One element erased.");
      return true;
   }

   gdcmWarningMacro( "Key not present " << key);
   return false ;
}

/**
 * \brief   delete all entries in the ElementSet
 */
void ElementSet::ClearEntry()
{
   for(TagDocEntryHT::iterator cc = TagHT.begin();cc != TagHT.end(); ++cc)
   {
      if ( cc->second )
      {
         delete cc->second;
      }
   }
   TagHT.clear();
}

/**
 * \brief   Get the first entry while visiting *the* 'zero level' DocEntrySet
 *              (DocEntries out of any Sequence)
 * \return  The first DocEntry if found, otherwhise NULL
 */
DocEntry *ElementSet::GetFirstEntry()
{
   ItTagHT = TagHT.begin();
   if (ItTagHT != TagHT.end())
      return  ItTagHT->second;
   return NULL;
}

/**
 * \brief   Get the next entry while visiting *the* 'zero level' DocEntrySet
 *              (DocEntries out of any Sequence) 
 * \note : meaningfull only if GetFirstEntry already called 
 * \return  The next DocEntry if found, otherwhise NULL
 */
DocEntry *ElementSet::GetNextEntry()
{
   gdcmAssertMacro (ItTagHT != TagHT.end());

   ++ItTagHT;
   if (ItTagHT != TagHT.end())
      return  ItTagHT->second;
   return NULL;
}

/**
 * \brief  retrieves a Dicom Element using (group, element)
 * @param   group  Group number of the searched Dicom Element 
 * @param   elem Element number of the searched Dicom Element 
 * @return  
 */
DocEntry *ElementSet::GetDocEntry(uint16_t group, uint16_t elem) 
{
   TagKey key = DictEntry::TranslateToKey(group, elem);
   TagDocEntryHT::iterator it = TagHT.find(key);

   if ( it!=TagHT.end() )
      return it->second;
   return NULL;
}

//-----------------------------------------------------------------------------
// Protected

//-----------------------------------------------------------------------------
// Private

//-----------------------------------------------------------------------------
// Print
/**
  * \brief   Prints the Header Entries (Dicom Elements) from the H Table
  * @param os ostream to write to  
  * @param indent Indentation string to be prepended during printing
  */ 
void ElementSet::Print(std::ostream &os, std::string const & )
{
   // Let's change the 'warning value' for Pixel Data,
   // to avoid human reader to be confused by 'gdcm::NotLoaded'.   
   gdcm::BinEntry *pixelElement = GetBinEntry(0x7fe0,0x0010);
   if ( pixelElement != 0 )
   {
      pixelElement->SetValue( gdcm::GDCM_PIXELDATA);
   }      

   for( TagDocEntryHT::const_iterator i = TagHT.begin(); i != TagHT.end(); ++i)
   {
      DocEntry *entry = i->second;

      entry->SetPrintLevel(PrintLevel);
      entry->Print(os);   

      if ( dynamic_cast<SeqEntry*>(entry) )
      {
         // Avoid the newline for a sequence:
         continue;
      }
      os << std::endl;
   }
}

//-----------------------------------------------------------------------------
} // end namespace gdcm
