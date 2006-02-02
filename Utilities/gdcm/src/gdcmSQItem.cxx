/*=========================================================================
  
  Program:   gdcm
  Module:    gdcmSQItem.cxx
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

#include "gdcmSQItem.h"
#include "gdcmSeqEntry.h"
#include "gdcmValEntry.h"
#include "gdcmBinEntry.h"
#include "gdcmGlobal.h"
#include "gdcmDictSet.h"
#include "gdcmUtil.h"
#include "gdcmDebug.h"

#include <fstream>

namespace gdcm 
{
//-----------------------------------------------------------------------------
// Constructor / Destructor
/**
 * \brief   Constructor from a given SQItem
 */
SQItem::SQItem(int depthLevel ) 
          : DocEntrySet( )
{
   SQDepthLevel = depthLevel;
   SQItemNumber = 0;
}

/**
 * \brief   Canonical destructor.
 */
SQItem::~SQItem() 
{
   ClearEntry();
}

//-----------------------------------------------------------------------------
// Public
/*
 * \brief   canonical Writer
 * @param fp     file pointer to an already open file. 
 * @param filetype type of the file (ACR, ImplicitVR, ExplicitVR, ...)
 */
void SQItem::WriteContent(std::ofstream *fp, FileType filetype)
{
   int j;
   uint16_t item[4] = { 0xfffe, 0xe000, 0xffff, 0xffff };
   uint16_t itemt[4]= { 0xfffe, 0xe00d, 0xffff, 0xffff };

    //we force the writting of an 'Item' Start Element
    // because we want to write the Item as a 'no Length' item
   for(j=0;j<4;++j)
   {
      binary_write( *fp, item[j]);  // fffe e000 ffff ffff 
   }
     
   for (ListDocEntry::iterator it = DocEntries.begin();  
                               it != DocEntries.end();
                             ++it)
   {   
      // we skip delimitors (start and end one) because 
      // we force them as 'no length'
      if ( (*it)->GetGroup() == 0xfffe )
      {
         continue;
      }

      // Fix in order to make some MR PHILIPS images e-film readable
      // see gdcmData/gdcm-MR-PHILIPS-16-Multi-Seq.dcm:
      // we just *always* ignore spurious fffe|0000 tag ! 
      if ( (*it)->GetGroup() == 0xfffe && (*it)->GetElement() == 0x0000 )
      {
          break; // FIXME : break or continue; ?!?  
                 // --> makes no difference since the only bugged file we have
                 // contains 'impossible tag' fffe|0000 in last position !                            
      }

      (*it)->WriteContent(fp, filetype);
   }
      
    //we force the writting of an 'Item Delimitation' item
    // because we wrote the Item as a 'no Length' item
   for(j=0;j<4;++j)
   {
      binary_write( *fp, itemt[j]);  // fffe e000 ffff ffff 
   } 
}

/**
 * \brief   Inserts *in the right place* any Entry (Dicom Element)
 *          into the Sequence Item
 * @param entry Entry to add
 * @return always true 
 */
bool SQItem::AddEntry(DocEntry *entry)
{   
   if (DocEntries.empty() )
   {
      DocEntries.push_back(entry);
      return true;
   }
 
   ListDocEntry::iterator insertSpot;
   ListDocEntry::iterator it = DocEntries.end();
   do
   {
      it--;

      if ( (*it)->IsItemDelimitor() )
      {
         continue;
      }
      if ( (*it)->GetGroup() < entry->GetGroup() )
         break;
      else
         if ( (*it)->GetGroup() == entry->GetGroup() &&
              (*it)->GetElement() < entry->GetElement() )
            break;
   } while (it != DocEntries.begin() );
  
   insertSpot = it++;
   insertSpot++; // ?!?
   DocEntries.insert(insertSpot, entry); 
   return true;
}   

/**
 * \brief   Clear the std::list from given entry AND delete the entry.
 * @param   entryToRemove Entry to remove AND delete.
 * @return true if the entry was found and removed; false otherwise
 */
bool SQItem::RemoveEntry( DocEntry *entryToRemove )
{
   for(ListDocEntry::iterator it = DocEntries.begin();
                              it != DocEntries.end();
                            ++it)
   {
      if ( *it == entryToRemove )
      {
         DocEntries.erase(it);
         gdcmWarningMacro( "One element erased: " << entryToRemove->GetKey() );
         delete entryToRemove;
         return true;
      }
   }
   gdcmWarningMacro( "Entry not found: " << entryToRemove->GetKey() );
   return false ;
}

/**
 * \brief   Clear the std::list from given entry BUT keep the entry.
 * @param   entryToRemove Entry to remove.
 * @return true if the entry was found and removed; false otherwise
 */
bool SQItem::RemoveEntryNoDestroy(DocEntry *entryToRemove)
{
   for(ListDocEntry::iterator it =  DocEntries.begin();
                              it != DocEntries.end();
                            ++it)
   {
      if ( *it == entryToRemove )
      {
         DocEntries.erase(it);
         gdcmWarningMacro( "One element removed, no destroyed: "
                            << entryToRemove->GetKey() );
         return true;
      }
   }
                                                                                
   gdcmWarningMacro( "Entry not found:" << entryToRemove->GetKey() );
   return false ;
}
                                                                                
/**
 * \brief  Remove all entry in the Sequence Item 
 */
void SQItem::ClearEntry()
{
   for(ListDocEntry::iterator cc = DocEntries.begin();
                              cc != DocEntries.end();
                            ++cc)
   {
      delete *cc;
   }
   DocEntries.clear();
}

/**
 * \brief  Clear the std::list from given Sequence Item  BUT keep the entries
 */
void SQItem::ClearEntryNoDestroy()
{
   DocEntries.clear();
}


/**
 * \brief  Move all the entries from a given Sequence Item 
 */
void SQItem::MoveObject(SQItem *source)
{
   DocEntries = source->DocEntries;
   source->ClearEntryNoDestroy();
}

/**
 * \brief   Get the first Dicom entry while visiting the SQItem
 * \return  The first DocEntry if found, otherwhise 0
 */
DocEntry *SQItem::GetFirstEntry()
{
   ItDocEntries = DocEntries.begin();
   if ( ItDocEntries != DocEntries.end() )
      return *ItDocEntries;
   return 0;   
}
                                                                                
/**
 * \brief   Get the next Dicom entry while visiting the SQItem
 * \return  The next DocEntry if found, otherwhise NULL
 */
DocEntry *SQItem::GetNextEntry()
{
   ++ItDocEntries;
   if ( ItDocEntries != DocEntries.end() )
      return  *ItDocEntries;
   return NULL;
}

/**
 * \brief   Gets a Dicom Element inside a SQ Item Entry
 * @param   group   Group number of the Entry
 * @param   elem  Element number of the Entry
 * @return Entry whose (group,elem) was passed. 0 if not found
 */
DocEntry *SQItem::GetDocEntry(uint16_t group, uint16_t elem)
{
   for(ListDocEntry::iterator i =  DocEntries.begin();
                              i != DocEntries.end(); 
                            ++i)
   {
      if ( (*i)->GetGroup() == group && (*i)->GetElement() == elem )
         return *i;
   }
   return NULL;
}

//-----------------------------------------------------------------------------
// Protected

//-----------------------------------------------------------------------------
// Private

//-----------------------------------------------------------------------------
// Print
/*
 * \brief   canonical Printer
 * @param os     Stream to print to. 
 * @param indent Indentation string to be prepended during printing.
 */
void SQItem::Print(std::ostream &os, std::string const &)
{
   itksys_ios::ostringstream s;

   if (SQDepthLevel > 0)
   {
      for (int i = 0; i < SQDepthLevel; ++i)
      {
         s << "   | " ;
      }
   }
   os << s.str() << " --- SQItem number " << SQItemNumber  << std::endl;
   for (ListDocEntry::iterator i  = DocEntries.begin();
                               i != DocEntries.end();
                             ++i)
   {
      DocEntry *Entry = *i;
      bool PrintEndLine = true;

      os << s.str();
      Entry->SetPrintLevel(PrintLevel);
      Entry->Print(os); 
      if ( dynamic_cast<SeqEntry*>(Entry) )
      {
         PrintEndLine = false;
      }
      if (PrintEndLine)
      {
         os << std::endl;
      }
   } 
}

//-----------------------------------------------------------------------------
} // end namespace gdcm
