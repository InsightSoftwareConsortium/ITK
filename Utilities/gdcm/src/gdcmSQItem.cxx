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
#include "gdcmUtil.h"
#include "gdcmDebug.h"
#include <fstream>

namespace gdcm 
{
//-----------------------------------------------------------------------------
// Constructor / Destructor
/**
 * \ingroup SQItem
 * \brief   Constructor from a given SQItem
 */
SQItem::SQItem(int depthLevel ) 
          : DocEntrySet( )
{
   SQDepthLevel = depthLevel;
}

/**
 * \brief   Canonical destructor.
 */
SQItem::~SQItem() 
{
   for(ListDocEntry::iterator cc = DocEntries.begin();
                             cc != DocEntries.end();
                             ++cc)
   {
      delete *cc;
   }
   DocEntries.clear();
}

//-----------------------------------------------------------------------------
// Print
/*
 * \brief   canonical Printer
 */
 void SQItem::Print(std::ostream& os)
 {
   std::ostringstream s;

   if (SQDepthLevel > 0)
   {
      for (int i = 0; i < SQDepthLevel; ++i)
      {
         s << "   | " ;
      }
   }
   //std::cout << s.str() << " --- SQItem number " << SQItemNumber  << std::endl;
   for (ListDocEntry::iterator i  = DocEntries.begin();
                               i != DocEntries.end();
                             ++i)
   {
      DocEntry* Entry = *i;
      bool PrintEndLine = true;

      os << s.str();
      Entry->SetPrintLevel(2);
      Entry->Print(os); 
      if ( SeqEntry* seqEntry = dynamic_cast<SeqEntry*>(Entry) )
      {
         (void)seqEntry;  //not used
         PrintEndLine = false;
      }
      if (PrintEndLine)
      {
         os << std::endl;
      }
   } 
}

/*
 * \ingroup SQItem
 * \brief   canonical Writer
 */
void SQItem::Write(std::ofstream* fp, FileType filetype)
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
     
   for (ListDocEntry::iterator i = DocEntries.begin();  
                              i != DocEntries.end();
                             ++i)
   {   
      // we skip delimitors (start and end one) because 
      // we force them as 'no length'
      if ( (*i)->GetGroup() == 0xfffe )
      {
         continue;
      }

      // Fix in order to make some MR PHILIPS images e-film readable
      // see gdcmData/gdcm-MR-PHILIPS-16-Multi-Seq.dcm:
      // we just *always* ignore spurious fffe|0000 tag ! 
      if ( (*i)->GetGroup() == 0xfffe && (*i)->GetElement() == 0x0000 )
      {
         break; // FIXME : continue; ?!?
      }

      (*i)->Write(fp, filetype);
   }
      
    //we force the writting of an 'Item Delimitation' item
    // because we wrote the Item as a 'no Length' item
   for(j=0;j<4;++j)
   {
      binary_write( *fp, itemt[j]);  // fffe e000 ffff ffff 
   }
 
}

//-----------------------------------------------------------------------------
// Public
/**
 * \brief   adds any Entry (Dicom Element) to the Sequence Item
 */
bool SQItem::AddEntry(DocEntry* entry)
{
   DocEntries.push_back(entry);
   //TODO : check if it worked
   return true;
}   

/**
 * \brief   Sets Entry (Dicom Element) value of an element,
 *          specified by it's tag (Group, Number) 
 *          and the length, too ...  inside a SQ Item
 *          If the Element is not found, it's just created !
 * \warning we suppose, right now, the element belongs to a Public Group
 *          (NOT a shadow one)       
 * @param   val string value to set
 * @param   group Group number of the searched tag.
 * @param   element Element number of the searched tag.
 * @return  true if element was found or created successfully
 */

bool SQItem::SetEntryByNumber(std::string const & val, uint16_t group, 
                              uint16_t element)
{
   for(ListDocEntry::iterator i = DocEntries.begin(); 
                              i != DocEntries.end(); 
                            ++i)
   { 
      if ( (*i)->GetGroup() == 0xfffe && (*i)->GetElement() == 0xe000 ) 
      {
         continue;
      }

      if (  ( group   < (*i)->GetGroup() )
          ||( group == (*i)->GetGroup() && element < (*i)->GetElement()) )
      {
         // instead of ReplaceOrCreateByNumber 
         // that is a method of Document :-( 
         ValEntry* entry = 0;
         TagKey key = DictEntry::TranslateToKey(group, element);

         if ( ! PtagHT->count(key))
         {
            // we assume a Public Dictionnary *is* loaded
            Dict *pubDict = Global::GetDicts()->GetDefaultPubDict();
            // if the invoked (group,elem) doesn't exist inside the Dictionary
            // we create a VirtualDictEntry
            DictEntry *dictEntry = pubDict->GetDictEntryByNumber(group, element);
            if (dictEntry == NULL)
            {
               dictEntry = 
                  Global::GetDicts()->NewVirtualDictEntry(group, element,
                                                          "UN", "??", "??");
            } 
            // we assume the constructor didn't fail
            entry = new ValEntry(dictEntry);
            /// \todo
            /// ----
            /// better we don't assume too much !
            /// SQItem is now used to describe any DICOMDIR related object
         }
         else
         {
            DocEntry* foundEntry = PtagHT->find(key)->second;
            entry = dynamic_cast<ValEntry*>(foundEntry);
            if (!entry)
            {
               dbg.Verbose(0, "SQItem::SetEntryByNumber: docEntries"
                              " contains non ValEntry occurences");
            }
         }
         if (entry)
         {
            entry->SetValue(val); 
         }
         entry->SetLength(val.length());
         DocEntries.insert(i,entry);

         return true;
      }   
      if (group == (*i)->GetGroup() && element == (*i)->GetElement() )
      {
         if ( ValEntry* entry = dynamic_cast<ValEntry*>(*i) )
         {
            entry->SetValue(val);
         }
         (*i)->SetLength(val.length()); 
         return true;    
      }
   }
   return false;
}
//-----------------------------------------------------------------------------
// Protected


/**
 * \brief   Gets a Dicom Element inside a SQ Item Entry, by number
 * @return
 */
DocEntry* SQItem::GetDocEntryByNumber(uint16_t group, uint16_t element)
{
   for(ListDocEntry::iterator i = DocEntries.begin();
                              i != DocEntries.end(); ++i)
   {
      if ( (*i)->GetGroup() == group && (*i)->GetElement() == element )
      {
         return *i;
      }
   }
   return 0;
}

/**
 * \brief   Get the value of a Dicom Element inside a SQ Item Entry, by number
 * @return
 */ 

std::string SQItem::GetEntryByNumber(uint16_t group, uint16_t element)
{
   for(ListDocEntry::iterator i = DocEntries.begin();
                              i != DocEntries.end(); ++i)
   {
      if ( (*i)->GetGroup() == group && (*i)->GetElement() == element)
      {
         return ((ValEntry *)(*i))->GetValue();   //FIXME
      }
   }
   return GDCM_UNFOUND;
}
//-----------------------------------------------------------------------------
// Private


//-----------------------------------------------------------------------------

} // end namespace gdcm
