/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmSeqEntry.cxx
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

#include "gdcmSeqEntry.h"
#include "gdcmSQItem.h"
#include "gdcmValEntry.h"
#include "gdcmTS.h"
#include "gdcmGlobal.h"
#include "gdcmUtil.h"
#include "gdcmDebug.h"

#include <iostream>
#include <iomanip>
#include <fstream>

namespace gdcm 
{
//-----------------------------------------------------------------------------
// Constructor / Destructor
/**
 * \brief   Constructor from a given SeqEntry
 */
SeqEntry::SeqEntry( DictEntry *e ) 
             : DocEntry(e)
{
   Length       = 0;
   ReadLength   = 0xffffffff;
   SQDepthLevel = -1;

   DelimitorMode = false;
   SeqTerm  = NULL;
}

/**
 * \brief   Constructor from a given DocEntry
 * @param   e Pointer to existing Doc entry
 * @param   depth depth level of the current Seq entry
 */
SeqEntry::SeqEntry( DocEntry *e, int depth )
             : DocEntry( e->GetDictEntry() )
{
   Length       = 0;
   ReadLength   = 0xffffffff;
   SQDepthLevel = depth;

   ImplicitVR   = e->IsImplicitVR();
   Offset       = e->GetOffset();
   SeqTerm = NULL;
}

/**
 * \brief   Canonical destructor.
 */
SeqEntry::~SeqEntry()
{
   ClearSQItem();
}

//-----------------------------------------------------------------------------
// Public
/*
 * \brief   canonical Writer
 * @param fp pointer to an already open file
 * @param filetype type of the file (ACR, ImplicitVR, ExplicitVR, ...)
 */
void SeqEntry::WriteContent(std::ofstream *fp, FileType filetype)
{
   uint16_t seq_term_gr = 0xfffe;
   uint16_t seq_term_el = 0xe0dd;
   uint32_t seq_term_lg = 0x00000000;
 
   // ignore 'Zero length' Sequences
   if ( GetReadLength() == 0 )
      return;

   DocEntry::WriteContent(fp, filetype);
   for(ListSQItem::iterator cc  = Items.begin();
                            cc != Items.end();
                          ++cc)
   {        
      (*cc)->WriteContent(fp, filetype);
   }
   
   // we force the writting of a Sequence Delimitation item
   // because we wrote the Sequence as a 'no Length' sequence
   binary_write(*fp, seq_term_gr);
   binary_write(*fp, seq_term_el);
   binary_write(*fp, seq_term_lg);
}

/**
 * \brief   Compute the full length of the SeqEntry (not only value
 *          length) depending on the VR.
 */
uint32_t SeqEntry::ComputeFullLength()
{
   uint32_t l = 12; // Tag (4) + VR (explicit) 4 + 4 (length);   
   for(ListSQItem::iterator cc  = Items.begin();
                            cc != Items.end();
                          ++cc)
   {        
      l += (*cc)->ComputeFullLength();
   }   
   l += 8; // seq_term Tag (4) +  seq_term_lg (4)
   return l;
}

/**
 * \brief   adds the passed ITEM to the ITEM chained List for this SeQuence.
 * @param sqItem SQItem to be pushed back in the SeqEntry
 * @param itemNumber ordinal number of the SQItem
 *  \note NOT end-user intendend method !
 */
void SeqEntry::AddSQItem(SQItem *sqItem, int itemNumber)
{
// FIXME : SQItemNumber is supposed to be the ordinal number of the SQItem
//         within the Sequence.
//         Either only 'push_back' is allowed, 
//                and we just have to do something like SeqEntry::lastNb++
//         Or we can add (or remove) anywhere, and SQItemNumber will be broken
   sqItem->SetSQItemNumber(itemNumber);
   Items.push_back(sqItem);
}

/**
 * \brief Remove all SQItem.
 */
void SeqEntry::ClearSQItem()
{
   for(ListSQItem::iterator cc = Items.begin(); cc != Items.end(); ++cc)
   {
      delete *cc;
   }
   if (SeqTerm)
   {
      delete SeqTerm;
   }
}

/**
 * \brief   Get the first entry while visiting the SeqEntry
 * \return  The first SQItem if found, otherwhise NULL
 */ 
SQItem *SeqEntry::GetFirstSQItem()
{
   ItSQItem = Items.begin();
   if (ItSQItem != Items.end())
      return *ItSQItem;
   return NULL;
} 

/**
 * \brief   Get the next SQItem while visiting the SeqEntry
 * \note : meaningfull only if GetFirstEntry already called
 * \return  The next SQItem if found, otherwhise NULL
 */

SQItem *SeqEntry::GetNextSQItem()
{
   gdcmAssertMacro (ItSQItem != Items.end())
   {
      ++ItSQItem;
      if (ItSQItem != Items.end())
         return *ItSQItem;
   }
   return NULL;
}
 
/**
 * \brief return a pointer to the SQItem referenced by its ordinal number.
 *        Returns the first item when argument is negative.
 *        Returns the  last item when argument is bigger than the total
 *        item number.
 */
SQItem *SeqEntry::GetSQItem(int nb)
{
   if (nb<0)
   {
      return *(Items.begin());
   }
   int count = 0 ;
   for(ListSQItem::iterator cc = Items.begin();
                           cc != Items.end();
                           count ++, ++cc)
   {
      if (count == nb)
      {
         return *cc;
      }
   }
   return *(Items.end());
}

/**
 * \brief returns the number of SQItems within the current Sequence
 */
unsigned int SeqEntry::GetNumberOfSQItems()
{
   return static_cast< unsigned int >( Items.size() );
}

//-----------------------------------------------------------------------------
// Protected

//-----------------------------------------------------------------------------
// Private

//-----------------------------------------------------------------------------
// Print
/**
 * \brief   canonical Printer
 */
void SeqEntry::Print( std::ostream &os, std::string const & )
{
   // First, Print the Dicom Element itself.
   os << "S ";
   DocEntry::Print(os);
   os << std::endl;

   if (GetReadLength() == 0)
      return;

   // Then, Print each SQ Item   
   for(ListSQItem::iterator cc = Items.begin(); cc != Items.end(); ++cc)
   {
      (*cc)->SetPrintLevel(PrintLevel);
      (*cc)->Print(os);   
   }

   // at end, print the sequence terminator item, if any
   if (DelimitorMode)
   {
      int i;
      for ( i = 0; i < SQDepthLevel; i++ )
         os << "   | " ;
      os << " --- "  << std::endl;
      for ( i = 0; i < SQDepthLevel; i++ )
         os << "   | " ;
      if (SeqTerm != NULL)
      {
         SeqTerm->SetPrintLevel(PrintLevel);
         SeqTerm->Print(os);
         os << std::endl;
      } 
      else 
      {
         // fuse
         gdcmWarningMacro("  -------- should have a sequence terminator item");
      }
   }
}

//-----------------------------------------------------------------------------
} // end namespace gdcm
