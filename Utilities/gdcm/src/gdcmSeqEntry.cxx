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
#include "gdcmTS.h"
#include "gdcmGlobal.h"
#include "gdcmUtil.h"

#include <iostream>
#include <iomanip>
#include <fstream>

namespace gdcm 
{

//-----------------------------------------------------------------------------
// Constructor / Destructor
/**
 * \ingroup SeqEntry
 * \brief   Constructor from a given SeqEntry
 */
SeqEntry::SeqEntry( DictEntry* e ) 
             : DocEntry(e)
{
   UsableLength = 0;
   ReadLength = 0xffffffff;
   SQDepthLevel = -1;

   DelimitorMode = false;
   SeqTerm  = NULL;
}

/**
 * \brief   Constructor from a given SeqEntry
 * @param   e Pointer to existing Doc entry
 * @param   depth depth level of the current Seq entry
  */
SeqEntry::SeqEntry( DocEntry* e, int depth )
             : DocEntry( e->GetDictEntry() )
{
   this->UsableLength = 0;
   this->ReadLength   = 0xffffffff;
   SQDepthLevel = depth;

   this->ImplicitVR   = e->IsImplicitVR();
   this->Offset       = e->GetOffset();
}

/**
 * \brief   Canonical destructor.
 */
SeqEntry::~SeqEntry()
{
   for(ListSQItem::iterator cc = Items.begin(); cc != Items.end(); ++cc)
   {
      delete *cc;
   }
   if (!SeqTerm)
   {
      delete SeqTerm;
   }
}

/**
 * \brief   canonical Printer
 */
void SeqEntry::Print( std::ostream &os )
{
   // First, Print the Dicom Element itself.
   SetPrintLevel(2);   
   DocEntry::Print(os);
   os << std::endl;

   if (GetReadLength() == 0)
      return;

   // Then, Print each SQ Item   
   for(ListSQItem::iterator cc = Items.begin(); cc != Items.end(); ++cc)
   {
      (*cc)->Print(os);   
   }

   // at end, print the sequence terminator item, if any
   if (DelimitorMode)
   {
      for ( int i = 0; i < SQDepthLevel; i++ )
      {
         os << "   | " ;
      }
      if (SeqTerm != NULL)
      {
         SeqTerm->Print(os);
         os << std::endl;
      } 
      else 
      {
         // fuse
         os << "      -------------- should have a sequence terminator item";
      }
   }                    
}

/*
 * \brief   canonical Writer
 */
void SeqEntry::Write(std::ofstream* fp, FileType filetype)
{
   uint16_t seq_term_gr = 0xfffe;
   uint16_t seq_term_el = 0xe0dd;
   uint32_t seq_term_lg = 0xffffffff;

   //uint16_t item_term_gr = 0xfffe;
   //uint16_t item_term_el = 0xe00d;
   
   DocEntry::Write(fp, filetype);
   for(ListSQItem::iterator cc  = Items.begin();
                            cc != Items.end();
                          ++cc)
   {        
      (*cc)->Write(fp, filetype);
   }
   
   // we force the writting of a Sequence Delimitation item
   // because we wrote the Sequence as a 'no Length' sequence
   binary_write(*fp, seq_term_gr);
   binary_write(*fp, seq_term_el);
   binary_write(*fp, seq_term_lg);
}

//-----------------------------------------------------------------------------
// Public

/// \brief   adds the passed ITEM to the ITEM chained List for this SeQuence.
void SeqEntry::AddEntry(SQItem *sqItem, int itemNumber)
{
   sqItem->SetSQItemNumber(itemNumber);
   Items.push_back(sqItem);
}

/**
 * \brief return a pointer to the SQItem referenced by its ordinal number.
 *        Returns the first item when argument is negative.
 *        Returns the last item when argument is bigget than the total
 *        item number.
 */
SQItem* SeqEntry::GetSQItemByOrdinalNumber(int nb)
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
   return *(Items.end()); // Euhhhhh ?!? Is this the last one . FIXME
}
//-----------------------------------------------------------------------------
// Protected


//-----------------------------------------------------------------------------
// Private

//-----------------------------------------------------------------------------
} // end namespace gdcm

