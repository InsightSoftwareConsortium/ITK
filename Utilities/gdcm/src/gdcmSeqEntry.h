/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmSeqEntry.h
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

#ifndef GDCMSQDOCENTRY_H
#define GDCMSQDOCENTRY_H

#include "gdcmDocEntry.h"

#include <list>

namespace gdcm 
{
class SQItem;
//-----------------------------------------------------------------------------
typedef std::list<SQItem *> ListSQItem;

//-----------------------------------------------------------------------------
/**
 * \brief a SeqEntry (as opposed to a ValEntry) is a non elementary DocEntry.
 *        It is composed by a set of SQItems.
 *        Each SQItem is composed by a set of DocEntry
 *        A DocEntry may be a SeqEntry
 *        ... and so forth 
 */ 
class GDCM_EXPORT SeqEntry : public DocEntry 
{
public:
   SeqEntry( DictEntry *e);
   SeqEntry( DocEntry *d, int depth );
   ~SeqEntry();
   
   void Print(std::ostream &os = std::cout, std::string const &indent = "" ); 
   void WriteContent(std::ofstream *fp, FileType filetype);
   uint32_t ComputeFullLength();

   void AddSQItem(SQItem *it, int itemNumber);
   void ClearSQItem();
   SQItem *GetFirstSQItem();
   SQItem *GetNextSQItem();
   SQItem *GetSQItem(int itemNumber);
   unsigned int GetNumberOfSQItems();
      
   /// Sets the delimitor mode
   void SetDelimitorMode(bool dm) { DelimitorMode = dm; }
   /// Sets the Sequence Delimitation Item
   void SetDelimitationItem(DocEntry *e) { SeqTerm = e;   }

   /// Gets the Sequence Delimitation Item
   DocEntry *GetDelimitationItem() { return SeqTerm;}

   /// Gets the depth level
   int GetDepthLevel() const { return SQDepthLevel; }
   /// Sets the depth level of a Sequence Entry embedded in a SeQuence
   void SetDepthLevel(int depth) { SQDepthLevel = depth; }

protected:

private:
// Variables
   /// If this Sequence is in delimitor mode (length =0xffffffff) or not
   bool DelimitorMode;
   
   /// Chained list of SQ Items
   ListSQItem Items;
   /// iterator on the SQItems of the current SeqEntry
   ListSQItem::iterator ItSQItem;

   /// sequence terminator item 
   DocEntry *SeqTerm;

   /// \brief Defines the depth level of this \ref SeqEntry inside
   ///        the (optionaly) nested sequences. \ref SQDepthLevel
   ///        and its \ref SQItem::SQDepthLevel counterpart
   ///        are only defined on printing purposes (see \ref Print).
   int SQDepthLevel;
};
} // end namespace gdcm
//-----------------------------------------------------------------------------
#endif

