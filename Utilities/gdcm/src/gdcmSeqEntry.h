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

#include "gdcmSQItem.h"
#include "gdcmDocEntry.h"
#include <list>
namespace gdcm 
{

//-----------------------------------------------------------------------------
typedef std::list<SQItem *> ListSQItem;
//-----------------------------------------------------------------------------

class GDCM_EXPORT SeqEntry : public DocEntry 
{
public:
   SeqEntry( DictEntry* );
   SeqEntry(DocEntry* d,  int depth);
   ~SeqEntry();
   
   virtual void Print(std::ostream &os = std::cout); 
   virtual void Write(std::ofstream *fp, FileType);

   /// returns the SQITEM chained List for this SeQuence.
   ListSQItem const & GetSQItems() const { return Items; }
      
   /// Sets the delimitor mode
   void SetDelimitorMode(bool dm) { DelimitorMode = dm;}

   /// Sets the Sequence Delimitation Item
   void SetSequenceDelimitationItem(DocEntry * e) { SeqTerm = e;}

   void AddEntry(SQItem *it, int itemNumber);
   SQItem *GetSQItemByOrdinalNumber(int itemNumber);

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

