/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmSQItem.h
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
#ifndef GDCMSQITEM_H
#define GDCMSQITEM_H

#include "gdcmDocEntrySet.h"
#include "gdcmElementSet.h"

#include <list>
#include <fstream>

namespace gdcm 
{
class DocEntry;

//-----------------------------------------------------------------------------
typedef std::list<DocEntry *> ListDocEntry;
//-----------------------------------------------------------------------------
class GDCM_EXPORT SQItem : public DocEntrySet 
{
public:
   SQItem(int depthLevel);
   ~SQItem();

   void Print(std::ostream &os = std::cout); 
   void Write(std::ofstream *fp, FileType filetype);

   /// \brief   returns the DocEntry chained List for this SQ Item.
   ListDocEntry const & GetDocEntries() const { return DocEntries; };
   
   /// \brief   adds the passed DocEntry to the DocEntry chained List for
   /// this SQ Item.      
   void AddDocEntry(DocEntry *e) { DocEntries.push_back(e); };

   virtual bool AddEntry(DocEntry *Entry); // add to the List
  
   DocEntry *GetDocEntryByNumber(uint16_t group, uint16_t element);
   // FIXME method to write
   //DocEntry *GetDocEntryByName  (std::string Name);
   
   bool SetEntryByNumber(std::string const & val, uint16_t group, 
                                                  uint16_t element);
    
   std::string GetEntryByNumber(uint16_t group, uint16_t element);

   /// \brief   returns the ordinal position of a given SQItem
   int GetSQItemNumber() { return SQItemNumber; };

   /// \brief   Sets the ordinal position of a given SQItem
   void SetSQItemNumber(int itemNumber) { SQItemNumber = itemNumber; };

   /// Accessor on \ref SQDepthLevel.
   int GetDepthLevel() { return SQDepthLevel; }
                                                                                
   /// Accessor on \ref SQDepthLevel.
   void SetDepthLevel(int depth) { SQDepthLevel = depth; }

   /// Accessor on \ref BaseTagKey.
   void SetBaseTagKey( BaseTagKey const & key ) { BaseTagKeyNested = key; }

   /// Accessor on \ref BaseTagKey.
   BaseTagKey const & GetBaseTagKey() const { return BaseTagKeyNested; }


protected:
// Variables that need to be access in subclasses

   /// \brief chained list of (Elementary) Doc Entries
   ListDocEntry DocEntries;
   
   ///\brief pointer to the HTable of the Document,
   ///       (because we don't know it within any DicomDirObject nor any SQItem)
   TagDocEntryHT *PtagHT;

private:

   /// \brief Sequences can be nested. This \ref SQDepthLevel represents
   ///        the level of the nesting of instances of this class.
   ///        \ref SQDepthLevel and its \ref SeqEntry::SQDepthLevel
   ///        counterpart are only defined on printing purposes
   ///        (see \ref Print).
   int SQDepthLevel;

   /// \brief A TagKey of a DocEntry nested in a sequence is prepended
   ///        with this BaseTagKey.
   BaseTagKey BaseTagKeyNested;

 
   /// \brief SQ Item ordinal number 
   int SQItemNumber;

};
} // end namespace gdcm
//-----------------------------------------------------------------------------
#endif
