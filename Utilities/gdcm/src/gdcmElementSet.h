/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmElementSet.h
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

#ifndef GDCMELEMENTSET_H
#define GDCMELEMENTSET_H

#include "gdcmDocEntrySet.h"

#include <map>
#include <iostream>
#include <fstream>

namespace gdcm 
{
class ValEntry;
class BinEntry;
class SeqEntry;

typedef std::map<TagKey, DocEntry *> TagDocEntryHT;

//-----------------------------------------------------------------------------
/**
 * \brief
 * \ref ElementSet is based on the STL map<> container
 * (see \ref ElementSet::TagHT), as opposed to \ref SQItem
 * which is based on an STL list container (see \ref ListDocEntry).
 * It contains the 'zero-level- DocEntry (out of any Dicom Sequence)
 */
class GDCM_EXPORT ElementSet : public DocEntrySet
{
public:
   ElementSet(int);
   ~ElementSet();

   virtual void Print(std::ostream &os = std::cout, 
                      std::string const &indent = "" ); 

   void WriteContent(std::ofstream *fp, FileType filetype); 

   bool AddEntry(DocEntry *Entry);
   bool RemoveEntry(DocEntry *EntryToRemove);
   bool RemoveEntryNoDestroy(DocEntry *EntryToRemove);
   void ClearEntry();
   
   DocEntry *GetFirstEntry();
   DocEntry *GetNextEntry();
   DocEntry *GetDocEntry(uint16_t group, uint16_t elem);
   /// Tells us if the ElementSet contains no entry
   bool IsEmpty() { return TagHT.empty(); }

protected:

private:
// Variables
   /// Hash Table (map), to provide fast access
   TagDocEntryHT TagHT;
   /// iterator, used to visit the TagHT variable
   TagDocEntryHT::iterator ItTagHT;
   /// iterator, used to visit the TagHT variable, seeking only for ValEntries
   TagDocEntryHT::iterator ItValEntryTagHT;
};
} // end namespace gdcm
//-----------------------------------------------------------------------------
#endif

