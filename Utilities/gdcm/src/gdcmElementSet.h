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

#include "gdcmCommon.h"
#include "gdcmDocEntrySet.h"
#include <map>
#include <iostream>

class ValEntry;
class BinEntry;
class SeqEntry;

namespace gdcm 
{
typedef std::map<TagKey, DocEntry *> TagDocEntryHT;

//-----------------------------------------------------------------------------

class GDCM_EXPORT ElementSet : public DocEntrySet
{
public:
   ElementSet(int);
   ~ElementSet();

   bool AddEntry(DocEntry *Entry);
   bool RemoveEntry(DocEntry *EntryToRemove);
   bool RemoveEntryNoDestroy(DocEntry *EntryToRemove);
   
   void Print(std::ostream &os = std::cout); 
   void Write(std::ofstream *fp, FileType filetype); 
   
   /// Accessor to \ref TagHT
   // Do not expose this to user (public API) ! 
   // A test is using it thus put it in public (matt)
   TagDocEntryHT const & GetTagHT() const { return TagHT; };

protected:
    
private:
// Variables
   /// Hash Table (map), to provide fast access
   TagDocEntryHT TagHT; 
 
   friend class Document;
   friend class DicomDir; //For accessing private TagHT
};
} // end namespace gdcm
//-----------------------------------------------------------------------------
#endif

