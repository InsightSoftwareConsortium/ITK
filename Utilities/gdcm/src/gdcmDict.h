/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmDict.h
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

#ifndef GDCMDICT_H
#define GDCMDICT_H

#include "gdcmCommon.h"
#include "gdcmDictEntry.h"

#include <iostream>
#include <list>
#include <map>

namespace gdcm 
{

//-----------------------------------------------------------------------------
typedef std::map<TagKey, DictEntry> TagKeyHT;
typedef std::map<TagName, DictEntry> TagNameHT;
typedef std::list<std::string>        EntryNamesList;
typedef std::map<std::string, std::list<std::string> > EntryNamesByCatMap;
//-----------------------------------------------------------------------------
/*
 * \defgroup Dict
 * \brief    Dict acts a memory representation of a dicom dictionary i.e.
 *           it is a container for a collection of dictionary entries. The
 *           dictionary is loaded from in an ascii file.
 *           There should be a single public dictionary (THE dictionary of
 *           the actual DICOM v3) but as many shadow dictionaries as imagers 
 *           combined with all software versions...
 * \see DictSet
 */
class GDCM_EXPORT Dict
{
public:
   Dict(std::string const & filename);
   ~Dict();

// Print
   void Print(std::ostream &os = std::cout);
   void PrintByKey(std::ostream &os = std::cout);
   void PrintByName(std::ostream &os = std::cout);

// Entries
   bool AddNewEntry (DictEntry const & newEntry);
   bool ReplaceEntry(DictEntry const & newEntry);
   bool RemoveEntry (TagKey const & key);
   bool RemoveEntry (uint16_t group, uint16_t element);
   
// Tag
   DictEntry *GetDictEntryByName(TagName const & name);
   DictEntry *GetDictEntryByNumber(uint16_t group, uint16_t element);

   EntryNamesList *GetDictEntryNames();
   EntryNamesByCatMap *GetDictEntryNamesByCategory();

   /// \brief  Returns a ref to the Dicom Dictionary H table (map)
   /// @return the Dicom Dictionary H table
   const TagKeyHT & GetEntriesByKey() const { return KeyHt; }

   /// \brief  Returns a ref to the Dicom Dictionary H table (map)
   /// @return the Dicom Dictionary H table
   const TagNameHT & GetEntriesByName() const { return NameHt; }
 
private:
   /// ASCII file holding the Dictionnary
   std::string Filename;

   /// Access through TagKey (see alternate access with NameHt)
   TagKeyHT  KeyHt;

   /// Access through TagName (see alternate access with KeyHt)
   TagNameHT NameHt;
};
} // end namespace gdcm

//-----------------------------------------------------------------------------
#endif
