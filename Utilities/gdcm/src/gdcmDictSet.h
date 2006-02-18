/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmDictSet.h
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

#ifndef GDCMDICTSET_H
#define GDCMDICTSET_H

#include "gdcmBase.h"
#include "gdcmDict.h"

#include <map>
#include <list>

namespace gdcm 
{
//-----------------------------------------------------------------------------
typedef std::map<DictKey, Dict*> DictSetHT;
typedef std::string ExtendedTagKey;
typedef std::map<ExtendedTagKey, DictEntry>  ExtendedTagKeyHT;

//-----------------------------------------------------------------------------
/**
 * \brief  Container for managing a set of loaded dictionaries (Dict).
 * \note   Hopefully, sharing dictionaries should avoid
 *         - reloading an already loaded dictionary (saving time)
 *         - having many in memory representations of the same dictionary
 *        (saving memory).
 */
class GDCM_EXPORT DictSet : public Base
{
public:
   DictSet();
   ~DictSet();

   void Print(std::ostream &os = std::cout, std::string const &indent = "" );

   // Probabely useless !
   //EntryNamesList *GetPubDictEntryNames();
   //EntryNamesByCatMap *GetPubDictEntryNamesByCategory();

   Dict *LoadDictFromFile( std::string const &fileName,
                           DictKey const &name );

   Dict *GetDict( DictKey const &DictName );

   /// \brief   Returns the default reference DICOM V3 public dictionary.
   Dict* GetDefaultPubDict() { return GetDict(PUB_DICT_NAME); }

   // \ brief   Returns the virtual references DICOM dictionary.
   // \ warning : not end user intended
   // Dict *GetVirtualDict() { return &VirtualEntries; }

   DictEntry *NewVirtualDictEntry(uint16_t group, uint16_t elem,
                                  TagName vr     = GDCM_UNKNOWN,
                                  TagName vm     = GDCM_UNKNOWN,
                                  TagName name   = GDCM_UNKNOWN);

   Dict *GetFirstEntry();
   Dict *GetNextEntry();

   static std::string BuildDictPath();

protected:
   bool AppendDict(Dict *NewDict, DictKey const &name);

private:
   /// Hash table of all dictionaries contained in this DictSet
   DictSetHT Dicts;
   DictSetHT::iterator ItDictHt;

   /// Directory path to dictionaries
   std::string DictPath;

   /// H table for the on the fly created DictEntries  
   ExtendedTagKeyHT VirtualEntries; 
};
} // end namespace gdcm

//-----------------------------------------------------------------------------
#endif
