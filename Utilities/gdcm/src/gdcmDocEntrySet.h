/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmDocEntrySet.h
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

#ifndef GDCMDOCENTRYSET_H
#define GDCMDOCENTRYSET_H

#include "gdcmException.h"
#include <fstream>

namespace gdcm 
{
class DocEntry;
class ValEntry;
class BinEntry;
class SeqEntry;
class DictEntry;

typedef std::string BaseTagKey;
//-----------------------------------------------------------------------------

/**
 * \ref DocEntrySet is an abstract base class for \ref ElementSet
 * and \ref SQItem which are both containers for DocEntries.
 * \ref ElementSet is based on the STL map<> container
 * (see \ref ElementSet::TagHT), as opposed to \ref SQItem
 * which is based on an STL list container (see \ref SQItem::docEntries).
 * Since the syntax for adding a new element to a map<> or a list<>
 * differ, \ref DocEntrySet is designed as an adapter to unify the
 * interfaces of \ref DocEntrySet and \ref ElementSet.
 * As an illustration of this design, please refer to the implementation
 * of \ref AddEntry (or any pure virtual method) in both derived classes.
 * This adapter unification of interfaces enables the parsing of a
 * DICOM header containing (optionaly heavily nested) sequences to be
 * written recursively [see \ref Document::ParseDES 
 * which calls \ref Document::ParseSQ, which in turns calls 
 * \ref Document::ParseDES ].
 *
 * \note Developpers should strongly resist to the temptation of adding
 *       members to this class since this class is designed as an adapter 
 *       in the form of an abstract base class.
 */
class GDCM_EXPORT DocEntrySet
{
public:
   DocEntrySet() {};
   virtual ~DocEntrySet() {};

   /// \brief adds any type of entry to the entry set (pure vitual)
   virtual bool AddEntry(DocEntry *Entry) = 0; // pure virtual
 
   /// \brief prints any type of entry to the entry set (pure vitual)
   virtual void Print (std::ostream & os = std::cout) = 0;// pure virtual

   /// \brief write any type of entry to the entry set
   virtual void Write (std::ofstream *fp, FileType filetype) = 0;// pure virtual

   virtual DocEntry* GetDocEntryByNumber(uint16_t group,
                                         uint16_t element) = 0;
   DocEntry *GetDocEntryByName(TagName const & name);
   virtual std::string GetEntryByNumber(uint16_t group, uint16_t element) = 0;
   std::string GetEntryByName(TagName const & name);
   DictEntry *NewVirtualDictEntry(uint16_t group, 
                                  uint16_t element,
                                  TagName const & vr     = "unkn",
                                  TagName const & fourth = "unkn",
                                  TagName const & name   = "unkn");
  
protected:

// DocEntry  related utilities 
   ValEntry* NewValEntryByNumber(uint16_t group, 
                                 uint16_t element);
   BinEntry* NewBinEntryByNumber(uint16_t group, 
                                 uint16_t element);
   DocEntry* NewDocEntryByNumber(uint16_t group, 
                                 uint16_t element); 
   DocEntry* NewDocEntryByNumber(uint16_t group, 
                                 uint16_t element,
                                 TagName const & vr); 
   DocEntry* NewDocEntryByName  (TagName const & name);
   SeqEntry* NewSeqEntryByNumber(uint16_t group, 
                                 uint16_t element);

// DictEntry  related utilities
   DictEntry *GetDictEntryByName  (TagName const & name);
   DictEntry *GetDictEntryByNumber(uint16_t, uint16_t);

};

} // end namespace gdcm
//-----------------------------------------------------------------------------
#endif

