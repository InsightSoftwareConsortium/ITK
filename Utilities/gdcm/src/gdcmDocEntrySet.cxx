/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmDocEntrySet.cxx
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
#include "gdcmDocEntrySet.h"

#include "gdcmDebug.h"
#include "gdcmCommon.h"
#include "gdcmGlobal.h"
#include "gdcmException.h"
#include "gdcmDocEntry.h"
#include "gdcmSeqEntry.h"
#include "gdcmValEntry.h"
#include "gdcmBinEntry.h"

namespace gdcm 
{

//-----------------------------------------------------------------------------
// Public

/**
 * \brief   Build a new Val Entry from all the low level arguments. 
 *          Check for existence of dictionary entry, and build
 *          a default one when absent.
 * @param   group group   number of the underlying DictEntry
 * @param   elem  element number of the underlying DictEntry
 */
ValEntry *DocEntrySet::NewValEntryByNumber(uint16_t group,
                                           uint16_t elem) 
{
   // Find out if the tag we encountered is in the dictionaries:
   DictEntry *dictEntry = GetDictEntryByNumber(group, elem);
   if (!dictEntry)
   {
      dictEntry = NewVirtualDictEntry(group, elem);
   }

   ValEntry *newEntry = new ValEntry(dictEntry);
   if (!newEntry) 
   {
      dbg.Verbose(1, "Document::NewValEntryByNumber",
                  "failed to allocate ValEntry");
      return 0;
   }
   return newEntry;
}


/**
 * \brief   Build a new Bin Entry from all the low level arguments. 
 *          Check for existence of dictionary entry, and build
 *          a default one when absent.
 * @param   group group   number of the underlying DictEntry
 * @param   elem  element number of the underlying DictEntry
 */
BinEntry *DocEntrySet::NewBinEntryByNumber(uint16_t group,
                                           uint16_t elem) 
{
   // Find out if the tag we encountered is in the dictionaries:
   DictEntry *dictEntry = GetDictEntryByNumber(group, elem);
   if (!dictEntry)
   {
      dictEntry = NewVirtualDictEntry(group, elem);
   }

   BinEntry *newEntry = new BinEntry(dictEntry);
   if (!newEntry) 
   {
      dbg.Verbose(1, "Document::NewBinEntryByNumber",
                  "failed to allocate BinEntry");
      return 0;
   }
   return newEntry;
}

/**
 * \brief   Build a new Seq Entry from all the low level arguments. 
 *          Check for existence of dictionary entry, and build
 *          a default one when absent.
 * @param   Group group   number of the underlying DictEntry
 * @param   Elem  element number of the underlying DictEntry
 */
SeqEntry* DocEntrySet::NewSeqEntryByNumber(uint16_t Group,
                                                   uint16_t Elem) 
{
   // Find out if the tag we encountered is in the dictionaries:
   DictEntry* DictEntry = GetDictEntryByNumber( Group, Elem );
   if ( ! DictEntry )
   {
      DictEntry = NewVirtualDictEntry(Group, Elem);
   }

   SeqEntry *NewEntry = new SeqEntry( DictEntry );
   if ( !NewEntry ) 
   {
      dbg.Verbose(1, "Document::NewSeqEntryByNumber",
                  "failed to allocate SeqEntry");
      return 0;
   }
   return NewEntry;
}

//-----------------------------------------------------------------------------
// Protected

/**
 * \brief   Gets a Dicom Element inside a SQ Item Entry, by name
 * @return
 */
 DocEntry *DocEntrySet::GetDocEntryByName(std::string const & name)
 {
   Dict *pubDict = Global::GetDicts()->GetDefaultPubDict();
   DictEntry *dictEntry = pubDict->GetDictEntryByName(name);
   if( !dictEntry )
   {
      return 0;
   }

   return GetDocEntryByNumber(dictEntry->GetGroup(),dictEntry->GetElement());      
}


/**
 * \brief   Get the value of a Dicom Element inside a SQ Item Entry, by name
 * @param   name : name of the searched element.
 * @return
 */ 

std::string DocEntrySet::GetEntryByName(TagName const & name)
{
   Dict *pubDict = Global::GetDicts()->GetDefaultPubDict();
   DictEntry *dictEntry = pubDict->GetDictEntryByName(name); 

   if( !dictEntry )
   {
      return GDCM_UNFOUND;
   }

   return GetEntryByNumber(dictEntry->GetGroup(), dictEntry->GetElement()); 
}


/**
 * \brief   Request a new virtual dict entry to the dict set
 * @param   group     group  number of the underlying DictEntry
 * @param   element  element number of the underlying DictEntry
 * @param   vr     VR of the underlying DictEntry
 * @param   fourth owner group
 * @param   name   english name
 */
DictEntry* DocEntrySet::NewVirtualDictEntry(uint16_t group,
                                                    uint16_t element,
                                                    std::string const & vr,
                                                    std::string const & fourth,
                                                    std::string const & name)
{
   return Global::GetDicts()->NewVirtualDictEntry(group,element,vr,fourth,name);
}

/** \brief 
 * Creates a new DocEntry (without any 'value' ...)
 * @param   group     group  number of the underlying DictEntry
 * @param   elem  elem number of the underlying DictEntry 
 */
DocEntry* DocEntrySet::NewDocEntryByNumber(uint16_t group,
                                           uint16_t elem)
{
   // Find out if the tag we encountered is in the dictionaries:
   Dict *pubDict = Global::GetDicts()->GetDefaultPubDict();
   DictEntry *dictEntry = pubDict->GetDictEntryByNumber(group, elem);
   if (!dictEntry)
   {
      dictEntry = NewVirtualDictEntry(group, elem);
   }

   DocEntry *newEntry = new DocEntry(dictEntry);
   if (!newEntry) 
   {
      dbg.Verbose(1, "SQItem::NewDocEntryByNumber",
                  "failed to allocate DocEntry");
      return 0;
   }
   return newEntry;
}


/** \brief 
 * Creates a new DocEntry (without any 'value' ...)
 * @param   group     group  number of the underlying DictEntry
 * @param   elem  elem number of the underlying DictEntry 
 * @param   VR   V(alue) R(epresentation) of the Entry -if private Entry- 

 */
DocEntry* DocEntrySet::NewDocEntryByNumber(uint16_t group, uint16_t elem,
                                           TagName const & vr)
{
   // Find out if the tag we encountered is in the dictionaries:
   Dict *pubDict = Global::GetDicts()->GetDefaultPubDict();
   DictEntry *dictEntry = pubDict->GetDictEntryByNumber(group, elem);
   if (!dictEntry)
   {
      dictEntry = NewVirtualDictEntry(group, elem, vr);
   }

   DocEntry *newEntry = new DocEntry(dictEntry);
   if (!newEntry) 
   {
      dbg.Verbose(1, "SQItem::NewDocEntryByNumber",
                  "failed to allocate DocEntry");
      return 0;
   }
   return newEntry;
}
/* \brief
 * Probabely move, as is, to DocEntrySet, as a non virtual method
 * and remove Document::NewDocEntryByName
 */
DocEntry *DocEntrySet::NewDocEntryByName(TagName const & name)
{
  Dict *pubDict = Global::GetDicts()->GetDefaultPubDict();
  DictEntry *newTag = pubDict->GetDictEntryByName(name);
   if (!newTag)
   {
      newTag = NewVirtualDictEntry(0xffff, 0xffff, "LO", "unkn", name);
   }

   DocEntry* newEntry = new DocEntry(newTag);
   if (!newEntry) 
   {
      dbg.Verbose(1, "SQItem::ObtainDocEntryByName",
                  "failed to allocate DocEntry");
      return 0;
   }

   return newEntry;
}


/**
 * \brief   Searches both the public and the shadow dictionary (when they
 *          exist) for the presence of the DictEntry with given name.
 *          The public dictionary has precedence on the shadow one.
 * @param   name Name of the searched DictEntry
 * @return  Corresponding DictEntry when it exists, NULL otherwise.
 */
DictEntry *DocEntrySet::GetDictEntryByName(TagName const & name) 
{
   DictEntry *found = 0;
   Dict *pubDict = Global::GetDicts()->GetDefaultPubDict();
   if (!pubDict) 
   {
      dbg.Verbose(0, "Document::GetDictEntry",
                     "we SHOULD have a default dictionary");
   }
   else
   {
      found = pubDict->GetDictEntryByName(name);  
   }
   return found;
}

/**
 * \brief   Searches both the public and the shadow dictionary (when they
 *          exist) for the presence of the DictEntry with given
 *          group and element. The public dictionary has precedence on the
 *          shadow one.
 * @param   group   group number of the searched DictEntry
 * @param   element element number of the searched DictEntry
 * @return  Corresponding DictEntry when it exists, NULL otherwise.
 */
DictEntry *DocEntrySet::GetDictEntryByNumber(uint16_t group, 
                                             uint16_t element) 
{
   DictEntry *found = 0;
   Dict *pubDict = Global::GetDicts()->GetDefaultPubDict();
   if (!pubDict) 
   {
      dbg.Verbose(0, "Document::GetDictEntry",
                     "we SHOULD have a default dictionary");
   }
   else
   {
      found = pubDict->GetDictEntryByNumber(group, element);  
   }
   return found;
}


//-----------------------------------------------------------------------------
// Private

} // end namespace gdcm

//-----------------------------------------------------------------------------
