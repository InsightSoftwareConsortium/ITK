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
#include "gdcmDictSet.h"
#include "gdcmGlobal.h"
#include "gdcmDocEntry.h"
#include "gdcmSeqEntry.h"
#include "gdcmValEntry.h"
#include "gdcmBinEntry.h"

namespace gdcm 
{
//-----------------------------------------------------------------------------
// Constructor / Destructor

//-----------------------------------------------------------------------------
// Public
/**
 * \brief   Get the "std::string representable" value of the Dicom entry
 * @param   group  Group number of the searched tag.
 * @param   elem Element number of the searched tag.
 * @return  Corresponding element value when it exists,
 *          and the string GDCM_UNFOUND ("gdcm::Unfound") otherwise.
 */
std::string DocEntrySet::GetEntryValue(uint16_t group, uint16_t elem)
{
   ContentEntry *entry = dynamic_cast<ContentEntry *>(GetDocEntry(group,elem));
   if( entry )
      return entry->GetValue();
   return GDCM_UNFOUND;
}

/**
 * \brief   Gets (from Header) a 'non string' element value 
 * @param group   group number of the Entry 
 * @param elem  element number of the Entry
 * @return Pointer to the 'non string' area
 */
void *DocEntrySet::GetEntryBinArea(uint16_t group, uint16_t elem) 
{
   BinEntry *entry = GetBinEntry(group, elem);
   if( entry )
      return entry->GetBinArea();
   return 0;
}

/**
 * \brief   Searches within Header Entries (Dicom Elements) parsed with 
 *          the public and private dictionaries 
 *          for the value length of a given tag..
 * @param   group  Group number of the searched tag.
 * @param   elem Element number of the searched tag.
 * @return  Corresponding element length; -2 if not found
 */
int DocEntrySet::GetEntryLength(uint16_t group, uint16_t elem)
{
   DocEntry *entry = GetDocEntry(group, elem);
   if( entry )
      return entry->GetLength();
   return -1;
}

/**
 * \brief   Searches within Header Entries (Dicom Elements) parsed with 
 *          the public [and private dictionaries] 
 *          for the element value representation of a given tag..
 *          Obtaining the VR (Value Representation) might be needed by caller
 *          to convert the string typed content to caller's native type 
 *          (think of C++ vs Python). The VR is actually of a higher level
 *          of semantics than just the native C++ type.
 * @param   group  Group number of the searched tag.
 * @param   elem Element number of the searched tag.
 * @return  Corresponding element value representation when it exists,
 *          and the string GDCM_UNFOUND ("gdcm::Unfound") otherwise.
 */
std::string DocEntrySet::GetEntryVR(uint16_t group, uint16_t elem)
{
   DocEntry *entry = GetDocEntry(group, elem);
   if( entry )
      return entry->GetVR();
   return GDCM_UNFOUND;
}

/**
 * \brief  Same as \ref Document::GetDocEntry except it only
 *         returns a result when the corresponding entry is of type
 *         ValEntry.
 * @param   group  Group number of the searched Dicom Element 
 * @param   elem Element number of the searched Dicom Element  
 * @return When present, the corresponding ValEntry. 
 */
ValEntry *DocEntrySet::GetValEntry(uint16_t group, uint16_t elem)
{
   DocEntry *currentEntry = GetDocEntry(group, elem);
   if ( !currentEntry )
      return NULL;

   return dynamic_cast<ValEntry*>(currentEntry);
}

/**
 * \brief  Same as \ref Document::GetDocEntry except it only
 *         returns a result when the corresponding entry is of type
 *         BinEntry.
 * @param   group  Group number of the searched Dicom Element
 * @param   elem Element number of the searched Dicom Element
 * @return When present, the corresponding BinEntry. 
 */
BinEntry *DocEntrySet::GetBinEntry(uint16_t group, uint16_t elem)
{
   DocEntry *currentEntry = GetDocEntry(group, elem);
   if ( !currentEntry )
   {
      gdcmWarningMacro( "No corresponding BinEntry " << std::hex << group <<
                         "," << elem);
      return NULL;
   }

   return dynamic_cast<BinEntry*>(currentEntry);
}

/**
 * \brief  Same as \ref Document::GetDocEntry except it only
 *         returns a result when the corresponding entry is of type
 *         SeqEntry.
 * @param   group  Group number of the searched Dicom Element 
 * @param   elem Element number of the searched Dicom Element  
 * @return When present, the corresponding SeqEntry. 
 */
SeqEntry *DocEntrySet::GetSeqEntry(uint16_t group, uint16_t elem)
{
   DocEntry *currentEntry = GetDocEntry(group, elem);
   if ( !currentEntry )
   {
      gdcmWarningMacro( "No corresponding SeqEntry " << std::hex << group <<
                        "," << elem);
      return NULL;
   }

   return dynamic_cast<SeqEntry*>(currentEntry);
}

/**
 * \brief   Accesses an existing DocEntry (i.e. a Dicom Element)
 *          through it's (group, element) and modifies it's content with
 *          the given value.
 * @param   content new value (string) to substitute with
 * @param   group  group number of the Dicom Element to modify
 * @param   elem element number of the Dicom Element to modify
 */
bool DocEntrySet::SetValEntry(std::string const &content, 
                              uint16_t group, uint16_t elem) 
{
   ValEntry *entry = GetValEntry(group, elem);
   if (!entry )
   {
      gdcmWarningMacro( "No corresponding ValEntry " << std::hex << group <<
                         "," << elem << " element (try promotion first).");
      return false;
   }
   return SetValEntry(content,entry);
}

/**
 * \brief   Accesses an existing DocEntry (i.e. a Dicom Element)
 *          through it's (group, element) and modifies it's content with
 *          the given value.
 * @param   content new value (void*  -> uint8_t*) to substitute with
 * @param   lgth new value length
 * @param   group  group number of the Dicom Element to modify
 * @param   elem element number of the Dicom Element to modify
 */
bool DocEntrySet::SetBinEntry(uint8_t *content, int lgth, 
                              uint16_t group, uint16_t elem) 
{
   BinEntry *entry = GetBinEntry(group, elem);
   if (!entry )
   {
      gdcmWarningMacro( "No corresponding ValEntry " << std::hex << group <<
                        "," << elem << " element (try promotion first).");
      return false;
   }

   return SetBinEntry(content,lgth,entry);
} 

/**
 * \brief   Accesses an existing DocEntry (i.e. a Dicom Element)
 *          and modifies it's content with the given value.
 * @param  content new value (string) to substitute with
 * @param  entry Entry to be modified
 */
bool DocEntrySet::SetValEntry(std::string const &content, ValEntry *entry)
{
   if(entry)
   {
      entry->SetValue(content);
      return true;
   }
   return false;
}

/**
 * \brief   Accesses an existing BinEntry (i.e. a Dicom Element)
 *          and modifies it's content with the given value.
 * @param   content new value (void*  -> uint8_t*) to substitute with
 * @param  entry Entry to be modified 
 * @param  lgth new value length
 */
bool DocEntrySet::SetBinEntry(uint8_t *content, int lgth, BinEntry *entry)
{
   if(entry)
   {
      entry->SetBinArea(content);  
      entry->SetLength(lgth);
      entry->SetValue(GDCM_BINLOADED);
      return true;
   }
   return false;
}

/**
 * \brief   Modifies the value of a given Doc Entry (Dicom Element)
 *          when it exists. Create it with the given value when unexistant.
 * @param   value (string) Value to be set
 * @param   group   Group number of the Entry 
 * @param   elem  Element number of the Entry
 * @param   vr  V(alue) R(epresentation) of the Entry -if private Entry-
 * \return  pointer to the modified/created Header Entry (NULL when creation
 *          failed).
 */ 
ValEntry *DocEntrySet::InsertValEntry(std::string const &value, 
                                      uint16_t group, uint16_t elem,
                                      TagName const &vr )
{
   ValEntry *valEntry = 0;
   DocEntry *currentEntry = GetDocEntry( group, elem );
   
   if (currentEntry)
   {
      valEntry = dynamic_cast<ValEntry *>(currentEntry);

      // Verify the VR
      if( valEntry )
         if( valEntry->GetVR()!=vr )
            valEntry = NULL;

      // if currentEntry doesn't correspond to the requested valEntry
      if( !valEntry)
      {
         if( !RemoveEntry(currentEntry) )
         {
            gdcmWarningMacro( "Removal of previous DocEntry failed.");

            return NULL;
         }
      }
   }

   // Create a new valEntry if necessary
   if( !valEntry )
   {
      valEntry = NewValEntry( group, elem, vr );

      if ( !AddEntry(valEntry) )
      {
         gdcmWarningMacro("AddEntry failed although this is a creation.");

         delete valEntry;
         return NULL;
      }
   }

   // Set the binEntry value
   SetValEntry(value, valEntry); // The std::string value
   return valEntry;
}

/**
 * \brief   Modifies the value of a given Header Entry (Dicom Element)
 *          when it exists. Create it with the given value when unexistant.
 *          A copy of the binArea is made to be kept in the Document.
 * @param   binArea (binary) value to be set
 * @param   lgth length of the Bin Area we want to set
 * @param   group   Group number of the Entry 
 * @param   elem  Element number of the Entry
 * @param   vr  V(alue) R(epresentation) of the Entry -if private Entry-
 * \return  pointer to the modified/created Header Entry (NULL when creation
 *          failed).
 */
BinEntry *DocEntrySet::InsertBinEntry(uint8_t *binArea, int lgth, 
                                      uint16_t group, uint16_t elem,
                                      TagName const &vr )
{
   BinEntry *binEntry = 0;
   DocEntry *currentEntry = GetDocEntry( group, elem );

   // Verify the currentEntry
   if (currentEntry)
   {
      binEntry = dynamic_cast<BinEntry *>(currentEntry);

      // Verify the VR
      if( binEntry )
         if( binEntry->GetVR()!=vr )
            binEntry = NULL;

      // if currentEntry doesn't correspond to the requested valEntry
      if( !binEntry)
      {
         if( !RemoveEntry(currentEntry) )
         {
            gdcmWarningMacro( "Removal of previous DocEntry failed.");

            return NULL;
         }
      }
   }

   // Create a new binEntry if necessary
   if( !binEntry)
   {
      binEntry = NewBinEntry(group, elem, vr);

      if ( !AddEntry(binEntry) )
      {
         gdcmWarningMacro( "AddEntry failed allthough this is a creation.");

         delete binEntry;
         return NULL;
      }
   }

   // Set the binEntry value
   uint8_t *tmpArea;
   if( lgth>0 && binArea )
   {
      tmpArea = new uint8_t[lgth];
      memcpy(tmpArea,binArea,lgth);
   }
   else
   {
      tmpArea = 0;
   }
   if( !SetBinEntry(tmpArea,lgth,binEntry) )
   {
      if( tmpArea )
      {
         delete[] tmpArea;
      }
   }

   return binEntry;
}  

/**
 * \brief   Modifies the value of a given Doc Entry (Dicom Element)
 *          when it exists. Creates it when unexistant.
 * @param   group   Group number of the Entry 
 * @param   elem  Element number of the Entry
 * \return  pointer to the modified/created SeqEntry (NULL when creation
 *          failed).
 */
SeqEntry *DocEntrySet::InsertSeqEntry(uint16_t group, uint16_t elem)
{
   SeqEntry *seqEntry = 0;
   DocEntry *currentEntry = GetDocEntry( group, elem );

   // Verify the currentEntry
   if( currentEntry )
   {
      seqEntry = dynamic_cast<SeqEntry *>(currentEntry);

      // Verify the VR
      if( seqEntry )
         seqEntry = NULL;

      // if currentEntry doesn't correspond to the requested seqEntry
      if( !seqEntry )
      {
         if (!RemoveEntry(currentEntry))
         {
            gdcmWarningMacro( "Removal of previous DocEntry failed.");

            return NULL;
         }
      }
   }
   // Create a new seqEntry if necessary
   if( !seqEntry )
   {
      seqEntry = NewSeqEntry(group, elem);

      if( !AddEntry(seqEntry) )
      {
         gdcmWarningMacro( "AddEntry failed allthough this is a creation.");

         delete seqEntry;
         return NULL;
      }
   }

   // TODO : Find a trick to insert a SequenceDelimitationItem 
   //       in the SeqEntry, at the end.
   return seqEntry;
} 


 
/**
 * \brief   Checks if a given Dicom Element exists within the H table
 * @param   group   Group number of the searched Dicom Element 
 * @param   elem  Element number of the searched Dicom Element 
 * @return true is found
 */
bool DocEntrySet::CheckIfEntryExist(uint16_t group, uint16_t elem )
{
   return GetDocEntry(group,elem)!=NULL;
}

/**
 * \brief   Build a new Val Entry from all the low level arguments. 
 *          Check for existence of dictionary entry, and build
 *          a default one when absent.
 * @param   group group   number of the new Entry
 * @param   elem  element number of the new Entry
 * @param   vr     VR of the new Entry 
 */
ValEntry *DocEntrySet::NewValEntry(uint16_t group,uint16_t elem,
                                   TagName const &vr) 
{
   DictEntry *dictEntry = GetDictEntry(group, elem, vr);
   gdcmAssertMacro(dictEntry);

   ValEntry *newEntry = new ValEntry(dictEntry);
   if (!newEntry) 
   {
      gdcmWarningMacro( "Failed to allocate ValEntry");
      return 0;
   }
   return newEntry;
}


/**
 * \brief   Build a new Bin Entry from all the low level arguments. 
 *          Check for existence of dictionary entry, and build
 *          a default one when absent.
 * @param   group group   number of the new Entry
 * @param   elem  element number of the new Entry
 * @param   vr     VR of the new Entry 
 */
BinEntry *DocEntrySet::NewBinEntry(uint16_t group, uint16_t elem,
                                   TagName const &vr) 
{
   DictEntry *dictEntry = GetDictEntry(group, elem, vr);
   gdcmAssertMacro(dictEntry);

   BinEntry *newEntry = new BinEntry(dictEntry);
   if (!newEntry) 
   {
      gdcmWarningMacro( "Failed to allocate BinEntry");
      return 0;
   }
   return newEntry;
}

/**
 * \brief   Build a new Seq Entry from all the low level arguments. 
 *          Check for existence of dictionary entry, and build
 *          a default one when absent.
 * @param   group group   number of the new Entry
 * @param   elem  element number of the new Entry
 */
SeqEntry* DocEntrySet::NewSeqEntry(uint16_t group, uint16_t elem) 
{
   DictEntry *dictEntry = GetDictEntry(group, elem, "SQ");
   gdcmAssertMacro(dictEntry);

   SeqEntry *newEntry = new SeqEntry( dictEntry );
   if (!newEntry)
   {
      gdcmWarningMacro( "Failed to allocate SeqEntry");
      return 0;
   }
   return newEntry;
}

/**
 * \brief   Request a new virtual dict entry to the dict set
 * @param   group group  number of the underlying DictEntry
 * @param   elem  element number of the underlying DictEntry
 * @param   vr    VR (Value Representation) of the underlying DictEntry
 * @param   vm    VM (Value Multiplicity)   of the underlying DictEntry
 * @param   name   english name
 */
DictEntry* DocEntrySet::NewVirtualDictEntry( uint16_t group, uint16_t elem,
                                             TagName const &vr,
                                             TagName const &vm,
                                             TagName const &name )
{
   return Global::GetDicts()->NewVirtualDictEntry(group,elem,vr,vm,name);
}

//-----------------------------------------------------------------------------
// Protected
/**
 * \brief   Searches [both] the public [and the shadow dictionary (when they
 *          exist)] for the presence of the DictEntry with given
 *          group and element. The public dictionary has precedence on the
 *          shadow one.
 * @param   group  group number of the searched DictEntry
 * @param   elem element number of the searched DictEntry
 * @return  Corresponding DictEntry when it exists, NULL otherwise.
 */
DictEntry *DocEntrySet::GetDictEntry(uint16_t group,uint16_t elem) 
{
   DictEntry *found = 0;
   Dict *pubDict = Global::GetDicts()->GetDefaultPubDict();
   if (!pubDict) 
   {
      gdcmWarningMacro( "We SHOULD have a default dictionary");
   }
   else
   {
      found = pubDict->GetEntry(group, elem);  
   }
   return found;
}

/**
 * \brief   Searches [both] the public [and the shadow dictionary (when they
 *          exist)] for the presence of the DictEntry with given
 *          group and element, and create a new virtual DictEntry if necessary
 * @param   group  group number of the searched DictEntry
 * @param   elem element number of the searched DictEntry
 * @param   vr Value Representation to use, if necessary 
 * @return  Corresponding DictEntry when it exists, NULL otherwise.
 */
DictEntry *DocEntrySet::GetDictEntry(uint16_t group, uint16_t elem,
                                     TagName const &vr)
{
   DictEntry *dictEntry = GetDictEntry(group,elem);
   DictEntry *goodEntry = dictEntry;
   std::string goodVR = vr;

   if (elem == 0x0000) goodVR="UL";

   if ( goodEntry )
   {
      if ( goodVR != goodEntry->GetVR()
        && goodVR != GDCM_UNKNOWN )
      {
         goodEntry = NULL;
      }
   }

   // Create a new virtual DictEntry if necessary
   if (!goodEntry)
   {
      if (dictEntry)
      {
         goodEntry = NewVirtualDictEntry(group, elem, goodVR, "FIXME", 
                                         dictEntry->GetName() );
      }
      else
      {
         goodEntry = NewVirtualDictEntry(group, elem, goodVR);
      }
   }
   return goodEntry;
}

//-----------------------------------------------------------------------------
// Private

//-----------------------------------------------------------------------------
// Print

//-----------------------------------------------------------------------------
} // end namespace gdcm
