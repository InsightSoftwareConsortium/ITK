/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmDocEntry.h
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

#ifndef GDCMDOCENTRY_H
#define GDCMDOCENTRY_H

#include "gdcmDictEntry.h"
#include <iostream>

class Header;
class ValEntry;
class BinEntry;
class SeqEntry;

namespace gdcm 
{
//-----------------------------------------------------------------------------
/**
 * \brief   The dicom header of a Dicom file contains a set of such entries
 *          (when successfuly parsed against a given Dicom dictionary)
 */
class GDCM_EXPORT DocEntry
{
public:
   DocEntry(DictEntry*);
   virtual ~DocEntry() {};

   /// Returns the Dicom Group number of the current Dicom Header Entry
   uint16_t      GetGroup()     { return DicomDict->GetGroup();  };

   /// Returns the Dicom Element number of the current Dicom Header Entry
   uint16_t      GetElement()   { return DicomDict->GetElement();};

   /// Returns the 'key' of the current Dicom Header Entry
   void  SetKey( TagKey const & key ) { Key = key; }

   /// Returns the 'key' of the current Dicom Header Entry
   std::string const & GetKey() const { return Key; }

   /// \brief Returns the 'Name' '(e.g. "Patient's Name") found in the Dicom
   /// Dictionnary of the current Dicom Header Entry
   std::string const & GetName() const { return DicomDict->GetName(); };

   /// \brief Returns the 'Value Representation' (e.g. "PN" : Person Name,
   /// "SL" : Signed Long), found in the Dicom Header or in the Dicom
   /// Dictionnary, of the current Dicom Header Entry
   std::string const & GetVR() const { return DicomDict->GetVR(); };

   /// \brief Returns offset (since the beginning of the file, including
   /// the File Preamble, if any) of the value of the current Dicom HeaderEntry
   /// \warning offset of the *value*, not of the Dicom Header Entry
   size_t GetOffset() { return Offset; };

   /// \brief Returns the actual value length of the current Dicom Header Entry
   /// \warning this value is not *always* the one stored in the Dicom Header
   ///          in case of well knowned bugs
   uint32_t GetLength() { return UsableLength; };
    
   /// \brief Returns the 'read length' of the current Dicom Header Entry
   /// \warning this value is the one stored in the Dicom Header but not
   ///          mandatoryly the one thats's used (in case on SQ, or delimiters,
   ///          the usable length is set to zero)
   uint32_t GetReadLength() { return ReadLength; };

   /// Sets the 'Value Representation' of the current Dicom Header Entry
   void SetVR( TagName const & v) { DicomDict->SetVR(v); };    

   /// \brief Sets both 'Read Length' and 'Usable Length' of the current
   /// Dicom Header Entry
   void SetLength(uint32_t l) { ReadLength = UsableLength = l; };
      
   // The following 3 members, for internal use only ! 
   
   /// \brief Sets only 'Read Length' (*not* 'Usable Length') of the current
   /// Dicom Header Entry
   void SetReadLength(uint32_t l) { ReadLength = l; };

   /// \brief Sets only 'Usable Length' (*not* 'Read Length') of the current
   /// Dicom Header Entry
   void SetUsableLength(uint32_t l) { UsableLength = l; }; 
   
   /// \brief   Sets the offset of the Dicom Element
   /// \warning use with caution !
   /// @param   of offset to be set
   void SetOffset(size_t of) { Offset = of; };

   /// Sets to TRUE the ImplicitVr flag of the current Dicom Element
   void SetImplicitVR() { ImplicitVR = true; };
 
   /// \brief Tells us if the current Dicom Element was checked as ImplicitVr
   /// @return true if the current Dicom Element was checked as ImplicitVr
   bool IsImplicitVR() { return ImplicitVR; };

   /// \brief Tells us if the VR of the current Dicom Element is Unknown
   /// @return true if the VR is unknown
   bool IsVRUnknown() { return DicomDict->IsVRUnknown(); };

   /// \brief   Sets the DicEntry of the current Dicom Element
   /// @param   newEntry pointer to the DictEntry
   void SetDictEntry(DictEntry *newEntry) { DicomDict = newEntry; };

   /// \brief  Gets the DicEntry of the current Dicom Element
   /// @return The DicEntry of the current Dicom Element
   DictEntry * GetDictEntry() { return DicomDict; }; 

   /// \brief Sets the print level for the Dicom Header Elements
   /// \note 0 for Light Print; 1 for 'medium' Print, 2 for Heavy
   void SetPrintLevel(int level) { PrintLevel = level; };

   /// \brief Gets the print level for the Dicom Header Elements
   int GetPrintLevel() { return PrintLevel; };
   
   virtual void Print (std::ostream & os = std::cout); 
   virtual void Write(std::ofstream *fp, FileType filetype);
   
   uint32_t GetFullLength();
   
   void Copy(DocEntry *doc);

   bool IsItemDelimitor();
   bool IsSequenceDelimitor();   

private:
   // FIXME: In fact we should be more specific and use :
   // friend DocEntry * Header::ReadNextElement(void);
   friend class Header;    

protected:
// Variables

   /// \brief pointer to the underlying Dicom dictionary element
   DictEntry *DicomDict;
   
   /// \brief Updated from ReadLength, by FixFoungLentgh() for fixing a bug
   /// in the header or helping the parser going on    
   uint32_t UsableLength; 
  
   /// \brief Length actually read on disk (before FixFoundLength). ReadLength
   /// will be updated only when FixFoundLength actually fixes a bug in the
   /// header, not when it performs a trick to help the Parser going on.
   uint32_t ReadLength;

   /// \brief Even when reading explicit vr files, some elements happen to
   /// be implicit. Flag them here since we can't use the entry->vr without
   /// breaking the underlying dictionary.
   bool ImplicitVR;

   /// Offset from the begining of file for direct user access
   size_t Offset; 

   /// How many details are to be printed (value : 0,1,2)      
   int PrintLevel;

   /// \brief Generalized key (i.e. a BaseTagKey prepending a TagKey)
   ///        of this DocEntry
   TagKey Key;
};
} // end namespace gdcm
//-----------------------------------------------------------------------------
#endif
