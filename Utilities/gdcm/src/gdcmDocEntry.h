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

#include "gdcmBase.h"
#include "gdcmDictEntry.h"

#include <iostream>
#include <fstream>

namespace gdcm 
{
class File;
class ValEntry;
class BinEntry;
class SeqEntry;

//-----------------------------------------------------------------------------
/**
 * \brief   The dicom header of a Dicom file contains a set of such entries
 *          (when successfuly parsed against a given Dicom dictionary)
 */
class GDCM_EXPORT DocEntry : public Base
{
public:
   DocEntry(DictEntry*);
   /// \brief Canonical Destructor
   virtual ~DocEntry() {}

   virtual void Print (std::ostream &os = std::cout, std::string const &indent = ""); 
   virtual void WriteContent(std::ofstream *fp, FileType filetype);

   /// \brief  Gets the DicEntry of the current Dicom entry
   /// @return The DicEntry of the current Dicom entry
   DictEntry * GetDictEntry() { return DicomDict; } 

   /// Returns the Dicom Group number of the current Dicom entry
   const uint16_t GetGroup() const { return DicomDict->GetGroup();  }

   /// Returns the Dicom Element number of the current Dicom entry
   const uint16_t GetElement() const { return DicomDict->GetElement();}

   /// Set the 'key' of the current Dicom entry
   void  SetKey( TagKey const &key ) { Key = key; }

   /// Returns the 'key' of the current Dicom entry
   TagKey const &GetKey() const { return Key; }

   /// \brief Returns the 'Name' '(e.g. "Patient's Name") found in the Dicom
   /// Dictionnary of the current Dicom Header Entry
   std::string const &GetName() const { return DicomDict->GetName(); }

   /// \brief Returns the 'Value Representation' (e.g. "PN" : Person Name,
   /// "SL" : Signed Long), found in the Dicom header or in the Dicom
   /// Dictionnary, of the current Dicom entry
   std::string const &GetVR() const { return DicomDict->GetVR(); }

   /// \brief Returns the 'Value Multiplicity' (e.g. "1", "6", "1-n", "3-n"),
   /// found in the Dicom entry or in the Dicom Dictionnary
   /// of the current Dicom entry
   std::string const &GetVM() const { return DicomDict->GetVM(); }

   /// Sets the 'Value Multiplicity' of the current Dicom entry
   void SetVM( TagName const &v) { DicomDict->SetVM(v); } 

   /// \brief Returns offset (since the beginning of the file, including
   /// the File Preamble, if any) of the value of the current Dicom entry
   /// \warning offset of the *value*, not of the Dicom entry
   const size_t &GetOffset() const { return Offset; }

   /// \brief Sets only 'Read Length' (*not* 'Usable Length') of the current
   /// Dicom entry
   void SetReadLength(uint32_t l) { ReadLength = l; }
   /// \brief Returns the 'read length' of the current Dicom entry
   /// \warning this value is the one stored in the Dicom header but not
   ///          mandatoryly the one thats's used (in case on SQ, or delimiters,
   ///          the usable length is set to zero)
   const uint32_t &GetReadLength() const { return ReadLength; }

   /// \brief Sets both 'Read Length' and 'Usable Length' of the current
   /// Dicom entry
   virtual void SetLength(uint32_t l) { Length = l; }
   /// \brief Returns the actual value length of the current Dicom entry
   /// \warning this value is not *always* the one stored in the Dicom header
   ///          in case of well known bugs
   const uint32_t &GetLength() const { return Length; }

   uint32_t GetFullLength();
   virtual uint32_t ComputeFullLength() = 0;

// The following 3 members, for internal use only ! 
   /// \brief   Sets the offset of the Dicom entry
   /// \warning use with caution !
   /// @param   of offset to be set
   void SetOffset(size_t of) { Offset = of; }

   /// Sets to TRUE the ImplicitVr flag of the current Dicom entry
   void SetImplicitVR() { ImplicitVR = true; }
 
   /// \brief Tells us if the current Dicom entry was checked as ImplicitVr
   /// @return true if the current Dicom entry was checked as ImplicitVr
   bool IsImplicitVR() const { return ImplicitVR; }

   /// \brief Tells us if the VR of the current Dicom entry is Unknown
   /// @return true if the VR is unknown
   bool IsVRUnknown() const { return DicomDict->IsVRUnknown(); }

   /// \brief Tells us if the VM of the current Dicom entry is Unknown
   /// @return true if the VM is unknown
   bool IsVMUnknown() const { return DicomDict->IsVMUnknown(); }

   bool IsItemDelimitor();
   bool IsItemStarter();
   bool IsSequenceDelimitor();   

   virtual void Copy(DocEntry *e);

protected:
   /// \brief pointer to the underlying Dicom dictionary element
   DictEntry *DicomDict;
   
   /// \brief Correspond to the real length of the data
   /// This length might always be even
   uint32_t Length; 
  
   /// \brief Length to read in the file to obtain data
   uint32_t ReadLength;

   /// \brief Even when reading explicit vr files, some elements happen to
   /// be implicit. Flag them here since we can't use the entry->vr without
   /// breaking the underlying dictionary.
   bool ImplicitVR;

   /// Offset from the beginning of file for direct user access
   size_t Offset; 

   /// \brief Generalized key of this DocEntry (for details on
   ///        the generalized key refer to \ref TagKey documentation).
   TagKey Key;

private:
};
} // end namespace gdcm
//-----------------------------------------------------------------------------
#endif
