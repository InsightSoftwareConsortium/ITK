/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmDictEntry.h
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

#ifndef GDCMDICTENTRY_H
#define GDCMDICTENTRY_H

#include "gdcmBase.h"

namespace gdcm 
{
//-----------------------------------------------------------------------------
/**
 * \brief
 * the DictEntry in an element contained by the Dict.
 * It contains :
 *  - the key referenced by the DICOM norm or the constructor (for private keys)
 *    i.e. the Group number
 *         the Element number
 *  - the VR (Value Representation)
 *  - the VM (Value Multiplicity)
 *  - the corresponding name in english
 */
class GDCM_EXPORT DictEntry : public Base
{
public:
   DictEntry(uint16_t group, 
             uint16_t elem,
             TagName const &vr     = GDCM_UNKNOWN,
             TagName const &vm     = GDCM_UNKNOWN,
             TagName const &name   = GDCM_UNKNOWN);

// Print
   void Print(std::ostream &os = std::cout, std::string const &indent = "");

// Content of DictEntry
   void SetVR(TagName const &vr);
   void SetVM(TagName const &vm);

   /// \brief tells if the V(alue) R(epresentation) is known (?!)
   /// @return 
   bool IsVRUnknown() { return VR == GDCM_UNKNOWN; }

   /// \brief tells if the V(alue) M(ultiplicity) is known (?!)
   /// @return 
   bool IsVMUnknown() { return VM == GDCM_UNKNOWN; }

   /// \brief  Returns the Dicom Group Number of the current DictEntry
   /// @return the Dicom Group Number
   uint16_t GetGroup() const { return Group; }
  
   /// \brief  Returns the Dicom Element Number of the current DictEntry
   /// @return the Dicom Element Number
   uint16_t GetElement() const { return Element; }
 
   /// \brief  Returns the Dicom Value Representation of the current
   ///         DictEntry
   /// @return the Dicom Value Representation
   const TagName &GetVR() const { return VR; }
 
   /// \brief   sets the key of the current DictEntry
   /// @param k New key to be set.
   void SetKey(TagKey const &k)  { Key = k; }
 
   /// \brief   returns the VM field of the current DictEntry
   /// @return  The 'Value Multiplicity' field
   const TagName &GetVM() const { return VM; } 

   /// \brief  Returns the Dicom Name of the current DictEntry
   ///         e.g. "Patient Name" for Dicom Tag (0x0010, 0x0010) 
   /// @return the Dicom Name
   const TagName &GetName() const { return Name; } 
 
   /// \brief  Gets the key of the current DictEntry
   /// @return the key.
   const TagKey &GetKey() const { return Key; }

// Key creation
   static TagKey TranslateToKey(uint16_t group, uint16_t elem);

private:
   /// \todo FIXME 
   ///        where are the group and elem used except from building up
   ///        a TagKey. If the answer is nowhere then there is no need
   ///        to store the group and elem independently.
   ///
   ///        --> EVERYWHERE ! The alternate question would be :
   ///                         What's TagKey used for ?
   
   /// DicomGroup number
   uint16_t Group;   // e.g. 0x0010

   /// DicomElement number
   uint16_t Element; // e.g. 0x0103

   /// \brief Value Representation i.e. some clue about the nature
   ///        of the data represented e.g. 
   ///        "FD" short for "Floating Point Double"(see \ref VR)
   ///        "PN" short for "Person Name"       
   TagName VR;

   /*
    *  .
    *  Formerly 'Group name abbreviations'
    *  Here is a small dictionary we encountered in "nature":
    *  - CMD      Command        
    *  - META     Meta Information 
    *  - DIR      Directory
    *  - ID       ???
    *  - PAT      Patient
    *  - ACQ      Acquisition
    *  - REL      Related
    *  - IMG      Image
    *  - SDY      Study
    *  - VIS      Visit 
    *  - WAV      Waveform
    *  - PRC      ???
    *  - DEV      Device
    *  - NMI      Nuclear Medicine
    *  - MED      ???
    *  - BFS      Basic Film Session
    *  - BFB      Basic Film Box
    *  - BIB      Basic Image Box
    *  - BAB
    *  - IOB
    *  - PJ
    *  - PRINTER
    *  - RT       Radio Therapy
    *  - DVH   
    *  - SSET
    *  - RES      Results
    *  - CRV      Curve
    *  - OLY      Overlays
    *  - PXL      Pixels
    *  - DL       Delimiters
    *  .
    *
    *  Other usefull abreviations used for Radiographic view associated with
    *  Patient Position (0018,5100):
    *  -  AP = Anterior/Posterior 
    *  -  PA = Posterior/Anterior 
    *  -  LL = Left Lateral 
    *  -  RL = Right Lateral 
    *  - RLD = Right Lateral Decubitus 
    *  - LLD = Left  Lateral Decubitus 
    *  - RLO = Right Lateral Oblique 
    *  - LLO = Left  Lateral Oblique  
    *  .
    */
   /// \brief Value Multiplicity (e.g. "1", "1-n", "6")
   TagName VM; 

   /// \brief English name of the entry (e.g. "Patient's Name")                   
   TagName Name;      

   /// Redundant with (group, elem) but we add it for efficiency purpose.
   TagKey  Key;
};
} // end namespace gdcm
//-----------------------------------------------------------------------------
#endif
