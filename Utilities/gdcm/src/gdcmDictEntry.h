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

#include "gdcmCommon.h"

namespace gdcm 
{

//-----------------------------------------------------------------------------
/*
 * \defgroup DictEntry
 * \brief
 * the DictEntry in an element contained by the Dict.
 * It contains :
 *  - the key referenced by the DICOM norm or the constructor (for private keys)
 *  - the corresponding name in english (it's equivalent to a label)
 *  - the owner group
 *  - etc.
 */
class GDCM_EXPORT DictEntry 
{
public:
   DictEntry(uint16_t group, 
             uint16_t element,
             TagName const & vr     = "Unknown",
             TagName const & fourth = "Unknown",
             TagName const & name   = "Unknown");

   static TagKey TranslateToKey(uint16_t group, uint16_t element);

   void SetVR(TagName const & vr);

   /// \brief tells if the V(alue) R(epresentation) is known (?!)
   /// @return 
   bool IsVRUnknown() { return VR == "??"; }

   /// \brief  Returns the Dicom Group Number of the current DictEntry
   /// @return the Dicom Group Number
   uint16_t GetGroup() { return Group; }
  
   /// \brief  Returns the Dicom Element Number of the current DictEntry
   /// @return the Dicom Element Number
   uint16_t GetElement() { return Element; }
 
   /// \brief  Returns the Dicom Value Representation of the current
   ///         DictEntry
   /// @return the Dicom Value Representation
   const TagName & GetVR() const { return VR; }
 
   /// \brief   sets the key of the current DictEntry
   /// @param k New key to be set.
   void SetKey(TagName const & k)  { Key = k; }
 
   /// \brief   returns the Fourth field of the current DictEntry
   /// \warning NOT part of the Dicom Standard.
   ///          May be REMOVED an any time. NEVER use it.
   /// @return  The Fourth field
   const TagName & GetFourth() const { return Fourth; } 

   /// \brief  Returns the Dicom Name of the current DictEntry
   ///         e.g. "Patient Name" for Dicom Tag (0x0010, 0x0010) 
   /// @return the Dicom Name
   const TagName & GetName() const { return Name; } 
 
   /// \brief  Gets the key of the current DictEntry
   /// @return the key.
   const TagName & GetKey() const { return Key; }

private:
   /// \todo FIXME 
   ///        where are the group and element used except from building up
   ///        a TagKey. If the answer is nowhere then there is no need
   ///        to store the group and element independently.
   ///
   ///        --> EVERYWHERE ! The alternate question would be :
   ///                         What's TagKey used for ?
   
   /// DicomGroup number
   uint16_t Group;   // e.g. 0x0010

   /// DicomElement number
   uint16_t Element; // e.g. 0x0103

   /// \brief Value Representation i.e. some clue about the nature
   ///        of the data represented e.g. "FD" short for
   ///        "Floating Point Double" (see \ref VR)
   TagName VR;

   /**
    * \brief AVOID using the following fourth field at all costs.
    * 
    *  They are at least two good reasons for NOT using fourth:
    *  - the main reason is that it is NOT part of the 'official'
    *    Dicom Dictionnary.
    *  - a second reason is that it is not defined for all the groups.
    *  .
    *  Still it provides some semantics as group name abbreviation that
    *  can prove of some help when organizing things in an interface.
    *  For the time being we keep it in gdcm but it migth be removed in
    *  future releases it proves to be source of confusion.
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
   TagName Fourth; 

   /// e.g. "Patient's Name"                    
   TagName Name;      

   /// Redundant with (group, element) but we add it for efficiency purpose.
   TagKey  Key;
};
} // end namespace gdcm
//-----------------------------------------------------------------------------
#endif
