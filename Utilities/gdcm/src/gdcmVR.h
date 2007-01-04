/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmVR.h
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

#ifndef GDCMVR_H
#define GDCMVR_H

#include "gdcmCommon.h"
#include <map>
#include <string>
#include <iostream>

namespace gdcm 
{

//-----------------------------------------------------------------------------
typedef std::string VRKey;
typedef std::string VRAtr;
/// Value Representation Hash Table
typedef std::map<VRKey, VRAtr> VRHT;

//-----------------------------------------------------------------------------
/**
 * \brief Container for dicom Value Representation Hash Table
 * \note   This is a singleton
 */
class GDCM_EXPORT VR 
{
public:
   VR();
   ~VR();

   void Print(std::ostream &os = std::cout,
              std::string const &s = "");

   /// \brief   Get the count for an element
   int Count(VRKey const &key) { return static_cast<int>( vr.count(key) ); };

   bool IsVROfBinaryRepresentable(VRKey const &tested);
   bool IsVROfStringRepresentable(VRKey const &tested);

   /// \brief   Simple predicate that checks whether the given argument
   ///          corresponds to the Value Representation of a \ref SeqEntry
   bool IsVROfSequence(VRKey const &tested) { return tested == "SQ"; }

   bool IsValidVR(VRKey const &key) { return vr.find(key) != vr.end(); }

private:
   VRHT vr;
};
} // end namespace gdcm

//-----------------------------------------------------------------------------
#endif
