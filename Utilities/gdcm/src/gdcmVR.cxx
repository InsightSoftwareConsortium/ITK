/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmVR.cxx
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

#include "gdcmVR.h"
#include "gdcmUtil.h"
#include "gdcmDictSet.h"
#include "gdcmDebug.h"

#include <fstream>
#include <iostream>
#include <string.h>
#include <algorithm>

namespace gdcm 
{
//-----------------------------------------------------------------------------
/// \brief auto generated function, to fill up the 'Value Representation'
///        Dictionnary, if relevant file is not found on user's disk
void FillDefaultVRDict(VRHT &vr);

//-----------------------------------------------------------------------------
// Constructor / Destructor
/**
 * \brief Constructor
 */
VR::VR() 
{
   std::string filename = DictSet::BuildDictPath() + DICT_VR;
   std::ifstream from(filename.c_str());
   if ( !from )
   {
      gdcmWarningMacro("Can't open dictionary " << filename.c_str());
      FillDefaultVRDict(vr);
   }
   else
   {
      char buff[1024];
      VRKey key;
      VRAtr name;
   
      while (!from.eof()) 
      {
         from >> std::ws;
         from.getline(buff, 1024, ' ');
         if( strcmp(buff,"") == 0)
            continue;

         key = buff;
         from >> std::ws;
         from.getline(buff, 1024, ';');
         name = buff;
   
         from >> std::ws;
         from.getline(buff, 1024, '\n');
   
         vr[key] = name;
      }
      from.close();
   }
}

/**
 * \brief Destructor
 */
VR::~VR()
{
   vr.clear();
}

//-----------------------------------------------------------------------------
// Public

/**
 * \brief   Simple predicate that checks whether the given argument
 *          corresponds to the Value Representation of a \ref BinEntry .
 * @param   tested value representation to check for.
 */
bool VR::IsVROfBinaryRepresentable(VRKey const &tested)
{
   if ( IsVROfStringRepresentable(tested) )
      return false;

   if ( IsVROfSequence(tested) )
      return false;

   return true;
}

/**
 * \brief   Simple predicate that checks whether the given argument
 *          corresponds to the Value Representation of a \ref ValEntry
 *          but NOT a \ref BinEntry.
 * @param   tested value representation to be checked.
 */
bool VR::IsVROfStringRepresentable(VRKey const &tested)
{
  // important: vrs must be specified in sorted order!!!
  static const std::string vrs[] = { "AE", "AS", "CS", "DA", "DS",
                                     "DT", "FD", "FL", "IS", "LO",
                                     "LT", "PN", "SH", "SL", "SS",
                                     "ST", "TM", "UI", "UL", "US",
                                     "UT"};
  static const int vrSize = sizeof(std::string("AE"));
  static const int numVRs = sizeof(vrs) / vrSize;
  static const std::string *end = vrs + numVRs;

  return std::binary_search(vrs, end, tested);
}

//-----------------------------------------------------------------------------
// Protected

//-----------------------------------------------------------------------------
// Private

//-----------------------------------------------------------------------------
// Print
/**
 * \brief   Print all 
 * @param   os The output stream to be written to.
 */
void VR::Print(std::ostream &os,std::string const &) 
{
   for (VRHT::iterator it = vr.begin(); it != vr.end(); ++it)
   {
      os << "VR : " << it->first << " = " << it->second << std::endl;
   }
}

//-----------------------------------------------------------------------------
} // end namespace gdcm
