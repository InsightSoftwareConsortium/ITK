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
#include <itksys/ios/sstream>

namespace gdcm 
{
void FillDefaultVRDict(VRHT & vr);
//-----------------------------------------------------------------------------
/**
 * \brief Constructor
 */
VR::VR() 
{
   std::string filename = DictSet::BuildDictPath() + DICT_VR;
   std::ifstream from(filename.c_str());
   if(!from)
   {
      dbg.Verbose(2, "VR::VR: can't open dictionary", filename.c_str());
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
         key = buff;
         from >> std::ws;
         from.getline(buff, 1024, ';');
         name = buff;
   
         from >> std::ws;
         from.getline(buff, 1024, '\n');
   
         if(key != "")
         {
            vr[key] = name;
         }
      }
      from.close();
   }
}

//-----------------------------------------------------------------------------
/**
 * \brief Destructor
 */
VR::~VR()
{
   vr.clear();
}

//-----------------------------------------------------------------------------
// Print
/**
 * \brief   Print all 
 * @param   os The output stream to be written to.
 */
void VR::Print(std::ostream &os) 
{
   itksys_ios::ostringstream s;

   for (VRHT::iterator it = vr.begin(); it != vr.end(); ++it)
   {
      s << "VR : " << it->first << " = " << it->second << std::endl;
   }
   os << s.str();
}

//-----------------------------------------------------------------------------
// Public
/**
 * \brief   Get the count for an element
 * @param   key key to count
 */
int VR::Count(VRKey const & key) 
{
   return vr.count(key);
}

//-----------------------------------------------------------------------------
/**
 * \brief   Simple predicate that checks wether the given argument
 *          corresponds to the Value Representation of a \ref BinEntry .
 *          This predicate is the negation of
 *          \ref VR::IsVROfGdcmStringRepresentable .
 * @param   tested value representation to check for.
 */
bool VR::IsVROfGdcmBinaryRepresentable(VRKey const & tested)
{
   //std::cout << "VR::IsVROfGdcmBinaryRepresentable===================="
   //   << tested << std::endl;

   if ( tested == "unkn")
      return true;

   if ( ! Count(tested) )
   {
      dbg.Verbose(0, "VR::IsVROfGdcmBinaryRepresentable: tested not a VR!");
      return false;
   }

   if ( IsVROfGdcmStringRepresentable(tested) )
   {
      dbg.Verbose(0, "VR::IsVROfGdcmBinaryRepresentable: binary VR !");
      return false;
   }

   return true;
}

//-----------------------------------------------------------------------------
/**
 * \brief   Simple predicate that checks wether the given argument
 *          corresponds to the Value Representation of a \ref ValEntry
 *          but NOT a \ref BinEntry.
 * @param   tested value representation to check for.
 */
bool VR::IsVROfGdcmStringRepresentable(VRKey const & tested)
{

   if ( ! Count(tested) )
   {
      dbg.Verbose(0, "VR::IsVROfGdcmStringRepresentable: tested not a VR!");
      return false;
   }

   if (tested == "AE" || tested == "AS" || tested == "DA" || tested == "PN" ||
       tested == "UI" || tested == "TM" || tested == "SH" || tested == "LO" ||
       tested == "CS" || tested == "IS" || tested == "LO" || tested == "LT" ||
       tested == "SH" || tested == "ST" || tested == "DS" || tested == "SL" ||
       tested == "SS" || tested == "UL" || tested == "US" || tested == "UN")
   {
      return true;
   }
   return false;
}

//-----------------------------------------------------------------------------
// Protected

//-----------------------------------------------------------------------------
// Private

//-----------------------------------------------------------------------------

} // end namespace gdcm
