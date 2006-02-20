/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmDictGroupName.cxx
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

#include "gdcmDictGroupName.h"
#include "gdcmUtil.h"
#include "gdcmDictSet.h"
#include "gdcmDebug.h"

#include <fstream>
#include <iostream>
#include <iomanip>

namespace gdcm 
{
//-----------------------------------------------------------------------------
/// \brief auto generated function, to fill up the 'Group Name'
///        Dictionnary, if relevant file is not found on user's disk
void FillDefaultDictGroupName(DictGroupNameHT &groupName);

//-----------------------------------------------------------------------------
// Constructor / Destructor
/**
 * \brief Constructor
 */
DictGroupName::DictGroupName() 
{
   std::string filename = DictSet::BuildDictPath() + DICT_GROUP_NAME;
   std::ifstream from(filename.c_str());
   if ( !from )
   {
      gdcmWarningMacro("Can't open dictionary" << filename.c_str());
      FillDefaultDictGroupName(groupName);
   }
   else
   {
      char buff[1024];
      uint16_t key;
      TagName value;
   
      while (!from.eof()) 
      {
         from >> std::ws;
         from >> std::hex;
         from >> key;
         from >> std::ws;
         from.getline(buff, 1024, '"');
         from.getline(buff, 1024, '"');
         value = buff;
         if ( !from.eof() )
            groupName[key] = value;

         from.getline(buff, 1024, '\n');
      }
      from.close();
   }
}

/**
 * \brief Destructor
 */
DictGroupName::~DictGroupName()
{
   groupName.clear();
}

//-----------------------------------------------------------------------------
// Public
/// \returns the formerly NIH defined ACR-NEMA group name
const TagName &DictGroupName::GetName(uint16_t group)
{
   DictGroupNameHT::const_iterator it = groupName.find(group);
   if ( it == groupName.end() )
   {
      return GDCM_UNFOUND;
   }
   return it->second;
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
void DictGroupName::Print(std::ostream &os,std::string const &) 
{
   itksys_ios::ostringstream s;

   for (DictGroupNameHT::iterator it = groupName.begin(); it != groupName.end(); ++it)
   {
      s << "DictGroupName : 0x" << std::hex << std::setw(4) << it->first 
        << std::dec << " = " << it->second << std::endl;
   }
   os << s.str();
}

//-----------------------------------------------------------------------------
} // end namespace gdcm
