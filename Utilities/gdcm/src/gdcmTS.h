/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmTS.h
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

#ifndef GDCMTS_H
#define GDCMTS_H

#include "gdcmCommon.h"
#include <map>
#include <string>
#include <iostream>

namespace gdcm 
{
//-----------------------------------------------------------------------------
typedef std::string TSKey;
typedef std::string TSAtr;
typedef std::map<TSKey, TSAtr> TSHT;    // Transfert Syntax Hash Table

//-----------------------------------------------------------------------------
/*
 * Container for dicom Transfert Syntax Hash Table
 * \note   This is a singleton
 */
class GDCM_EXPORT TS
{
public:
   TS();
   ~TS();

   void Print(std::ostream &os = std::cout);

   int Count(TSKey const & key);
   TSAtr const & GetValue(TSKey const & key);

private:
   TSHT TsMap;
};
} // end namespace gdcm

//-----------------------------------------------------------------------------
#endif
