/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmDirList.h
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

#ifndef GDCMDIRLIST_H
#define GDCMDIRLIST_H

#include "gdcmCommon.h"

#include <string>
#include <list>

namespace gdcm 
{

//-----------------------------------------------------------------------------
/**
 * \ingroup DirList
 * \brief   List containing the file headers from root directory. 
 */
// NOTE: Due to a VC6 'feature' we can not export a std::list in a dll, 
// so GDCM_EXPORT keyword was removed for this class only

class DirList: public std::list<std::string>
{
public :
   DirList(std::string const & dirName, bool recursive=false);
   virtual ~DirList();

   std::string const & GetDirName() const;

   /// Character '\' 
   static const char SEPARATOR_X;
   /// Character '/'  
   static const char SEPARATOR_WIN;
   /// depending on the O.S.
   static const std::string SEPARATOR;

private :
   int Explore(std::string const & dirName, bool recursive=false);

   /// name of the root directory to explore
   std::string name;
};
} // end namespace gdcm
//-----------------------------------------------------------------------------
#endif
