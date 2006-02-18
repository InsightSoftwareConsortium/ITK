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
#include <vector>
#include <iostream>

namespace gdcm 
{

typedef std::vector<std::string> DirListType;

//-----------------------------------------------------------------------------

// NOTE: Due to a M$VC6 'feature' we cannot export a std::list in a dll, 
// so GDCM_EXPORT keyword was removed for this class only

/**
 * \brief   List containing the file headers of all the 'gdcm readable' files
 *          found by exploring (possibly recursively) a root directory. 
 */
class GDCM_EXPORT DirList
{
public :
   DirList(std::string const &dirName, bool recursive=false);
   ~DirList();

   void Print(std::ostream &os = std::cout, std::string const &indent = "" );

   /// Return the name of the directory
   std::string const &GetDirName() const { return DirName; }

   /// Return the file names
   DirListType const &GetFilenames() const { return Filenames; }

   static bool IsDirectory(std::string const &dirName);

private :
   int Explore(std::string const &dirName, bool recursive=false);

   /// List of file names
   DirListType Filenames;
   /// name of the root directory to explore
   std::string DirName;
};
} // end namespace gdcm
//-----------------------------------------------------------------------------
#endif
