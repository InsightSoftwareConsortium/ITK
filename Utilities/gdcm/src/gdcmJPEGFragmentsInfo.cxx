/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmJPEGFragmentsInfo.cxx
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

#include "gdcmJPEGFragmentsInfo.h"

namespace gdcm 
{

/**
 * \brief Default destructor
 */
JPEGFragmentsInfo::~JPEGFragmentsInfo()
{
   for(JPEGFragmentsList::iterator it  = Fragments.begin();
                                   it != Fragments.end();
                                 ++it )
   {
      delete *it;
   }
   Fragments.clear();
}

/**
 * \brief        Print self.
 * @param indent Indentation string to be prepended during printing.
 * @param os     Stream to print to.
 */
void JPEGFragmentsInfo::Print( std::string indent, std::ostream &os )
{
   os << indent
      << "----------------- JPEG fragments --------------------------------"
      << std::endl;
   os << indent
      << "Total number of fragments : " << Fragments.size()
      << std::endl;
   int fragmentNumber = 0;
   for(JPEGFragmentsList::iterator it  = Fragments.begin();
                                   it != Fragments.end();
                                 ++it)
   {
      os << indent
         << "   fragment number :" << fragmentNumber++;
      (*it)->Print( indent + "   ", os );
      os << std::endl;
   }
}


} // end namespace gdcm
