/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmRLEFrame.cxx
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
                                                                                
#include "gdcmRLEFrame.h"
                                                                                
namespace gdcm
{

/**
 * \brief        Print self.
 * @param indent Indentation string to be prepended during printing.
 * @param os     Stream to print to.
 */
void RLEFrame::Print( std::string indent, std::ostream &os )
{
   os << indent
      << "--- fragments"
      << std::endl;
   for ( unsigned int i = 0; i < NumberFragments; i++ )
   {
      os << indent
         << "   offset : " <<  Offset[i]
         << "   length : " <<  Length[i]
         << std::endl;
   }
}

} // end namespace gdcm

