/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmRLEFramesInfo.cxx
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

#include "gdcmRLEFramesInfo.h"

namespace gdcm 
{

RLEFramesInfo::~RLEFramesInfo()
{
   for(RLEFrameList::iterator it = Frames.begin(); it != Frames.end(); ++it)
   {
      delete (*it);
   }
   Frames.clear();
}

/**
 * \brief        Print self.
 * @param indent Indentation string to be prepended during printing.
 * @param os     Stream to print to.
 */
void RLEFramesInfo::Print( std::string indent, std::ostream &os )
{
   os << indent
      << "----------------- RLE frames --------------------------------"
      << std::endl;
   os << indent
      << "Total number of Frames : " << Frames.size()
      << std::endl;
   int frameNumber = 0;
   for(RLEFrameList::iterator it = Frames.begin(); it != Frames.end(); ++it)
   {
      os << indent
         << "   frame number :" << frameNumber++
         << std::endl;
      (*it)->Print( indent + "   ", os );
   }
}

} // end namespace gdcm
