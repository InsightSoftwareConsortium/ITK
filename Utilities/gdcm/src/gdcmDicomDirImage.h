/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmDicomDirImage.h
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

#ifndef GDCMDICOMIMAGE_H
#define GDCMDICOMIMAGE_H

#include "gdcmDicomDirObject.h"

namespace gdcm 
{
//-----------------------------------------------------------------------------
/**
 * \brief   describes an IMAGE within a SERIE
 * (DicomDirSerie) of a given DICOMDIR (DicomDir)
 */
 class GDCM_EXPORT DicomDirImage : public DicomDirObject
{
public:
   DicomDirImage(bool empty=false); 
   ~DicomDirImage();

   void Print(std::ostream &os = std::cout, std::string const &indent = "" );
};
} // end namespace gdcm
//-----------------------------------------------------------------------------
#endif
