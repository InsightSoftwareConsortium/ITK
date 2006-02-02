/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmDicomDirVisit.h
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

#ifndef GDCMDICOMDIRVISIT_H
#define GDCMDICOMDIRVISIT_H

#include "gdcmDicomDirObject.h"

namespace gdcm 
{

/**
 * \brief   describes a VISIT  within a within a STUDY
 * (DicomDirStudy) of a given DICOMDIR (DicomDir)
 */
class GDCM_EXPORT DicomDirVisit : public DicomDirObject 
{
public:
   DicomDirVisit(bool empty=false); 
   ~DicomDirVisit();

   void Print( std::ostream &os = std::cout, std::string const &indent = "" );
  // void WriteContent( std::ofstream *fp, FileType t );

};
} // end namespace gdcm
//-----------------------------------------------------------------------------
#endif
