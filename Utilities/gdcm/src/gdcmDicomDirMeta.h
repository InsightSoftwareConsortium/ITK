/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmDicomDirMeta.h
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

#ifndef GDCMDICOMDIRMETA_H
#define GDCMDICOMDIRMETA_H

#include "gdcmDicomDirObject.h"

namespace gdcm 
{

//-----------------------------------------------------------------------------
/**
 * \brief   Meta Elements (group 0002) of a DicomDir
 */
class GDCM_EXPORT DicomDirMeta : public DicomDirObject 
{
public:
   DicomDirMeta(bool empty=false); 
   ~DicomDirMeta();

   virtual void Print(std::ostream &os = std::cout, std::string const &indent = "" );
   virtual void WriteContent(std::ofstream *fp, FileType t);
};
} // end namespace gdcm
//-----------------------------------------------------------------------------
#endif
