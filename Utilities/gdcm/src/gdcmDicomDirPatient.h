/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmDicomDirPatient.h
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

#ifndef GDCMPATIENT_H
#define GDCMPATIENT_H

#include "gdcmDicomDirObject.h"

namespace gdcm 
{
class DicomDirStudy;

//-----------------------------------------------------------------------------
typedef std::list<DicomDirStudy*> ListDicomDirStudy;

//-----------------------------------------------------------------------------
/**
 * \brief   describes a PATIENT within a DICOMDIR (DicomDir)
 */

class GDCM_EXPORT DicomDirPatient : public DicomDirObject 
{
public:
   DicomDirPatient(bool empty=false); 
   ~DicomDirPatient();

   void Print(std::ostream &os = std::cout, std::string const &indent = "" );
   void WriteContent(std::ofstream *fp, FileType t);
  
   // Patient methods
   /// \brief Adds a new gdcmDicomDirStudy to the Patient
   void AddStudy(DicomDirStudy *obj) { Studies.push_back(obj); };
   DicomDirStudy *NewStudy(); 
   void ClearStudy();

   DicomDirStudy *GetFirstStudy();
   DicomDirStudy *GetNextStudy();
   DicomDirStudy *GetLastStudy();

private:

   /// chained list of DicomDirStudy  (to be exploited hierarchicaly)
   ListDicomDirStudy Studies;
   /// iterator on the DicomDirStudies of the current DicomDirPatient
   ListDicomDirStudy::iterator ItStudy;
};
} // end namespace gdcm

//-----------------------------------------------------------------------------
#endif
