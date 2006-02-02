/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmDicomDirPatient.cxx
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

#include "gdcmDicomDirPatient.h"
#include "gdcmDicomDirElement.h"
#include "gdcmGlobal.h"
#include "gdcmDicomDirStudy.h"
#include "gdcmSQItem.h"
#include "gdcmDebug.h"

namespace gdcm 
{
//-----------------------------------------------------------------------------
// Constructor / Destructor
/**
 * \brief   Constructor
 * \note End user must use : DicomDir::NewPatient()
 */
DicomDirPatient::DicomDirPatient(bool empty)
                :DicomDirObject()
{
   if ( !empty )
   {
      ListDicomDirStudyElem const &elemList = 
         Global::GetDicomDirElements()->GetDicomDirPatientElements();
      FillObject(elemList);
   }
}

/**
 * \brief   Canonical destructor.
 */
DicomDirPatient::~DicomDirPatient() 
{
   ClearStudy();
}

//-----------------------------------------------------------------------------
// Public
/**
 * \brief   Writes the Object
 * @param fp ofstream to write to
 * @param t Type of the File (explicit VR, implicitVR, ...) 
 */ 
void DicomDirPatient::WriteContent(std::ofstream *fp, FileType t)
{
   DicomDirObject::WriteContent(fp, t);

   for(ListDicomDirStudy::iterator cc = Studies.begin();
                                   cc!= Studies.end();
                                 ++cc )
   {
      (*cc)->WriteContent( fp, t );
   }
}

/**
 * \brief   adds a new Patient at the beginning of the PatientList
 *          of a partially created DICOMDIR
 */
DicomDirStudy* DicomDirPatient::NewStudy()
{
   DicomDirStudy *st = new DicomDirStudy();
   Studies.push_back(st);
   return st; 
}   

/**
 * \brief  Remove all studies in the patient 
 */
void DicomDirPatient::ClearStudy()
{
   for(ListDicomDirStudy::const_iterator cc = Studies.begin();
                                         cc != Studies.end(); 
                                       ++cc )
   {
      delete *cc;
   }
   Studies.clear();
}

/**
 * \brief   Get the first entry while visiting the DicomDirStudy
 * \return  The first DicomDirStudy if found, otherwhise NULL
 */ 
DicomDirStudy *DicomDirPatient::GetFirstStudy()
{
   ItStudy = Studies.begin();
   if (ItStudy != Studies.end())
      return *ItStudy;
   return NULL;
}

/**
 * \brief   Get the next entry while visiting the DicomDirStudies
 * \note : meaningfull only if GetFirstEntry already called
 * \return  The next DicomDirStudies if found, otherwhise NULL
 */
DicomDirStudy *DicomDirPatient::GetNextStudy()
{
   gdcmAssertMacro (ItStudy != Studies.end())

   ++ItStudy;
   if (ItStudy != Studies.end())
      return *ItStudy;
   return NULL;
}

/**
 * \brief   Get the first entry while visiting the DicomDirStudy
 * \return  The first DicomDirStudy if found, otherwhise NULL
 */ 
DicomDirStudy *DicomDirPatient::GetLastStudy()
{
   ItStudy = Studies.end();
   if (ItStudy != Studies.begin())
   {
      --ItStudy;
      return *ItStudy;
   }
   return NULL;
}

//-----------------------------------------------------------------------------
// Protected

//-----------------------------------------------------------------------------
// Private

//-----------------------------------------------------------------------------
// Print
/**
 * \brief   Prints the Object
 * @param os ostream to write to 
 * @param indent Indentation string to be prepended during printing
 */ 
void DicomDirPatient::Print(std::ostream &os, std::string const & )
{
   os << "PATIENT" << std::endl;
   DicomDirObject::Print(os);

   for(ListDicomDirStudy::const_iterator cc = Studies.begin();
                                         cc != Studies.end(); 
                                       ++cc )
   {
      (*cc)->SetPrintLevel(PrintLevel);
      (*cc)->Print(os);
   }
}

//-----------------------------------------------------------------------------
} // end namespace gdcm
