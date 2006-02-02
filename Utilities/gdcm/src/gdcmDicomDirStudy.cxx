/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmDicomDirStudy.cxx
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

#include "gdcmDicomDirStudy.h"
#include "gdcmDicomDirElement.h"
#include "gdcmGlobal.h"
#include "gdcmDicomDirSerie.h"
#include "gdcmDicomDirVisit.h"
#include "gdcmDebug.h"

namespace gdcm 
{
//-----------------------------------------------------------------------------
// Constructor / Destructor
/**
 * \brief  Constructor 
 * \note End user must use : DicomDirPatient::NewStudy()
 */
DicomDirStudy::DicomDirStudy(bool empty)
              :DicomDirObject()
{
   if ( !empty )
   {
      ListDicomDirStudyElem const &elemList = 
         Global::GetDicomDirElements()->GetDicomDirStudyElements();
      FillObject(elemList);
   }
}

/**
 * \brief   Canonical destructor.
 */
DicomDirStudy::~DicomDirStudy() 
{
   ClearSerie();
}

//-----------------------------------------------------------------------------
// Public
/**
 * \brief   Writes the Object
 * @param fp ofstream to write to
 * @param t Type of the File (explicit VR, implicitVR, ...) 
 * @return
 */ 
void DicomDirStudy::WriteContent(std::ofstream *fp, FileType t)
{
   DicomDirObject::WriteContent(fp, t);

   for(ListDicomDirSerie::iterator cc = Series.begin();
                                   cc!= Series.end();
                                 ++cc )
   {
      (*cc)->WriteContent( fp, t );
   }

   for(ListDicomDirVisit::iterator icc = Visits.begin();
                                   icc!= Visits.end();
                                 ++icc )
   {
      (*icc)->WriteContent( fp, t );
   }
}

/**
 * \brief   adds a new Serie at the beginning of the SerieList
 *          of a partially created DICOMDIR
 */
DicomDirSerie *DicomDirStudy::NewSerie()
{
   DicomDirSerie *st = new DicomDirSerie();
   Series.push_back(st);
   return st;
} 

/**
 * \brief  Remove all series in the study 
 */
void DicomDirStudy::ClearSerie()
{
   for(ListDicomDirSerie::iterator cc = Series.begin();
                                   cc != Series.end();
                                 ++cc )
   {
      delete *cc;
   }
   Series.clear();
}

/**
 * \brief   Get the first entry while visiting the DicomDirSeries
 * \return  The first DicomDirSerie if found, otherwhise NULL
 */
DicomDirSerie *DicomDirStudy::GetFirstSerie()
{
   ItSerie = Series.begin();
   if (ItSerie != Series.end())
      return *ItSerie;
   return NULL;
}

/**
 * \brief   Get the next entry while visiting the DicomDirSeries
 * \note : meaningfull only if GetFirstEntry already called
 * \return  The next DicomDirSerie if found, otherwhise NULL
 */
DicomDirSerie *DicomDirStudy::GetNextSerie()
{
   gdcmAssertMacro (ItSerie != Series.end());

   ++ItSerie;
   if (ItSerie != Series.end())
      return *ItSerie;
   return NULL;
}  

/**
 * \brief   Get the last entry while visiting the DicomDirSeries
 * \return  The last DicomDirSerie if found, otherwhise NULL
 */
DicomDirSerie *DicomDirStudy::GetLastSerie()
{
   ItSerie = Series.end();
   if (ItSerie != Series.begin())
   {
     --ItSerie;
      return *ItSerie;
   }
   return NULL;
}


/**
 * \brief   adds a new Visit at the beginning of the VisitList
 *          of a partially created DICOMDIR
 */
DicomDirVisit *DicomDirStudy::NewVisit()
{
   DicomDirVisit *st = new DicomDirVisit();
   Visits.push_back(st);
   return st;
} 

/**
 * \brief  Remove all visits in the study 
 */
void DicomDirStudy::ClearVisit()
{
   for(ListDicomDirVisit::iterator cc =  Visits.begin();
                                   cc != Visits.end();
                                 ++cc )
   {
      delete *cc;
   }
   Visits.clear();
}

/**
 * \brief   Get the first entry while visiting the DicomDirVisit
 * \return  The first DicomDirVisit if found, otherwhise NULL
 */
DicomDirVisit *DicomDirStudy::GetFirstVisit()
{
   ItVisit = Visits.begin();
   if (ItVisit != Visits.end())
      return *ItVisit;
   return NULL;
}

/**
 * \brief   Get the next entry while visiting the DicomDirVisit
 * \note : meaningfull only if GetFirstEntry already called
 * \return  The next DicomDirVisit if found, otherwhise NULL
 */
DicomDirVisit *DicomDirStudy::GetNextVisit()
{
   gdcmAssertMacro (ItVisit != Visits.end());

   ++ItVisit;
   if (ItVisit != Visits.end())
      return *ItVisit;
   return NULL;
}

/**
 * \brief   Get the last entry while visiting the DicomDirVisit
 * \return  The last DicomDirVisit if found, otherwhise NULL
 */
DicomDirVisit *DicomDirStudy::GetLastVisit()
{
   ItVisit = Visits.end();
   if (ItVisit != Visits.begin())
   {
     --ItVisit;
      return *ItVisit;
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
 * @return
 */ 
void DicomDirStudy::Print(std::ostream &os, std::string const & )
{
   os << "STUDY" << std::endl;
   DicomDirObject::Print(os);

   for(ListDicomDirSerie::iterator cc = Series.begin();
                                   cc != Series.end();
                                   ++cc)
   {
      (*cc)->SetPrintLevel(PrintLevel);
      (*cc)->Print(os);
   }

   for(ListDicomDirVisit::iterator cc2 =  Visits.begin();
                                   cc2 != Visits.end();
                                   ++cc2)
   {
      (*cc2)->SetPrintLevel(PrintLevel);
      (*cc2)->Print(os);
   }

}

//-----------------------------------------------------------------------------
} // end namespace gdcm
