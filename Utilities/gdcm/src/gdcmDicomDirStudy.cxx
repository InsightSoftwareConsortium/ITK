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

namespace gdcm 
{
//-----------------------------------------------------------------------------
// Constructor / Destructor

/**
 * \ingroup DicomDirStudy
 * \brief constructor  
 * @param  s SQ Item holding the elements related to this "STUDY" part
 * @param ptagHT pointer to the HTable (DicomDirObject needs it 
 *               to build the HeaderEntries)
 */
DicomDirStudy::DicomDirStudy(SQItem* s, TagDocEntryHT* ptagHT):
   DicomDirObject(ptagHT)
{
   DocEntries = s->GetDocEntries();
}
/**
 * \ingroup DicomDirStudy
 * \brief constructor  
 * @param ptagHT pointer to the HTable (DicomDirObject needs it 
 *               to build the HeaderEntries)
 */
DicomDirStudy::DicomDirStudy(TagDocEntryHT* ptagHT):
   DicomDirObject(ptagHT)
{
}
/**
 * \ingroup DicomDirStudy
 * \brief   Canonical destructor.
 */
DicomDirStudy::~DicomDirStudy() 
{
   for(ListDicomDirSerie::iterator cc = Series.begin();
                                   cc != Series.end();
                                 ++cc )
   {
      delete *cc;
   }
}

//-----------------------------------------------------------------------------
// Print
/**
 * \ingroup DicomDirStudy
 * \brief   Prints the Object
 * @return
 */ 
void DicomDirStudy::Print(std::ostream& os)
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
}

//-----------------------------------------------------------------------------
// Public

/**
 * \brief   Writes the Object
 * @return
 */ 
void DicomDirStudy::Write(std::ofstream* fp, FileType t)
{
   DicomDirObject::Write(fp, t);

   for(ListDicomDirSerie::iterator cc = Series.begin();
                                   cc!= Series.end();
                                 ++cc )
   {
      (*cc)->Write( fp, t );
   }
}

/**
 * \ingroup DicomDirStudy
 * \brief   adds a new Serie at the begining of the SerieList
 *          of a partially created DICOMDIR
 */
DicomDirSerie* DicomDirStudy::NewSerie()
{
   ListDicomDirSerieElem const & elemList = 
      Global::GetDicomDirElements()->GetDicomDirSerieElements();   

   DicomDirSerie* st = new DicomDirSerie(PtagHT);
   FillObject(elemList);
   Series.push_front(st);

   return st;  
}   
//-----------------------------------------------------------------------------
// Protected

//-----------------------------------------------------------------------------
// Private

//-----------------------------------------------------------------------------
} // end namespace gdcm

