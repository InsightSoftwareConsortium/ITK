/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmDicomDirSerie.cxx
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

#include "gdcmDicomDirSerie.h"
#include "gdcmDicomDirElement.h"
#include "gdcmGlobal.h"

namespace gdcm 
{

//-----------------------------------------------------------------------------
// Constructor / Destructor
/**
 * \brief  Constructor 
 * @param  s  SQ Item holding the elements related to this "SERIE" part
 * @param ptagHT pointer to the HTable (DicomDirObject needs it 
 *               to build the DocEntries)
 */
DicomDirSerie::DicomDirSerie(SQItem* s, TagDocEntryHT* ptagHT) :
   DicomDirObject(ptagHT)
{
   DocEntries = s->GetDocEntries();
}

/**
 * \brief  Constructor 
 * @param ptagHT pointer to the HTable (DicomDirObject needs it 
 *               to build the DocEntries)
 */
DicomDirSerie::DicomDirSerie(TagDocEntryHT* ptagHT):
   DicomDirObject(ptagHT)
{
}
/**
 * \brief   Canonical destructor.
 */
DicomDirSerie::~DicomDirSerie() 
{
   for(ListDicomDirImage::iterator cc = Images.begin();
                                   cc != Images.end();
                                   ++cc)
   {
      delete *cc;
   }
}

//-----------------------------------------------------------------------------
// Print
/**
 * \brief   Prints the Object
 * @return
 */ 
void DicomDirSerie::Print(std::ostream& os)
{
   os << "SERIE" << std::endl;
   DicomDirObject::Print(os);

   for(ListDicomDirImage::iterator cc = Images.begin();
                                   cc != Images.end();
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
void DicomDirSerie::Write(std::ofstream* fp, FileType t)
{
   DicomDirObject::Write(fp, t);

   for(ListDicomDirImage::iterator cc = Images.begin();
                                   cc!= Images.end();
                                 ++cc )
   {
      (*cc)->Write( fp, t );
   }
}

/**
 * \brief   adds a new Image (with the basic elements) to a partially created DICOMDIR
 */
DicomDirImage* DicomDirSerie::NewImage()
{
   ListDicomDirImageElem const & elemList = 
      Global::GetDicomDirElements()->GetDicomDirImageElements();

   DicomDirImage* st = new DicomDirImage(PtagHT);
   FillObject(elemList);
   Images.push_front(st);

   return st;   
} 
//-----------------------------------------------------------------------------
// Protected

//-----------------------------------------------------------------------------
// Private

//-----------------------------------------------------------------------------
} // end namespace gdcm


