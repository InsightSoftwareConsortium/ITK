/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmDicomDirImage.cxx
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

#include "gdcmDicomDirImage.h"
#include "gdcmValEntry.h"
#include "gdcmGlobal.h"

namespace gdcm 
{
//-----------------------------------------------------------------------------
// Constructor / Destructor
/**
 * \brief  Constructor 
 * \note End user must use : DicomDirSerie::NewImage()
 */
DicomDirImage::DicomDirImage(bool empty):
   DicomDirObject()
{
   if( !empty )
   {
      ListDicomDirImageElem const &elemList = 
         Global::GetDicomDirElements()->GetDicomDirImageElements();
      FillObject(elemList);
   }
}

/**
 * \brief   Canonical destructor.
 */
DicomDirImage::~DicomDirImage() 
{
}

//-----------------------------------------------------------------------------
// Public

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
void DicomDirImage::Print(std::ostream &os, std::string const & )
{
   os << "IMAGE : ";
   for(ListDocEntry::iterator i = DocEntries.begin();
                              i!= DocEntries.end();
                              ++i)
   {
      if( (*i)->GetGroup() == 0x0004 && (*i)->GetElement() == 0x1500 )
      {
         os << (dynamic_cast<ValEntry *>(*i))->GetValue(); //FIXME
      }
   }
   os << std::endl;

   DicomDirObject::Print(os);
}

//-----------------------------------------------------------------------------
} // end namespace gdcm

