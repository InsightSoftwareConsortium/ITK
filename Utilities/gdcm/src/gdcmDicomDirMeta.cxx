/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmDicomDirMeta.cxx
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

#include "gdcmDicomDirMeta.h"
#include "gdcmDocument.h"
#include "gdcmDocEntry.h"
#include "gdcmGlobal.h"

namespace gdcm 
{
//-----------------------------------------------------------------------------
// Constructor / Destructor
/**
 * \brief  Constructor
 */ 
DicomDirMeta::DicomDirMeta(bool empty):
   DicomDirObject()
{
   if ( !empty )
   {
      ListDicomDirStudyElem const &elemList = 
         Global::GetDicomDirElements()->GetDicomDirMetaElements();
      FillObject(elemList);
   }
}

/**
 * \brief   Canonical destructor.
 */
DicomDirMeta::~DicomDirMeta() 
{
}

//-----------------------------------------------------------------------------
// Public
/**
 * \brief   Writes the Meta Elements
 * @param fp ofstream to write to
 * @param filetype type of the file (ACR, ImplicitVR, ExplicitVR, ...)
 * @return
 */ 
void DicomDirMeta::WriteContent(std::ofstream *fp, FileType filetype)
{   
   for (ListDocEntry::iterator i = DocEntries.begin();  
                              i != DocEntries.end();
                              ++i)
   {
      (*i)->WriteContent(fp, filetype);
   }
}

//-----------------------------------------------------------------------------
// Protected

//-----------------------------------------------------------------------------
// Private

//-----------------------------------------------------------------------------
// Print
/**
 * \brief   Prints the Meta Elements
 * @param os ostream to write to 
 * @param indent Indentation string to be prepended during printing
 */ 
void DicomDirMeta::Print(std::ostream &os, std::string const & )
{
   os << "META" << std::endl;
   // warning : META doesn't behave exactly like a Objet 
   for (ListDocEntry::iterator i = DocEntries.begin();
        i != DocEntries.end();
        ++i)
   {
      (*i)->SetPrintLevel(PrintLevel);
      (*i)->Print();
      os << std::endl;
   }
}

//-----------------------------------------------------------------------------
} // end namespace gdcm
