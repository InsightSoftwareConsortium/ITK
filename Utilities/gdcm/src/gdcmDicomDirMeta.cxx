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

namespace gdcm 
{

//-----------------------------------------------------------------------------
// Constructor / Destructor

/**
 * \ingroup DicomDirMeta
 * \brief  Constructor
 */ 
DicomDirMeta::DicomDirMeta(TagDocEntryHT* ptagHT):
   DicomDirObject(ptagHT)
{

}

/**
 * \ingroup DicomDirMeta
 * \brief   Canonical destructor.
 */
DicomDirMeta::~DicomDirMeta() 
{
   
}

//-----------------------------------------------------------------------------
// Print
/**
 * \brief   Prints the Meta Elements
 */ 
void DicomDirMeta::Print(std::ostream& os)
{
   os << "META" << std::endl;
   // warning : META doesn't behave exactly like a Objet 
   for (ListDocEntry::iterator i = DocEntries.begin();  
        i != DocEntries.end();
        ++i)
      (*i)->Print();    
}


//-----------------------------------------------------------------------------
// Public


/**
 * \brief   Writes the Meta Elements
 * @return
 */ 
void DicomDirMeta::Write(std::ofstream* fp, FileType t)
{   
   for (ListDocEntry::iterator i = DocEntries.begin();  
                              i != DocEntries.end();
                              ++i)
   {
      (*i)->Write(fp, t);
   }
}

//-----------------------------------------------------------------------------
// Protected

//-----------------------------------------------------------------------------
// Private

//-----------------------------------------------------------------------------

} // end namespace gdcm
