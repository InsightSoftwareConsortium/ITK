/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmBase.h
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

#ifndef GDCMBASE_H
#define GDCMBASE_H

#include "gdcmCommon.h"
#include <iostream>

namespace gdcm 
{
//-----------------------------------------------------------------------------
/**
 * \brief Base class of all gdcm classes
 * Contains the Print related methods :
 *  - Print 
 *  - SetPrintLevel / GetPrintLevel 
 */
class GDCM_EXPORT Base
{
public:
   Base( );
   virtual ~Base();

   virtual void Print(std::ostream &os = std::cout, 
                      std::string const & indent = "" ); 

   /// \brief Sets the print level for the Dicom Header Elements
   /// \note 0 for Light Print; 1 for 'medium' Print, 2 for Heavy Print
   void SetPrintLevel(int level) { PrintLevel = level; }

   /// \brief Gets the print level for the Dicom Entries
   int GetPrintLevel() { return PrintLevel; }

protected:
   /// \brief Amount of printed details for each Dicom Entries :
   /// 0 : stands for the least detail level.
   int PrintLevel;
};
} // end namespace gdcm

//-----------------------------------------------------------------------------
#endif
