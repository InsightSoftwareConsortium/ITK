  /*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmBase.cxx
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

#include "gdcmBase.h"

namespace gdcm 
{
//-------------------------------------------------------------------------
// Constructor / Destructor
/**
 * \brief canonical constructor
 */
Base::Base( )
{
   PrintLevel = 0;
}

/**
 * \brief canonical destructor
 * \note  If the Header was created by the File constructor,
 *        it is destroyed by the File
 */
Base::~Base()
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
 * \brief   Print all the object
 * @param   os The output stream to be written to.
 * @param indent Indentation string to be prepended during printing
 */
void Base::Print(std::ostream &, std::string const & )
{
}

//-----------------------------------------------------------------------------
} // end namespace gdcm

