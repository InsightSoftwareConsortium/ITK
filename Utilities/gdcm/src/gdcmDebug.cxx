/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmDebug.cxx
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

#include "gdcmDebug.h"
#include <iostream>

namespace gdcm 
{
//-----------------------------------------------------------------------------
// Warning message level to be displayed
static bool DebugFlag   = false;
static bool DebugToFile = false;
static std::ofstream DebugFile;

//-----------------------------------------------------------------------------
// Constructor / Destructor
Debug::Debug()
{

}

Debug::~Debug()
{
  if ( DebugFile.is_open() )
      DebugFile.close();     
}

//-----------------------------------------------------------------------------
// Public
/**
 * \brief   Sets the debug flag
 * @param   flag Set the debug flag
 */ 
void Debug::SetDebugFlag (bool flag) 
{
   DebugFlag = flag;
}

/**
 * \brief   Gets the debug flag value
 * @return debug flag value
 */ 
bool Debug::GetDebugFlag ()
{
   return DebugFlag;
}

/**
 * \brief   Accessor
 * @param   flag whether we want to redirect to file
 */ 
void Debug::SetDebugToFile (bool flag) 
{
   DebugToFile = flag;
}

/**
 * \brief   Accessor to know whether debug info are redirected to file
 */ 
bool Debug::GetDebugToFile ()
{
   return DebugToFile;
}

/**
 * \brief Set the filename the debug stream should be redirect to
 *        Settting a filename also sets DebugToFile to true
 * @param   filename  File to redirect debug info
 *          Absolutely nothing is check. You have to pass in
 *          a correct filename
 */ 
void Debug::SetDebugFilename (std::string const &filename)
{
   DebugToFile = true;  // Just in case ... 
   DebugFlag   = true;  // Just in case ...
   if ( DebugFile.is_open() )
      DebugFile.close();
   DebugFile.open( filename.c_str() );
}

/**
 * \brief Internal use only. Allow us to retrieve the static from anywhere
 *        in gdcm code
 * @return Debug file
 */
std::ofstream &Debug::GetDebugFile ()
{
  return DebugFile;
}

//-----------------------------------------------------------------------------
// Protected

//-----------------------------------------------------------------------------
// Private
   
//-----------------------------------------------------------------------------
// Print

//-----------------------------------------------------------------------------
} // end namespace gdcm
