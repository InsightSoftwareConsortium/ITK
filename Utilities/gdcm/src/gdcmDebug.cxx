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

#include <iostream>
#include "gdcmDebug.h"

namespace gdcm 
{

//-----------------------------------------------------------------------------
/**
 * \brief   constructor
 * @param level debug level
 */ 
Debug::Debug(int level) 
{
   DebugLevel = level;
}

/**
 * \brief   Accessor
 * @param   level Set the debug level
 */ 
void Debug::SetDebug(int level) 
{
   DebugLevel = level;
}

/**
 * \brief   Verbose 
 * @param level level
 * @param msg1 first message part
 * @param msg2 second message part 
 */
void Debug::Verbose(int level, const char * msg1, const char * msg2) 
{
   if (level > DebugLevel)
   {
      return ;
   }
   std::cerr << "gdcm::" << msg1 << ' ' << msg2 << std::endl << std::flush;
}

/**
 * \brief   Error 
 * @param test test
 * @param msg1 first message part
 * @param msg2 second message part 
 */
void Debug::Error(bool test, const char * msg1, const char * msg2) 
{
   if (!test)
   {
      return;
   }
   std::cerr << "gdcm::" << msg1 << ' ' << msg2 << std::endl << std::flush;
   Exit(1);
}

/**
 * \brief   Error 
 * @param msg1 first message part
 * @param msg2 second message part
 * @param msg3 Third message part  
 */
void Debug::Error(const char* msg1, const char* msg2,
                      const char* msg3) 
{
   std::cerr << "gdcm::" << msg1 << ' ' << msg2 << ' ' << msg3
             << std::endl << std::flush;
   Exit(1);
}

/**
 * \brief   Assert 
 * @param level level 
 * @param test test
 * @param msg1 first message part
 * @param msg2 second message part
 */
void Debug::Assert(int level, bool test, const char * msg1, 
                       const char * msg2) 
{
   if (level > DebugLevel)
   {
      return ;
   }
   if (!test)
   {
      std::cerr << "gdcm::" <<  msg1 << ' ' << msg2
                << std::endl << std::flush;
   }
}

/**
 * \brief   Exit 
 * @param a return code 
 */
void Debug::Exit(int a) 
{
   exit(a);    // Found in #include <stdlib.h>
}

} // end namespace gdcm
