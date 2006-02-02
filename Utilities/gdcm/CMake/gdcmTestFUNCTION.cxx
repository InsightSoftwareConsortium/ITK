/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmTestFUNCTION.cxx
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

// Minimal test for existence of __FUNCTION__ pseudo-macro
#include <string.h>

int TestFUNCTION()
{
#ifdef __BORLANDC__
  #ifndef __FUNCTION__
    #define __FUNCTION__ __FUNC__
  #endif
#endif
  const char *f = __FUNCTION__;
  int r = strcmp( "TestFUNCTION", f);
  return r;
}
 
int main()
{
  return TestFUNCTION();
}

