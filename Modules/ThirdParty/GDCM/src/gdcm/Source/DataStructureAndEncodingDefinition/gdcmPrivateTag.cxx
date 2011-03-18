/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library
  Module:  $URL$

  Copyright (c) 2006-2010 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "gdcmPrivateTag.h"
#include "gdcmTrace.h"

#include <stdio.h> // sscanf

namespace gdcm
{
  bool PrivateTag::ReadFromCommaSeparatedString(const char *str)
    {
    unsigned int group = 0, element = 0;
    std::string owner;
    owner.resize( strlen(str) );
    if( !str || sscanf(str, "%04x,%04x,%s", &group , &element, &owner[0] ) != 3 )
      {
      gdcmDebugMacro( "Problem reading Private Tag: " << str );
      return false;
      }
    SetGroup( group );
    SetElement( element );
    SetOwner( owner.c_str() );
    return true;
    }

} // end namespace gdcm
