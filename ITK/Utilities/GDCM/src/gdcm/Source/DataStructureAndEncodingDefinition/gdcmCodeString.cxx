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
#include "gdcmCodeString.h"

namespace gdcm
{
  bool CodeString::IsValid() const {
    if( !Internal.IsValid() ) return false;
    // Implementation specific:

    /*
     * Uppercase characters, 0-9, the SPACE character, and underscore _, of the
     * Default Character Repertoire
     */
    const_iterator it = Internal.begin();
    for( ; it != Internal.end(); ++it )
      {
      int c = *it;
      if( !isupper(c) && !isdigit(c) && c != ' ' && c != '_' )
        {
        return false;
        }
      }
    return true;
  }

#if !defined(GDCM_LEGACY_REMOVE)
  CodeString::size_type CodeString::size() const
    {
    GDCM_LEGACY_REPLACED_BODY(CodeString::size, "GDCM 2.2",
                              CodeString::size);
    return Internal.size();
    }
  std::string CodeString::Trim() const
    {
    GDCM_LEGACY_REPLACED_BODY(CodeString::Trim, "GDCM 2.2",
                              CodeString::Trim);
    return Internal;
    }
#endif

} // end namespace gdcm
