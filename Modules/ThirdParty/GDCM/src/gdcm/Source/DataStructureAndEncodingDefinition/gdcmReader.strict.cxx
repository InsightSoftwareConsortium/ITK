/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
// do the magic:
#define GDCM_FORCE_EXPORT
#define GDCM_OVERRIDE_BROKEN_IMPLEMENTATION
#undef gdcm_ns
#define gdcm_ns gdcmstrict
#ifdef GDCM_SUPPORT_BROKEN_IMPLEMENTATION
#error misconfiguration
#endif
#include "gdcmReader.h"

namespace gdcm
{
  // make it public:
  GDCM_EXPORT
  bool StrictReadUpToTag( const char * filename, Tag const & last, std::set<Tag> const & skiptags )
    {
    gdcmstrict::Reader reader;
    assert( filename );
    reader.SetFileName( filename );
    bool read = false;
    try
      {
      // Start reading all tags, including the 'last' one:
      read = reader.ReadUpToTag(last, skiptags);
      }
    catch(std::exception & ex)
      {
      (void)ex;
      gdcmWarningMacro( "Failed to read:" << filename << " with ex:" << ex.what() );
      }
    catch(...)
      {
      gdcmWarningMacro( "Failed to read:" << filename  << " with unknown error" );
      }
    return read;
    }
} // end namespace gdcm
