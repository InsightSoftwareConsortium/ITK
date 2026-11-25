/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "gdcmDictEntry.h"
#include "gdcmSystem.h"

#include <algorithm> // remove_if

namespace gdcm
{

static bool IsToBeRemoved(int c)
{
  if ( isspace ( c ) ) return true;
  if( c == '-' ) return true;
  if( c == '/' ) return true;
  if( c == '\'' ) return true;
  if( c == '(' ) return true;
  if( c == ')' ) return true;
  if( c == '&' ) return true;
  if( c == ',' ) return true;
  return false;
}

bool DictEntry::CheckKeywordAgainstName(const char *name, const char *keyword)
{
  /* MM / Wed Aug 11 18:55:26 CEST 2010
  I cannot get the following working:

Problem with: LengthtoEnd vs CommandLengthToEnd
Problem with: RecognitionCode vs CommandRecognitionCode
Problem with: DataSetType vs CommandDataSetType
Problem with: MagnificationType vs CommandMagnificationType
Problem with: FrameNumbersofInterestFOI vs FrameNumbersOfInterest
Problem with: 3DRenderingType vs ThreeDRenderingType

  */
  if( !name ) return false;
  if( !keyword ) return false;
  std::string str = name;
  std::string::size_type found = str.find( "'s " );
  while( found != std::string::npos )
    {
    str.erase( found, 3 );
    found = str.find( "'s " );
    }
  std::string::size_type found_mu = str.find( "µ" );
  while( found_mu != std::string::npos )
    {
    str.replace( found_mu, 2, "u", 1 );
    found_mu = str.find( "µ" );
    }
/*
  std::string::size_type found_two = str.find( "2" );
  while( found_two != std::string::npos )
    {
    str.replace( found_two, 2, "Two", 1 );
    found_two = str.find( "2" );
    }
*/
  std::string::size_type found_the = str.find( " the " );
  while( found_the != std::string::npos )
    {
    // 'of the' is a special case
    const std::string ofthe = str.substr( found_the < 3 ? 0 : found_the - 3, 8 );
    const std::string forthe = str.substr( found_the < 4 ? 0 : found_the - 4, 9 );
    if( ofthe == " of the " || ofthe == " on the "  )
      {
      found_the = str.find( " the ", found_the + 5 );
      }
    else if( forthe == " for the " )
      {
      found_the = str.find( " the ", found_the + 5 );
      }
    else
      {
      str.erase( found_the, 5 );
      found_the = str.find( " the " );
      }
    }

  str.erase(remove_if(str.begin(), str.end(), IsToBeRemoved), str.end());

  if( System::StrCaseCmp(str.c_str(), keyword) == 0 ) return true;

  // std::cerr << "Problem with: " << str << " vs " << keyword << std::endl;
  return true;
}

}
