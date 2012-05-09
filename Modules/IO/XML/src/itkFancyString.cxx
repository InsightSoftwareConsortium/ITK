/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkFancyString.h"
#include "itkStringTools.h"

namespace itk
{

FancyString::FancyString() : std::string()
{
}

FancyString::FancyString( const std::string& str ) : std::string( str )
{
}

FancyString::FancyString( const char* s ) : std::string( s )
{
}

FancyString&
FancyString::operator=( const std::string& str )
{
  this->std::string::operator=( str );
  return *this;
}

FancyString&
FancyString::operator=( const char* s )
{
  this->std::string::operator=( s );
  return *this;
}

/** Function to cast this type to "const char *". */
FancyString::operator const char * () const
{
  return this->c_str();
}

/////////////////////////////////////////////////////////////////////////////
// helper functions for string manipulations
/////////////////////////////////////////////////////////////////////////////

/** Method to trim the spaces or user-specified characters on both ends of a string. */
FancyString&
FancyString::Trim( const std::string& dislike )
{
  StringTools::Trim( *this, dislike );
  return *this;
}

/** Method to trim the spaces or user-specified characters on left end of a string. */
FancyString&
FancyString::TrimLeft( const std::string& dislike )
{
  StringTools::TrimLeft( *this, dislike );
  return *this;
}

/** Method to trim the spaces or user-specified characters on right end of a string. */
FancyString&
FancyString::TrimRight( const std::string& dislike )
{
  StringTools::TrimRight( *this, dislike );
  return *this;
}

/** Method to covert lower-case characters to upper cases in a string. */
FancyString&
FancyString::ToUpperCase()
{
  StringTools::ToUpperCase( *this );
  return *this;
}

/** Method to covert upper-case characters to lower cases in a string. */
FancyString&
FancyString::ToLowerCase()
{
  StringTools::ToLowerCase( *this );
  return *this;
}

/** Method to split a string into two parts with user-defined delimiters. */
void
FancyString::Split( std::string& lpart, std::string& rpart, const std::string& delims ) const
{
  StringTools::Split( *this, lpart, rpart, delims );
}

/** Method to split a string into a sequence of strings with user-defined delimiters. */
void
FancyString::Split( std::vector<std::string>& result, const std::string& delims ) const
{
  StringTools::Split( *this, result, delims );
}

/**
 * Method to split a string into a sequence of sub-strings with user-defined delimiters,
 * then each sub-string is further splitted into a <key,value> pair with separators "=:".
 */
void
FancyString::Split( std::map<std::string,std::string>& result, const std::string& delims )
{
  StringTools::Split( *this, result, delims );
}

/** Method to test whether one string matches with another. */
bool
FancyString::MatchWith( const std::string& s2, bool ignoreCase )
{
  return StringTools::MatchWith( *this, s2, ignoreCase );
}

/** Method to test whether a string starts with a user-given sub-string. */
bool
FancyString::StartWith( const std::string& s2, bool ignoreCase )
{
  return StringTools::StartWith( *this, s2, ignoreCase );
}

/** Method to test whether a string ends with a user-given sub-string. */
bool
FancyString::EndWith( const std::string& s2, bool ignoreCase )
{
  return StringTools::EndWith( *this, s2, ignoreCase );
}

/** Method to test whether a string contains a user-given sub-string. */
bool
FancyString::ContainSub( const std::string& s2, bool ignoreCase )
{
  return StringTools::ContainSub( *this, s2, ignoreCase );
}

} // namespace itk
