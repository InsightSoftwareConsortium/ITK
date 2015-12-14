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

FancyString::FancyString()// : std::string()
{
}

FancyString::FancyString( const std::string& str )// : std::string( str )
{
  this->m_Value = str;
}

FancyString::FancyString( const char* s )// : std::string( s )
{
  this->m_Value = std::string( s );
}

FancyString&
FancyString::operator=( const std::string& str )
{
  this->m_Value = str;
  return *this;
}

FancyString&
FancyString::operator=( const char* s )
{
  this->m_Value = std::string( s );
  return *this;
}

/** Function to cast this type to "const char *". */
FancyString::operator const char * () const
{
  return this->m_Value.c_str();
}

/** Function to convert this value to a "std::string". */
FancyString::operator const std::string & () const
{
  return this->ToString();
}

/** Function to convert this value to a "std::string". */
const std::string& FancyString::ToString() const
{
  return this->m_Value;
}

/////////////////////////////////////////////////////////////////////////////
// helper functions for string manipulations
/////////////////////////////////////////////////////////////////////////////

/** Clear all characters. */
void
FancyString::ClearContent()
{
  this->m_Value.clear();
}

/** Extend the string by appending additional characters. */
void
FancyString::Append(const FancyString& str)
{
  this->m_Value.append(str.ToString());
}

/** Method to trim the spaces or user-specified characters on both ends of a string. */
FancyString&
FancyString::Trim( const std::string& dislike )
{
  StringTools::Trim( this->m_Value, dislike );
  return *this;
}

/** Method to trim the spaces or user-specified characters on left end of a string. */
FancyString&
FancyString::TrimLeft( const std::string& dislike )
{
  StringTools::TrimLeft( this->m_Value, dislike );
  return *this;
}

/** Method to trim the spaces or user-specified characters on right end of a string. */
FancyString&
FancyString::TrimRight( const std::string& dislike )
{
  StringTools::TrimRight( this->m_Value, dislike );
  return *this;
}

/** Method to covert lower-case characters to upper cases in a string. */
FancyString&
FancyString::ToUpperCase()
{
  StringTools::ToUpperCase( this->m_Value );
  return *this;
}

/** Method to covert upper-case characters to lower cases in a string. */
FancyString&
FancyString::ToLowerCase()
{
  StringTools::ToLowerCase( this->m_Value );
  return *this;
}

/** Method to split a string into two parts with user-defined delimiters. */
void
FancyString::Split( std::string& lpart, std::string& rpart, const std::string& delims ) const
{
  StringTools::Split( this->m_Value, lpart, rpart, delims );
}

/** Method to split a string into a sequence of strings with user-defined delimiters. */
void
FancyString::Split( std::vector<std::string>& result, const std::string& delims ) const
{
  StringTools::Split( this->m_Value, result, delims );
}

/**
 * Method to split a string into a sequence of sub-strings with user-defined delimiters,
 * then each sub-string is further splitted into a <key,value> pair with separators "=:".
 */
void
FancyString::Split( std::map<std::string,std::string>& result, const std::string& delims )
{
  StringTools::Split( this->m_Value, result, delims );
}

/** Method to test whether one string matches with another. */
bool
FancyString::MatchWith( const std::string& s2, bool ignoreCase )
{
  return StringTools::MatchWith( this->m_Value, s2, ignoreCase );
}

/** Method to test whether a string starts with a user-given sub-string. */
bool
FancyString::StartWith( const std::string& s2, bool ignoreCase )
{
  return StringTools::StartWith( this->m_Value, s2, ignoreCase );
}

/** Method to test whether a string ends with a user-given sub-string. */
bool
FancyString::EndWith( const std::string& s2, bool ignoreCase )
{
  return StringTools::EndWith( this->m_Value, s2, ignoreCase );
}

/** Method to test whether a string contains a user-given sub-string. */
bool
FancyString::ContainSub( const std::string& s2, bool ignoreCase )
{
  return StringTools::ContainSub( this->m_Value, s2, ignoreCase );
}

} // namespace itk

bool operator!=( itk::FancyString& s, const std::string& str)
{
  return s.ToString() != str;
}

bool operator!=( itk::FancyString& s, const char* str)
{
  return s.ToString() != str;
}

bool operator!=( itk::FancyString& s, const  itk::FancyString& str)
{
  return s.ToString() != str.ToString();
}

bool operator==( itk::FancyString& s, const std::string& str)
{
  return s.ToString() == str;
}

bool operator==( itk::FancyString& s, const char* str)
{
  return s.ToString() == str;
}

bool operator==( itk::FancyString& s, const  itk::FancyString& str)
{
  return s.ToString() == str.ToString();
}
