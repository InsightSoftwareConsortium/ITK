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
#include "itkStringStream.h"
#include <iostream>

namespace itk
{

/**
 * Default constructor.  Use this to create a re-usable instance.
 */
StringStream::StringStream()
{
}


/**
 * Destructor will set the  result to the string value if an
 * interpreter was provided to the constructor, and GetString() and
 * Reset() were never called.
 */
StringStream::~StringStream()
{
}


/**
 * Get the string that has been written to the stream.  This disables
 * further writing until Reset() is called.
 */
const char* StringStream::GetString()
{
  m_String = this->str();
  return m_String.c_str();
}


/**
 * Reset the stream to accept new input starting from an empty string.
 */
void StringStream::Reset()
{
  this->seekp(0, std::ios::beg);
}

} // namespace itk
