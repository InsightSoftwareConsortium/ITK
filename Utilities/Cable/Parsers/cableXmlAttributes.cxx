/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cableXmlAttributes.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#include "cableXmlAttributes.h"

#include <stdlib.h>

namespace xml
{


/**
 * Set the value of a given attribute.
 */
void
Attributes
::Set(const String& a, const String& v)
{
  m_Attrs[a] = v;
}


/**
 * Get the string representation of an attribute.
 */
const char*
Attributes
::Get(const String& a) const
{
  if(m_Attrs.count(a) > 0)
    {
    return m_Attrs.find(a)->second.c_str();
    }
  else
    {
    throw MissingAttributeException(a.c_str());
    return NULL;
    }
}


/**
 * Get an attribute with conversion to integer.
 */
int
Attributes
::GetAsInteger(const String& a) const
{
  return atoi(this->Get(a));
}


/**
 * Get an attribute with conversion to boolean.
 */
bool
Attributes
::GetAsBoolean(const String& a) const
{
  return (this->GetAsInteger(a) != 0);
}


/**
 * Check if an attribute is available.
 */
bool
Attributes
::Have(const String& a) const
{
  return (m_Attrs.count(a) > 0);
}


} // namespace xml
