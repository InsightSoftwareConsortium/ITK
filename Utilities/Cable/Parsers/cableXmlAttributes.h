/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cableXmlAttributes.h
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
#ifndef _cableXmlAttributes_h
#define _cableXmlAttributes_h

#include "cableXmlParseException.h"

#include <map>

namespace xml
{


/**
 * Store the set of attributes provided to an element tag, and their values.
 */
class Attributes
{
public:
  void Set(const String& a, const String& v);
  const char* Get(const String& a) const;
  int GetAsInteger(const String& a) const;
  bool GetAsBoolean(const String& a) const;
  bool Have(const String& a) const;
  
private:
  /**
   * Map from attribute name to its value.
   */
  std::map<String, String>  m_Attrs;
};


/**
 * An attribute requested from an element begin tag is not known.
 */
class MissingAttributeException: public ParseException
{
public:
  MissingAttributeException(const char* missing):
    ParseException(), m_Missing(missing) {}
  virtual ~MissingAttributeException() {}
  
  void Print(std::ostream& os) const
    {
      os << "Missing element attribute: " << m_Missing.c_str()
         << std::endl;
    }
private:
  String m_Missing;
};


} // namespace xml
  
#endif
