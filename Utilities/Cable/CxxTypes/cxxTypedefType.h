/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxTypedefType.h
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
#ifndef _cxxTypedefType_h
#define _cxxTypedefType_h

#include "cxxTypes.h"

namespace _cxx_
{

/**
 * Represent a C++ typedef.  It is simply a name for a CvQualifiedType.
 */
class _cxx_EXPORT TypedefType: public Type
{
public:
  typedef TypedefType Self;
  
  virtual RepresentationType GetRepresentationType() const;
  virtual bool IsTypedefType() const;
  
  virtual const Type* Id() const;
  virtual CvQualifiedType GetCvQualifiedType(bool, bool) const;
  virtual String GenerateName(const String& indirection,
                              bool isConst, bool isVolatile) const;
  
protected:
  TypedefType(const String& name, const CvQualifiedType&);
  TypedefType(const Self&): m_CvQualifiedType(NULL) {}
  void operator=(const Self&) {}
  virtual ~TypedefType() {}
  
private:
  /**
   * The name of the typedef type.
   */
  String m_Name;
  
  /**
   * The type to which the typedef refers, including optional cv-qualifiers.
   */
  CvQualifiedType m_CvQualifiedType;
};


} // namespace _cxx_

#endif
