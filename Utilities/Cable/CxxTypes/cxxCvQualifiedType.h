/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxCvQualifiedType.h
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
#ifndef _cxxCvQualifiedType_h
#define _cxxCvQualifiedType_h

#include "cxxType.h"

#include <vector>

namespace _cxx_
{


/**
 * Hold cv-qualifiers attached to a type.
 */
class _cxx_EXPORT CvQualifiedType
{
public:
  typedef CvQualifiedType Self;
  
  CvQualifiedType();
  CvQualifiedType(const Type*);
  CvQualifiedType(const Type*, bool, bool);
  CvQualifiedType(const Self&);
  
  bool IsConst() const    { return m_Const; }
  bool IsVolatile() const { return m_Volatile; }
  
  String GetName() const { return this->GenerateName(""); }
  const Type* GetType() const { return m_Type; }
  CvQualifiedType GetMoreQualifiedType(bool isConst, bool isVolatile) const;
  bool IsEquallyOrMoreCvQualifiedThan(const CvQualifiedType&) const;
  String GenerateName(const String& indirection,
                      bool isConst = false, bool isVolatile = false) const;
  
  bool operator== (const Self&) const;
  bool operator< (const Self&) const;
  
  RepresentationType GetRepresentationType() const;
  
  /*@{
   * Quick type representation test.
   */     
  bool IsArrayType() const           { return m_Type->IsArrayType(); }
  bool IsClassType() const           { return m_Type->IsClassType(); }
  bool IsFunctionType() const        { return m_Type->IsFunctionType(); }
  bool IsFundamentalType() const     { return m_Type->IsFundamentalType(); }
  bool IsPointerType() const         { return m_Type->IsPointerType(); }
  bool IsPointerToMemberType() const { return m_Type->IsPointerToMemberType(); }
  bool IsReferenceType() const       { return m_Type->IsReferenceType(); }
  //@}

private:
  /**
   * The type to which these cv-qualifiers apply.
   */
  const Type* m_Type;
  
  /**
   * Flag for presence of "const" cv-qualifier.
   */
  bool m_Const;

  /**
   * Flag for presence of "volatile" cv-qualifier.
   */
  bool m_Volatile;
};

/**
 * A vector of CvQualifiedType instances.
 */
typedef std::vector<CvQualifiedType> CvQualifiedTypes;
  
} // namespace _cxx_


#endif
