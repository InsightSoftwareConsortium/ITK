/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxPointerToMemberType.h
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
#ifndef _cxxPointerToMemberType_h
#define _cxxPointerToMemberType_h

#include "cxxPointerType.h"

namespace _cxx_
{

class ClassType;

/**
 * Represents a C++ pointer-to-member type.
 */
class _cxx_EXPORT PointerToMemberType: public PointerType
{
public:
  typedef PointerToMemberType Self;
  
  virtual RepresentationType GetRepresentationType() const;
  static PointerToMemberType* SafeDownCast(Type*);
  static const PointerToMemberType* SafeDownCast(const Type*);
  
  virtual String GenerateName(const String&, bool, bool) const;
  
  const ClassType* GetClassType() const;
protected:
  PointerToMemberType(const CvQualifiedType&, const ClassType*);  
  PointerToMemberType(const Self& s): PointerType(s), m_ClassType(NULL) {}
  void operator=(const Self&) {}
  virtual ~PointerToMemberType() {}
  
private:
  /**
   * The class type holding the member.
   */
  const ClassType* m_ClassType;
  
  friend class TypeSystem;
};

} // namespace _cxx_


#endif
