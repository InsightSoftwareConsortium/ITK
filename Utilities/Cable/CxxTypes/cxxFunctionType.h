/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxFunctionType.h
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
#ifndef _cxxFunctionType_h
#define _cxxFunctionType_h

#include "cxxCvQualifiedType.h"

#include <vector>

namespace _cxx_
{


/**
 * Represent a C++ function type.  This consists of the return type and
 * argument types.
 */
class _cxx_EXPORT FunctionType: public Type
{
public:
  typedef FunctionType Self;
  
  virtual RepresentationType GetRepresentationType() const;
  static FunctionType* SafeDownCast(Type*);
  static const FunctionType* SafeDownCast(const Type*);
  
  const CvQualifiedType& GetReturnType() const
    { return m_ReturnType; }
  const CvQualifiedTypes& GetArgumentTypes() const
    { return m_Arguments; }
  
  virtual String GenerateName(const String&, bool, bool) const;
  virtual String GenerateDeclaration(const String&, bool, bool) const;
protected:
  String GenerateArgumentString() const;
  
  FunctionType(const CvQualifiedType& returnType,
               const CvQualifiedTypes& arguments);
  FunctionType(const Self&): m_ReturnType(NULL) {}
  void operator=(const Self&) {}
  virtual ~FunctionType() {}
  
private:
  /**
   * The function's return type.
   */
  CvQualifiedType m_ReturnType;
  
  /**
   * The function's argument types.
   */
  CvQualifiedTypes m_Arguments;
  
  friend class TypeSystem;
};

} // namespace _cxx_


#endif
