/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    genCvTypeGenerator.h
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
#ifndef _genCvTypeGenerator_h
#define _genCvTypeGenerator_h

#include "cableSourceRepresentation.h"
#include "genUtils.h"
#include <iostream>

namespace gen
{


/**
 * Holds a set of CxxTypes and writes out code to generate the
 * representations again when executed.  It is assumed that an empty
 * primary template exists when the generated code is compiled:
 *
 *   template <typename> struct CvType;
 *
 * The code generation for a class type "Foo" and a type "Foo*" works like this:
 *  GenerateClasses() produces:
 *   template <> struct CvType< Foo > { static CvQualifiedType type; };
 *   template <> struct CvType< Foo* > { static CvQualifiedType type; };
 *  GenerateDataDeclarations() produces:
 *   CvQualifiedType CvType< Foo >::type; 
 *   CvQualifiedType CvType< Foo* >::type;
 *  GenerateInitalizations() produces:
 *   CvType< Foo >::type = TypeInfo::GetClassType("Foo", false, false);
 *   CvType< Foo* >::type = TypeInfo::GetPointerType(CvType< Foo >::type, false, false);
 */
class GENERATORS_EXPORT CvTypeGenerator
{
public:
  void GenerateClasses(std::ostream&) const;
  void GenerateDataDeclarations(std::ostream&) const;
  void GenerateInitalizations(std::ostream&) const;
  void Add(const cxx::CvQualifiedType&);
private:
  void AddFunctionTypes(const cxx::FunctionType*);  
  void GenerateInitialization(std::ostream&, const cxx::CvQualifiedType&) const;
  void GenerateArgumentAs(std::ostream&, const cxx::CvQualifiedType&) const;
private:
  /**
   * An ordering of the added types which guarantees that any type's
   * "inner" types come before it does.
   */
  typedef std::vector<cxx::CvQualifiedType> TypeOrdering;
  TypeOrdering m_TypeOrdering;
  
  /**
   * The set of types that have been added to m_TypeOrdering.  This allows
   * logarithmic time for checking whether a type has been added.
   */
  std::set<cxx::CvQualifiedType> m_TypesAdded;
};


} // namespace gen

#endif
