/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxClassType.h
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
#ifndef _cxxClassType_h
#define _cxxClassType_h

#include "cxxCvQualifiedType.h"

#include <vector>
#include <set>

namespace _cxx_
{
class _cxx_EXPORT ClassType;

/**
 * A vector of ClassType pointers.
 */
typedef std::vector<const ClassType*> ClassTypes;

/**
 * A set of ClassType pointers.
 */
typedef std::set<const ClassType*> ClassTypeSet;

/**
 * Represents a C++ class type.  This could have been produced by a
 * class, struct, union, template full specialization, or template
 * instantiation.
 */
class ClassType: public Type
{
public:
  typedef ClassType Self;
  
  virtual RepresentationType GetRepresentationType() const;
  static ClassType* SafeDownCast(Type*);
  static const ClassType* SafeDownCast(const Type*);

  bool IsAbstract() const;
  
  String GetName() const;
  ClassTypes::const_iterator ParentsBegin() const;
  ClassTypes::const_iterator ParentsEnd() const;
  ClassTypeSet GetAllSuperclasses() const;
  bool IsSubclassOf(const ClassType*) const;
  
  virtual String GenerateName(const String&, bool, bool) const;
protected:
  ClassType(const String&, bool isAbstract, const ClassTypes&);
  ClassType(const Self&) {}
  void operator=(const Self&) {}
  virtual ~ClassType() {}
  
private:  
  /**
   * The name of the class.
   */
  String m_Name;
  
  /**
   * Flag for whether the class is abstract.
   */
  bool m_Abstract;
  
  /**
   * The immediate public superclasses of this class.
   * A pointer or reference ot this class can be cast up to these
   * types.
   */
  ClassTypes m_Parents;
  
  /**
   * The list of types from which this class can construct.
   */
  CvQualifiedTypes m_ConversionByConstructor;
  
  /**
   * The list of types to which this class can convert by type conversion
   * operator.
   */
  CvQualifiedTypes m_ConversionOperators;
  
  friend class TypeSystem;
};


} // namespace _cxx_


#endif
