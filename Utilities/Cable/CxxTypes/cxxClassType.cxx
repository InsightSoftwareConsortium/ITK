/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxClassType.cxx
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
#include "cxxClassType.h"

namespace _cxx_
{

  
/**
 * Retrieve what kind of Type this is.
 */
RepresentationType ClassType::GetRepresentationType() const
{
  return ClassType_id;
}


/**
 * Try to cast the given Type to an ClassType.  If this returns, the
 * pointer will be valid.  If the cast is not allowed, an exception is
 * thrown.
 */
ClassType* ClassType::SafeDownCast(Type* t)
{
  ClassType* result = dynamic_cast<ClassType*>(t);
  if(!result) { throw TypeDownCastException(t, ClassType_id); }
  return result;
}


/**
 * Try to cast the given Type to an ClassType.  If this returns, the
 * pointer will be valid.  If the cast is not allowed, an exception is
 * thrown.
 */
const ClassType* ClassType::SafeDownCast(const Type* t)
{
  const ClassType* result = dynamic_cast<const ClassType*>(t);
  if(!result) { throw TypeDownCastException(t, ClassType_id); }
  return result;
}


/**
 * Return whether the class is abstract.
 */
bool ClassType::IsAbstract() const
{
  return m_Abstract;
}


/**
 * Merge the class information given with that already known.  This
 * includes the abstract flag and parent classes.
 */
void ClassType::MergeClassInformation(bool isAbstract,
                                      const ClassTypes& parents)
{
  m_Abstract = isAbstract | m_Abstract;
  if(parents.size() > m_Parents.size())
    {
    m_Parents = parents;
    }
}


/**
 * Get the name of the class.
 */
String ClassType::GetName() const
{
  return m_Name;
}


/**
 * Get a begin iterator to the set of immediate parent classes.
 */
ClassTypes::const_iterator ClassType::ParentsBegin() const
{
  return m_Parents.begin();
}


/**
 * Get an end iterator to the set of immediate parent classes.
 */
ClassTypes::const_iterator ClassType::ParentsEnd() const
{
  return m_Parents.end();
}


/**
 * Find all the superclasses through any inheritance chain.
 */
void ClassType::GetAllSuperclasses(ClassTypeSet& result) const
{
  for(ClassTypes::const_iterator parent = m_Parents.begin();
      parent != m_Parents.end(); ++parent)
    {
    // Insert our immediate parent.
    result.insert(*parent);
    
    // Insert our parent's superclasses.
    (*parent)->GetAllSuperclasses(result);
    }
}


/**
 * Determine whether this ClassType is a subclass of the given ClassType
 * through any chain of inheritance.
 */
bool ClassType::IsSubclassOf(const ClassType* superclass) const
{
  // See if any of our immediate parents are the superclass.
  for(ClassTypes::const_iterator parent = m_Parents.begin();
      parent != m_Parents.end(); ++parent)
    {
    if(*parent == superclass)
      {
      return true;
      }
    }
  
  // Didn't find an immediate parent.  Ask each parent recursively.
  for(ClassTypes::const_iterator parent = m_Parents.begin();
      parent != m_Parents.end(); ++parent)
    {
    if((*parent)->IsSubclassOf(superclass))
      {
      return true;
      }
    }
  
  // We are not a subclass of the given class.
  return false;
}


String ClassType::GenerateName(const String& outerType,
                               bool isConst, bool isVolatile) const
{
  String cv = this->GetLeftCvString(isConst, isVolatile);
  return cv+m_Name+this->PrepareOuterStringForPostfix(outerType);
}


/**
 * Constructor for ClassType just takes the name of the class.  This name
 * may include template parameters of an instantiation.
 */
ClassType::ClassType(const String& name, bool isAbstract,
                     const ClassTypes& parents):
  m_Name(name),
  m_Abstract(isAbstract),
  m_Parents(parents)
{
}


} // namespace _cxx_
