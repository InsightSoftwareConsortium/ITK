/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxTypedefType.cxx
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
#include "cxxTypedefType.h"

namespace _cxx_
{


/**
 * Retrieve what kind of Type this is.
 * This is a special version for the TypedefType, because it passes the
 * call through to the underlying type.
 */
RepresentationType
TypedefType
::GetRepresentationType() const
{
  return m_CvQualifiedType.GetType()->GetRepresentationType();
}


/**
 * Test whether this is a TypedefType.  This is a special test because
 * TypedefType's GetRepresentationType() passes the call through to the
 * real type.  This call is not passed through.
 */
bool
TypedefType
::IsTypedefType() const
{
  return true;
}


/**
 * Return the pointer that is used to identify this type.
 * This must pass through to the real type.
 */
const Type* TypedefType::Id() const
{
  return m_CvQualifiedType.GetType()->Id();
}


/**
 * Given cv-qualifiers, construct the CvQualifiedType referring to
 * this type.  The result may be more cv-qualified than that given if
 * this is a typedef type.
 */
CvQualifiedType
TypedefType
::GetCvQualifiedType(bool isConst, bool isVolatile) const
{
  // This is a TypedefType, but it may refer to a TypedefType as well.
  // Ask the referred-to-type to construct the CvQualifiedType, but with
  // the addition of all qualifiers specified by this typedef.
  return m_CvQualifiedType.GetType()
    ->GetCvQualifiedType(isConst || m_CvQualifiedType.IsConst(),
                         isVolatile || m_CvQualifiedType.IsVolatile());
}


/**
 * Get the name of the type.  This call is not passed through to the
 * real type because we want the typedef name.
 */
String TypedefType::GenerateName(const String& indirection,
                                 bool isConst, bool isVolatile) const
{
  String cv = this->GetLeftCvString(isConst, isVolatile);
  return cv+m_Name+indirection;
}


/**
 * Constructor takes the cv-qualified type that is referenced.
 */
TypedefType
::TypedefType(const String& name, const CvQualifiedType& in_type):
  m_Name(name),
  m_CvQualifiedType(in_type)
{
}
  


} // namespace _cxx_
