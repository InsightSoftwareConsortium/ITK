/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxPointerType.cxx
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
#include "cxxTypes.h"

namespace _cxx_
{


/**
 * Retrieve what kind of Type this is.
 */
RepresentationType PointerType::GetRepresentationType() const
{
  return PointerType_id;
}


/**
 * Try to cast the given Type to an PointerType.  If this returns, the
 * pointer will be valid.  If the cast is not allowed, an exception is
 * thrown.
 */
PointerType* PointerType::SafeDownCast(Type* t)
{
  PointerType* result = dynamic_cast<PointerType*>(t);
  if(!result) { throw TypeDownCastException(t, PointerType_id); }
  return result;
}


/**
 * Try to cast the given Type to an PointerType.  If this returns, the
 * pointer will be valid.  If the cast is not allowed, an exception is
 * thrown.
 */
const PointerType* PointerType::SafeDownCast(const Type* t)
{
  const PointerType* result = dynamic_cast<const PointerType*>(t);
  if(!result) { throw TypeDownCastException(t, PointerType_id); }
  return result;
}


String PointerType::GenerateName(const String& indirection,
                                 bool isConst, bool isVolatile) const
{
  String cv = this->GetRightCvString(isConst, isVolatile);
  String indirect = "*"+cv;
  if(indirection != "")
    {
    indirect += " "+indirection;
    }
  return m_PointedToType.GenerateName(indirect);
}


/**
 * Get the CvQualifiedType to which this PointerType points.
 */
const CvQualifiedType& PointerType::GetPointedToType() const
{
  return m_PointedToType;
}


/**
 * Constructor takes the cv-qualified type to which the pointer points.
 */
PointerType::PointerType(const CvQualifiedType& in_type):
  m_PointedToType(in_type)
{
}

} // namespace _cxx_
