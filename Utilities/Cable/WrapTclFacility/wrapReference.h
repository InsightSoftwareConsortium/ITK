/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapReference.h
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
#ifndef _wrapReference_h
#define _wrapReference_h

#include "wrapUtils.h"

namespace _wrap_
{

/** \class Reference
 * Represent a reference with its type.
 */
class Reference
{
public:
  Reference(): m_Object(NULL), m_Type(NULL) {}
  Reference(const Reference& r):
    m_Object(r.m_Object), m_Type(r.m_Type) {}
  Reference(const void* object, const CvQualifiedType& type):
    m_Object(const_cast<void*>(object)), m_Type(type) {}
  
  /**
   * Get a pointer to the object.
   */
  void* GetObject() const { return m_Object; }
  
  /**
   * Get the type of the object.
   */
  const CvQualifiedType& GetReferencedType() const { return m_Type; }

  String GetStringRep() const;
  bool SetFromStringRep(const String&);
  
private:
  /**
   * The reference to the object.
   */
  void* m_Object;
  
  /**
   * The type of the object.
   */
  CvQualifiedType m_Type;
};


// Standard Tcl interface for its object types.
// This one is for the Reference object.
_wrap_EXPORT int Tcl_GetReferenceFromObj(Tcl_Interp*, Tcl_Obj*, Reference*);
_wrap_EXPORT void Tcl_SetReferenceObj(Tcl_Obj*, const Reference&);
_wrap_EXPORT Tcl_Obj* Tcl_NewReferenceObj(const Reference&);

// A couple useful utility functions for the type.
_wrap_EXPORT bool TclObjectTypeIsReference(Tcl_Obj*);
_wrap_EXPORT bool StringRepIsReference(const String&);
  
} // namespace _wrap_

#endif
