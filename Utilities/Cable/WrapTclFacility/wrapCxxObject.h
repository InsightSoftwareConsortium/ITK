/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapCxxObject.h
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
#ifndef _wrapCxxObject_h
#define _wrapCxxObject_h

#include "wrapUtils.h"
#include "wrapAnything.h"

namespace _wrap_
{

class WrapperFacility;

/**
 * An instance of this class is used to refer to a C++ object from one
 * or more Tcl objects.  It maintains a reference count of the number
 * of Tcl objects refering to it in their internal representations.
 * Such Tcl objects are of the type "CxxObject", which is defined in
 * "wrapTclCxxObject.h".
 *
 * When an instance is created, it automatically tries to create a Tcl
 * command to allow methods to be called on the referenced object.
 * When it is destroyed, the command is removed.  Also, if the
 * WrapperFacility knows how to delete the object, it will be deleted
 * when this instance is destroyed.
 */
class _wrap_EXPORT CxxObject
{
public:
  typedef CxxObject Self;
  
  void Delete() const;
  
  void* GetObject() const;
  const Type* GetType() const;
  Tcl_Interp* GetInterpreter() const;
  const WrapperFacility* GetWrapperFacility() const;
  int GetReferenceCount() const;
  void Increment();
  void Decrement();
  
  String GetStringRepresentation() const;
  static CxxObject* GetFromStringRepresentation(const char*);  
  
private:
  // Make sure no one creates or destroys an instance directly.
  CxxObject(const Anything&, const Type*, const WrapperFacility*);
  ~CxxObject();
  
  void CreateTclCommand() const;
  void DeleteTclCommand() const;
  void CleanupObject() const;
  
  // Leave these unimplemented so that no one can copy an instance.
  CxxObject(const CxxObject&);
  void operator=(const CxxObject&);
  
  ///! The a pointer to the object or function.
  Anything m_Anything;
  
  ///! The type of the object.  Top level cv-qualifiers are never stored.
  const Type* m_Type;

  ///! The WrapperFacility instance for which the object has been created.
  const WrapperFacility* m_WrapperFacility;
  
  ///! Count the number of Object instances referencing this.
  int m_ReferenceCount;
  
  // Make sure that a WrapperFacility can create instances of
  // CxxObject.
  friend class WrapperFacility;
};

} // namespace _wrap_

#endif
