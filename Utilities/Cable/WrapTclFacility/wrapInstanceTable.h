/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapInstanceTable.h
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
#ifndef _wrapInstanceTable_h
#define _wrapInstanceTable_h

#include "wrapUtils.h"
#include "wrapReference.h"
#include <map>

namespace _wrap_
{

/**
 * A class to maintain a table of object instances for a Tcl interpreter.
 */
class _wrap_EXPORT InstanceTable
{
public:
  /**
   * The type of a function that deletes an object.
   */
  typedef void (*DeleteFunction)(const void*);
  
  InstanceTable(Tcl_Interp*);
  
  void SetObject(const String& name, void* object, const CvQualifiedType&);
  void DeleteObject(const String& name);
  bool Exists(const String& name) const;
  const Reference& GetEntry(const String& name) const;
  void* GetObject(const String& name);
  const CvQualifiedType& GetType(const String& name);
  String CreateTemporary(void* object, const CvQualifiedType&);
  void DeleteIfTemporary(const String& name);
  void DeleteCallBack(void* object);
  void SetDeleteFunction(const Type*, DeleteFunction);
  
private:
  /**
   * The Tcl interpreter to which this InstanceTable object is attached.
   */
  Tcl_Interp* m_Interpreter;
  
  /**
   * Map from instance name to a Reference referring to it.
   */
  typedef std::map<String, Reference> InstanceMap;
  
  /**
   * Store the mappings internally.
   */
  InstanceMap m_InstanceMap;
  
  /**
   * Map from type to delete function.
   */
  typedef std::map<const Type*, DeleteFunction>  DeleteFunctionMap;
  DeleteFunctionMap m_DeleteFunctionMap;
  
  typedef std::map<const void*, String, VoidPointerCompare>  AddressToNameMap;
  
  /** 
   * Map from object address to object name.
   * Used for delete call-back implementation.
   */
  AddressToNameMap m_AddressToNameMap;

  /**
   * Counter to create unique temporary object names.
   */
  unsigned int m_TempNameNumber;
  
  void CheckExists(const String& name) const;
  
public:
  static InstanceTable* GetForInterpreter(Tcl_Interp*);
  
private:
  typedef std::map<const Tcl_Interp*, InstanceTable*>  InterpreterInstanceTableMap;
  static InterpreterInstanceTableMap interpreterInstanceTableMap;
};


/**
 * A function to delete an object of any type.
 * Specializations can be created for any object that requires something
 * fancy to delete.
 */
template <typename T>
struct OldObjectOf
{
  /**
   * Method to delete the given object.
   */
  static void Delete(const void* object)
    {
    // Use delete operator by default.
    delete const_cast<T*>(static_cast<const T*>(object));
    }
};


} // namespace _wrap_

#endif
