/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapCxxObject.cxx
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
#include "wrapCxxObject.h"
#include "wrapWrapperFacility.h"
#include "wrapWrapperBase.h"

#include <stdio.h>
#include <map>

namespace _wrap_
{

/**
 * There is one instance of this class for each WrapperFacility.  It
 * maps from an object or function pointer value and its corresponding
 * type to the CxxObject instance for it.
 */
class CxxObject::CxxObjectMap
{
public:
  CxxObjectMap(const WrapperFacility*);
  ~CxxObjectMap();
  
  CxxObject* GetObjectFor(const Anything& anything, const Type* type);
  void DeleteObjectFor(const Anything& anything, const Type* type);
  
private:
  ///! The WrapperFacility corresponding to CxxObject instance in this map.
  const WrapperFacility* m_WrapperFacility;
  
  typedef std::pair<Anything::ObjectType, const Type*> ObjectMapKey;
  typedef std::map<ObjectMapKey, CxxObject*> ObjectMap;
  typedef Anything::FunctionType FunctionMapKey;
  typedef std::map<FunctionMapKey, CxxObject*> FunctionMap;
  
  ///! Map from object location and type to the CxxObject instance for it.
  ObjectMap m_ObjectMap;
  
  ///! Map from function pointer value to CxxObject instance for it.
  FunctionMap m_FunctionMap;
};


/**
 * The constructor just sets up the WrapperFacility pointer.
 */
CxxObject::CxxObjectMap::CxxObjectMap(const WrapperFacility* wrapperFacility):
  m_WrapperFacility(wrapperFacility)
{
}


/**
 * The destructor cleans up any leftover CxxObject instances.  This
 * will delete any object of class type referred to by a CxxObject.
 */
CxxObject::CxxObjectMap::~CxxObjectMap()
{
  // Loop over the maps and explicitly delete every instance left.
  // Do not call CxxObject::Delete because that will try to remove its
  // instance of from this map!
  for(FunctionMap::const_iterator f = m_FunctionMap.begin();
      f != m_FunctionMap.end(); ++f)
    {
    delete f->second;
    }
  for(ObjectMap::const_iterator o = m_ObjectMap.begin();
      o != m_ObjectMap.end(); ++o)
    {
    delete o->second;
    }
}


/**
 * Get an instance of CxxObject corresponding to the given value and
 * type, as well as the WrapperFacility for this CxxObjectMap.
 */
CxxObject* CxxObject::CxxObjectMap::GetObjectFor(const Anything& anything,
                                                 const Type* type)
{
  if(type->IsFunctionType())
    {
    FunctionMapKey key = anything.function;
    FunctionMap::const_iterator f = m_FunctionMap.find(key);
    if(f != m_FunctionMap.end())
      {
      return f->second;
      }
    else
      {
      CxxObject* cxxObject = new CxxObject(anything, type, m_WrapperFacility);
      m_FunctionMap.insert(FunctionMap::value_type(key, cxxObject));
      return cxxObject;
      }
    }
  else
    {
    ObjectMapKey key(anything.object, type);
    ObjectMap::const_iterator o = m_ObjectMap.find(key);
    if(o != m_ObjectMap.end())
      {
      return o->second;
      }
    else
      {
      CxxObject* cxxObject = new CxxObject(anything, type, m_WrapperFacility);
      m_ObjectMap.insert(ObjectMap::value_type(key, cxxObject));
      return cxxObject;
      }
    }
}


/**
 * Find the CxxObject corresponding to the input and delete it.  This
 * also removes it from the map.
 */
void CxxObject::CxxObjectMap::DeleteObjectFor(const Anything& anything,
                                              const Type* type)
{  
  if(type->IsFunctionType())
    {
    FunctionMapKey key = anything.function;
    FunctionMap::const_iterator f = m_FunctionMap.find(key);
    if(f != m_FunctionMap.end())
      {
      delete f->second;
      m_FunctionMap.erase(key);
      }
    }
  else
    {
    ObjectMapKey key(anything.object, type);
    ObjectMap::const_iterator o = m_ObjectMap.find(key);
    if(o != m_ObjectMap.end())
      {
      delete o->second;
      m_ObjectMap.erase(key);
      }
    }
}


/**
 * Get an instance of CxxObject for the given value, type, and
 * WrapperFacility.  This is used instead of a constructor so that if
 * such an instance already exists, it can be returned instead.
 */
CxxObject* CxxObject::GetObjectFor(const Anything& anything,
                                   const Type* type,
                                   const WrapperFacility* wrapperFacility)
{
  return Self::GetCxxObjectMapFor(wrapperFacility)
    ->GetObjectFor(anything, type);
}


/**
 * Delete the instance of CxxObject on which this method is called.
 * This will clean up the object's Tcl command, if any, as well as delete
 * the object referenced if it is of C++ class type.
 */
void CxxObject::Delete() const
{
  // Ask the map of all instances to delete this object.
  Self::DeleteObjectFor(m_Anything, m_Type, m_WrapperFacility);
}


/**
 * Get a pointer to the C++ object.
 */
void* CxxObject::GetObject() const
{
  return m_Anything.object;
}
  

/**
 * Get the type represented by this CxxObject.
 */
const Type* CxxObject::GetType() const
{
  return m_Type;
}


/**
 * Get the interpreter to which this CxxObject is attached.
 */
Tcl_Interp* CxxObject::GetInterpreter() const
{
  return m_WrapperFacility->GetInterpreter();
}


/**
 * Get the WrapperFacility to which this CxxObject is attached.
 */
const WrapperFacility* CxxObject::GetWrapperFacility() const
{
  return m_WrapperFacility;
}


/**
 * Get the number of Tcl objects currently referencing this CxxObject
 * in their binary representations.  Note that this being zero does
 * not necessarily mean the object should be deleted, because a Tcl
 * object in string form may still refer to it.
 */
int CxxObject::GetReferenceCount() const
{
  return m_ReferenceCount;
}


/**
 * Increment the number of Tcl object currently referencing this
 * CxxObject in their binary representations.
 */
void CxxObject::Increment()
{
  ++m_ReferenceCount;
  _wrap_DEBUG_OUTPUT(this->GetWrapperFacility(),
                     "Incrementing references to CxxObject at " << this
                     << " to " << m_ReferenceCount << std::endl);
}


/**
 * Decrement the number of Tcl object currently referencing this
 * CxxObject in their binary representations.
 */
void CxxObject::Decrement()
{
  --m_ReferenceCount;
  _wrap_DEBUG_OUTPUT(this->GetWrapperFacility(),
                     "Decrementing references to CxxObject at " << this
                     << " to " << m_ReferenceCount << std::endl);
}

  
/**
 * Return the string representation for this CxxObject instance.
 * Passing the resulting string to
 * CxxObject::GetFromStringRepresentation() should result in a pointer
 * to this instance.
 */
String CxxObject::GetStringRepresentation() const
{
  char addrBuf[sizeof(CxxObject*)*2+7];
  sprintf(addrBuf, "_cxx%p", this);
  return addrBuf;
}


/**
 * Attempt to convert the given string representation into a pointer
 * to an instance of CxxObject.
 */
CxxObject* CxxObject::GetFromStringRepresentation(const char* stringRep)
{
  CxxObject* cxxObject = 0;
  sscanf(stringRep, "_cxx%p", &cxxObject);
  return cxxObject;
}

// private implementation methods:


/**
 * Constructor creates the Tcl command to allow methods to be called
 * on an object if its type is wrapped.
 */
CxxObject::CxxObject(const Anything& anything,
                     const Type* type,
                     const WrapperFacility* wrapperFacility):
  m_Anything(anything),
  m_Type(type),
  m_WrapperFacility(wrapperFacility),
  m_ReferenceCount(0)
{
  _wrap_DEBUG_OUTPUT(this->GetWrapperFacility(),
                     "Creating CxxObject at " << this <<
                     " with type " << m_Type->Name().c_str() << std::endl);
  this->CreateTclCommand();
}


/**
 * Destructor removes the Tcl command and cleans up.
 */
CxxObject::~CxxObject()
{
  _wrap_DEBUG_OUTPUT(this->GetWrapperFacility(),
                     "Deleting CxxObject at " << this <<
                     " with type " << m_Type->Name().c_str() << std::endl);
  this->DeleteTclCommand();
  this->CleanupObject();  
}


/**
 * Create the Tcl command corresponding to this CxxObject.
 */
void CxxObject::CreateTclCommand() const
{
  // Walk through a pointer or reference type to its target.  This
  // allows methods to be called through pointers or references to an
  // object.
  const Type* targetType = m_Type;
  if(m_Type->IsPointerType())
    {
    targetType = PointerType::SafeDownCast(m_Type)
      ->GetPointedToType().GetType();
    }
  else if(m_Type->IsReferenceType())
    {
    targetType = ReferenceType::SafeDownCast(m_Type)
      ->GetReferencedType().GetType();
    }
  
  // If the type is a class type, it may be wrapped.
  if(targetType->IsClassType())
    {
    // Try to get the wrapper for the given type.
    WrapperBase* wrapper = m_WrapperFacility->GetWrapper(targetType);
    if(wrapper)
      {
      char addrBuf[sizeof(CxxObject*)*2+7];
      sprintf(addrBuf, "_cxx%p", this);
      
      // Found a wrapper.  Create the command to use it.
      Tcl_CreateObjCommand(m_WrapperFacility->GetInterpreter(), addrBuf,
                           wrapper->GetObjectWrapperFunction(), wrapper, 0);
      }
    else
      {
      // TODO: Create a dummy wrapper that reports that the wrapper
      // for the type has not been loaded.
      }
    }
}


/**
 * Delete the Tcl command corresponding to this CxxObject.
 */
void CxxObject::DeleteTclCommand() const
{
  String cmdName = this->GetStringRepresentation();
  Tcl_DeleteCommand(m_WrapperFacility->GetInterpreter(),
                    const_cast<char*>(cmdName.c_str()));
}


/**
 * If this CxxObject refers to an instance of a C++ class type, try to
 * delete it.
 */
void CxxObject::CleanupObject() const
{
  if(m_Type->IsClassType())
    {
    m_WrapperFacility->DeleteObject(this->GetObject(), m_Type);
    }
}


typedef std::map<const WrapperFacility*,
                 CxxObject::CxxObjectMap*>  FacilityToObjectMap;

/**
 * There is one instance of the FacilityToObjectMap for CxxObject.
 * This is it.
 */
namespace
{
FacilityToObjectMap facilityToObjectMap;
}


/**
 * Get the CxxObjectMap instance for the given WrapperFacility.  If
 * one does not exist, it will be created.
 */
CxxObject::CxxObjectMap*
CxxObject::GetCxxObjectMapFor(const WrapperFacility* wrapperFacility)
{
  FacilityToObjectMap::const_iterator m =
    facilityToObjectMap.find(wrapperFacility);
  if(m != facilityToObjectMap.end())
    {
    return m->second;
    }
  else
    {
    CxxObjectMap* cxxObjectMap = new CxxObjectMap(wrapperFacility);
    facilityToObjectMap.insert(
      FacilityToObjectMap::value_type(wrapperFacility, cxxObjectMap));
    return cxxObjectMap;
    }
}


/**
 * Find the CxxObject instance corresponding to the input, remove it
 * from the map of all instances, and delete it.
 */
void CxxObject::DeleteObjectFor(const Anything& anything, const Type* type,
                                const WrapperFacility* wrapperFacility)
{  
  Self::GetCxxObjectMapFor(wrapperFacility)->DeleteObjectFor(anything, type);
}


/**
 * This function is called when the program ends or the shared library
 * is unloaded.  It will destroy all the remaining CxxObject
 * instances.
 */
void CxxObject::ClassFinalize()
{
  // Delete all the CxxObjectMap instances.  They will delete their
  // own set of CxxObject instances.
  for(FacilityToObjectMap::const_iterator m = facilityToObjectMap.begin();
      m != facilityToObjectMap.end(); ++m)
    {
    delete m->second;
    }
}
  
} // namespace _wrap_
