/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapInstanceTable.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _wrapInstances_h
#define _wrapInstances_h

#include "wrapUtils.h"
#include "wrapReference.h"
#include <map>

namespace _wrap_
{

/**
 * A class to maintain a table of object instances for a Tcl interpreter.
 */
class _wrap_EXPORT Instances
{
public:
  /**
   * The type of a function that deletes an object.
   */
  typedef void (*DeleteFunction)(void*);
  
  Instances(Tcl_Interp*);
  
  void SetObject(const String& name, void* object, const CvQualifiedType&);
  void DeleteObject(const String& name);
  bool Exists(const String& name) const;
  void* GetObject(const String& name);
  const CvQualifiedType& GetType(const String& name);
  String CreateTemporary(void* object, const CvQualifiedType&);
  void DeleteIfTemporary(const String& name);
  void DeleteCallBack(void* object);
  void RegisterDeleteFunction(const Type*, DeleteFunction);
  
private:

  /**
   * The Tcl interpreter to which this Instances object is attached.
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
  
  typedef std::map<const void*, String,
                   PointerCompare<const void> >  AddressToNameMap;
  
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
  static Instances* GetInterpreterInstances(Tcl_Interp*);
  
private:
  typedef std::map<const Tcl_Interp*, Instances*,
                   PointerCompare<const Tcl_Interp> >  InterpreterInstancesMap;
  static InterpreterInstancesMap interpreterInstancesMap;
};


/**
 * A function to create an object of any type.
 * Specializations can be created for any object that requires something
 * fancy to create.
 */
template <typename T>
struct NewObjectOf
{
  static T* Create(void)
    {
      return new T;
    }
};


/**
 * A function to delete an object of any type.
 * Specializations can be created for any object that requires something
 * fancy to delete.
 */
template <typename T>
struct OldObjectOf
{
  static void Delete(void* object)
    {
      delete static_cast<T*>(object);
    }
};

} // namespace _wrap_

#endif
