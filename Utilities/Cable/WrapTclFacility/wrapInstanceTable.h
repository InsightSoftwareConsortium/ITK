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
