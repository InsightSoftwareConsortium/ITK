/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapWrapperTable.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _wrapWrapperTable_h
#define _wrapWrapperTable_h

#include "wrapUtils.h"
#include <map>

namespace _wrap_
{  

/**
 * A class to maintain a table of the type wrapper functions that have
 * been registered with its interpreter.
 */
class _wrap_EXPORT WrapperTable
{
public:
  /**
   * The type of a wrapper function.
   */
  typedef int (*WrapperFunction)(ClientData, Tcl_Interp*, int, Tcl_Obj* CONST[]);
  
  WrapperTable(Tcl_Interp*);
  
  bool Exists(const Type*) const;
  void SetFunction(const Type*, WrapperFunction);
  WrapperFunction GetFunction(const Type*);
  
private:
  /**
   * The Tcl interpreter to which this InstanceTable object is attached.
   */
  Tcl_Interp* m_Interpreter;

  typedef std::map<const Type*, WrapperFunction,
                   PointerCompare<const Type> >  WrapperMap;
  /**
   * Map from type to wrapper function.
   */
  WrapperMap m_WrapperMap;
  
  void CheckExists(const Type*) const;
  
public:
  static WrapperTable* GetInterpreterWrapperTable(Tcl_Interp*);
  
private:
  typedef std::map<const Tcl_Interp*, WrapperTable*,
                   PointerCompare<const Tcl_Interp> >  InterpreterWrapperTableMap;
  static InterpreterWrapperTableMap interpreterWrapperTableMap;
};

} // namespace _wrap_

#endif
