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

// All wrappers inherit from WrapperBase.
class WrapperBase;

/**
 * A class to maintain a table of the type wrappers that have
 * been registered with its interpreter.
 */
class _wrap_EXPORT WrapperTable
{
public:
  WrapperTable(Tcl_Interp*);
  
  bool Exists(const Type* type) const;
  void SetWrapper(const Type*, WrapperBase*);
  WrapperBase* GetWrapper(const Type*) const;
  
private:
  /**
   * The Tcl interpreter to which this InstanceTable object is attached.
   */
  Tcl_Interp* m_Interpreter;

  typedef std::map<const Type*, WrapperBase*>  WrapperMap;
  /**
   * Map from type to wrapper function.
   */
  WrapperMap m_WrapperMap;
  
public:
  static WrapperTable* GetForInterpreter(Tcl_Interp*);
  
private:
  typedef std::map<const Tcl_Interp*, WrapperTable*>  InterpreterWrapperTableMap;
  static InterpreterWrapperTableMap interpreterWrapperTableMap;
};

} // namespace _wrap_

#endif
