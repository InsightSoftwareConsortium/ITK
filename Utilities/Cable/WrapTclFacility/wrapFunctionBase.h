/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapFunctionBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _wrapFunctionBase_h
#define _wrapFunctionBase_h

#include "wrapUtils.h"

namespace _wrap_
{

/**
 * Base class for all method wrappers.
 */
class _wrap_EXPORT FunctionBase
{
public:
  typedef std::vector<const Type*> ParameterTypes;
  FunctionBase(const String& name,
               const ParameterTypes& parameterTypes);
  virtual ~FunctionBase();

  const String& GetName() const;
  virtual String GetPrototype() const =0;
  unsigned long GetNumberOfParameters() const;
  const ParameterTypes& GetParameterTypes() const;
  ParameterTypes::const_iterator ParametersBegin() const;
  ParameterTypes::const_iterator ParametersEnd() const;
  
  /**
   * This is called by the overload resolution algorithm when this
   * method wrapper has been selected.  It must be defined by
   * a subclass to actually call a wrapped method.
   */
  virtual void Call(int objc, Tcl_Obj*CONST objv[]) const =0;
protected:
  /**
   * The name of the method.
   */
  String m_Name;
  
  /**
   * The parameter types of the method.  These may be needed for
   * overload resolution.
   */
  ParameterTypes m_ParameterTypes;
};

} // namespace _wrap_

#endif
