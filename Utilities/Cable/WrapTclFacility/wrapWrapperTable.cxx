/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapWrapperTable.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "wrapWrapperTable.h"
#include "wrapException.h"

namespace _wrap_
{

/**
 * Constructor takes interpreter to which the WrapperTable will be
 * attached.
 */
WrapperTable::WrapperTable(Tcl_Interp* interp):
  m_Interpreter(interp)
{
}


/**
 * Return whether a wrapper for the given type name exists.
 */
bool WrapperTable::Exists(const Type* type) const
{
  return (m_WrapperMap.count(type) > 0);
}
  

/**
 * Register a wrapper function for the given type.
 */
void WrapperTable::SetFunction(const Type* type, WrapperFunction func)
{
  m_WrapperMap[type] = func;
}
  
 
/**
 * Retrieve the wrapper function for the given type.
 */
WrapperTable::WrapperFunction
WrapperTable::GetFunction(const Type* type)
{
  this->CheckExists(type);
  return m_WrapperMap[type];
}


/**
 * Make sure a wrapper for the given type exists.  Throw an exception
 * if it does not.
 */
void WrapperTable::CheckExists(const Type* type) const
{
  if(!this->Exists(type))
    {
    // throw _wrap_NoWrapperFunctionException(type);
    throw _wrap_NoWrapperFunctionException("");
    }
}
 

/**
 * Get an WrapperTable object set up to deal with the given Tcl interpreter.
 * If one exists, it will be returned.  Otherwise, a new one will be
 * created.
 */
WrapperTable* WrapperTable::GetForInterpreter(Tcl_Interp* interp)
{
  // See if an WrapperTable exists for the given interpreter.
  if(interpreterWrapperTableMap.count(interp) == 0)
    {
    // No, we must create a new WrapperTable for this interpreter.
    interpreterWrapperTableMap[interp] = new WrapperTable(interp);
    }
  
  // Return the WrapperTable.
  return interpreterWrapperTableMap[interp];  
}


/**
 * Map from a Tcl interpreter to the WrapperTable for it.
 */
WrapperTable::InterpreterWrapperTableMap WrapperTable::interpreterWrapperTableMap;

} // namespace _wrap_
