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
 * Return whether a wrapper for the given type exists.
 */
bool WrapperTable::Exists(const Type* type) const
{
  return (m_WrapperMap.count(type) > 0);
}
  

/**
 * Register a wrapper for the given type.
 */
void WrapperTable::SetWrapper(const Type* type, WrapperBase* wrapper)
{
  m_WrapperMap[type] = wrapper;
}
  
 
/**
 * Retrieve the wrapper for the given type.  If none exists, NULL is
 * returned.
 */
WrapperBase*
WrapperTable::GetWrapper(const Type* type) const
{
  WrapperMap::const_iterator i = m_WrapperMap.find(type);
  if(i != m_WrapperMap.end())
    {
    return i->second;
    }
  return NULL;
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
