/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapTypeSystemTable.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "wrapTypeSystemTable.h"

namespace _wrap_
{


/**
 * Get the TypeSystem object for the given Tcl interpreter.
 */
TypeSystem* TypeSystemTable::GetForInterpreter(Tcl_Interp* interp)
{
  // See if a TypeSystem exists for the given interpreter.
  if(interpreterTypeSystemMap.count(interp) == 0)
    {
    // No, we must create a new TypeSystem for this interpreter.
    interpreterTypeSystemMap[interp] = new TypeSystem;
    }
  
  // Return the TypeSystem.
  return interpreterTypeSystemMap[interp];  
}


/**
 * Map from a Tcl interpreter to the TypeSystem for it.
 */
TypeSystemTable::InterpreterTypeSystemMap TypeSystemTable::interpreterTypeSystemMap;

} // namespace _wrap_
