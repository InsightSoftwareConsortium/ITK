/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapTypeSystemTable.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _wrapTypeSystemTable_h
#define _wrapTypeSystemTable_h

#include "wrapUtils.h"
#include <map>

namespace _wrap_
{  

/**
 * A class to maintain a mapping of interpreter to its TypeSystem.
 */
class _wrap_EXPORT TypeSystemTable
{
public:
  static TypeSystem* GetForInterpreter(Tcl_Interp*);
  
private:
  typedef std::map<const Tcl_Interp*, TypeSystem*>  InterpreterTypeSystemMap;
  static InterpreterTypeSystemMap interpreterTypeSystemMap;
};

} // namespace _wrap_

#endif
