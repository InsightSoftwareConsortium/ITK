/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapCalls.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "wrapCalls.h"

#include <string.h>

namespace _wrap_
{

void Return<bool>::From(bool result, WrapperBase* wrapper)
{
  int boolValue = result;
  Tcl_SetBooleanObj(Tcl_GetObjResult(wrapper->GetInterpreter()),
                    boolValue);
}

void Return<short>::From(short result, WrapperBase* wrapper)
{
  int intValue = result;
  Tcl_SetIntObj(Tcl_GetObjResult(wrapper->GetInterpreter()), intValue);
}

void Return<unsigned short>::From(unsigned short result, WrapperBase* wrapper)
{
  int intValue = result;
  Tcl_SetIntObj(Tcl_GetObjResult(wrapper->GetInterpreter()), intValue);
}

void Return<int>::From(int result, WrapperBase* wrapper)
{
  Tcl_SetIntObj(Tcl_GetObjResult(wrapper->GetInterpreter()), result);
}

void Return<unsigned int>::From(unsigned int result, WrapperBase* wrapper)
{
  int intValue = result;
  Tcl_SetIntObj(Tcl_GetObjResult(wrapper->GetInterpreter()), intValue);
}

void Return<long>::From(long result, WrapperBase* wrapper)
{
  Tcl_SetLongObj(Tcl_GetObjResult(wrapper->GetInterpreter()), result);
}

void Return<unsigned long>::From(unsigned long result, WrapperBase* wrapper)
{
  long longValue = result;
  Tcl_SetLongObj(Tcl_GetObjResult(wrapper->GetInterpreter()), longValue);
}

void Return<float>::From(float result, WrapperBase* wrapper)
{
  double doubleValue = result;
  Tcl_SetDoubleObj(Tcl_GetObjResult(wrapper->GetInterpreter()), doubleValue);
}  

void Return<double>::From(double result, WrapperBase* wrapper)
{
  Tcl_SetDoubleObj(Tcl_GetObjResult(wrapper->GetInterpreter()), result);
}
  

void ReturnPointerTo<char>::From(char* result, WrapperBase* wrapper)
{
  Tcl_SetStringObj(Tcl_GetObjResult(wrapper->GetInterpreter()), result, -1);
}

void ReturnPointerTo<const char>::From(const char* result,
                                       WrapperBase* wrapper)
{
  Tcl_SetStringObj(Tcl_GetObjResult(wrapper->GetInterpreter()),
                   const_cast<char*>(result), -1);
}

} // namespace _wrap_
