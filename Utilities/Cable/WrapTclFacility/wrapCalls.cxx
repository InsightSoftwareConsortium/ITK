/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapCalls.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#include "wrapCalls.h"

#include <string.h>

namespace _wrap_
{

void Return<void>::From(const WrapperBase* wrapper)
{
  Tcl_SetStringObj(Tcl_GetObjResult(wrapper->GetInterpreter()),
                   "", -1);
}

void Return<bool>::From(bool result, const WrapperBase* wrapper)
{
  int boolValue = result;
  Tcl_SetBooleanObj(Tcl_GetObjResult(wrapper->GetInterpreter()),
                    boolValue);
}

void Return<short>::From(short result, const WrapperBase* wrapper)
{
  int intValue = result;
  Tcl_SetIntObj(Tcl_GetObjResult(wrapper->GetInterpreter()), intValue);
}

void Return<unsigned short>::From(unsigned short result, const WrapperBase* wrapper)
{
  int intValue = result;
  Tcl_SetIntObj(Tcl_GetObjResult(wrapper->GetInterpreter()), intValue);
}

void Return<int>::From(int result, const WrapperBase* wrapper)
{
  Tcl_SetIntObj(Tcl_GetObjResult(wrapper->GetInterpreter()), result);
}

void Return<unsigned int>::From(unsigned int result, const WrapperBase* wrapper)
{
  int intValue = result;
  Tcl_SetIntObj(Tcl_GetObjResult(wrapper->GetInterpreter()), intValue);
}

void Return<long>::From(long result, const WrapperBase* wrapper)
{
  Tcl_SetLongObj(Tcl_GetObjResult(wrapper->GetInterpreter()), result);
}

void Return<unsigned long>::From(unsigned long result, const WrapperBase* wrapper)
{
  long longValue = result;
  Tcl_SetLongObj(Tcl_GetObjResult(wrapper->GetInterpreter()), longValue);
}

void Return<float>::From(float result, const WrapperBase* wrapper)
{
  double doubleValue = result;
  Tcl_SetDoubleObj(Tcl_GetObjResult(wrapper->GetInterpreter()), doubleValue);
}  

void Return<double>::From(double result, const WrapperBase* wrapper)
{
  Tcl_SetDoubleObj(Tcl_GetObjResult(wrapper->GetInterpreter()), result);
}
  

void ReturnPointerTo<char>::From(char* result, const WrapperBase* wrapper)
{
  Tcl_SetStringObj(Tcl_GetObjResult(wrapper->GetInterpreter()), result, -1);
}

void ReturnPointerTo<const char>::From(const char* result,
                                       const WrapperBase* wrapper)
{
  Tcl_SetStringObj(Tcl_GetObjResult(wrapper->GetInterpreter()),
                   const_cast<char*>(result), -1);
}

void ReturnReferenceTo<const bool>::From(const bool& result, const WrapperBase* wrapper)
{
  int boolValue = result;
  Tcl_SetBooleanObj(Tcl_GetObjResult(wrapper->GetInterpreter()),
                    boolValue);
}

void ReturnReferenceTo<const short>::From(const short& result, const WrapperBase* wrapper)
{
  int intValue = result;
  Tcl_SetIntObj(Tcl_GetObjResult(wrapper->GetInterpreter()), intValue);
}

void ReturnReferenceTo<const unsigned short>::From(const unsigned short& result, const WrapperBase* wrapper)
{
  int intValue = result;
  Tcl_SetIntObj(Tcl_GetObjResult(wrapper->GetInterpreter()), intValue);
}

void ReturnReferenceTo<const int>::From(const int& result, const WrapperBase* wrapper)
{
  Tcl_SetIntObj(Tcl_GetObjResult(wrapper->GetInterpreter()), result);
}

void ReturnReferenceTo<const unsigned int>::From(const unsigned int& result, const WrapperBase* wrapper)
{
  int intValue = result;
  Tcl_SetIntObj(Tcl_GetObjResult(wrapper->GetInterpreter()), intValue);
}

void ReturnReferenceTo<const long>::From(const long& result, const WrapperBase* wrapper)
{
  Tcl_SetLongObj(Tcl_GetObjResult(wrapper->GetInterpreter()), result);
}

void ReturnReferenceTo<const unsigned long>::From(const unsigned long& result, const WrapperBase* wrapper)
{
  long longValue = result;
  Tcl_SetLongObj(Tcl_GetObjResult(wrapper->GetInterpreter()), longValue);
}

void ReturnReferenceTo<const float>::From(const float& result, const WrapperBase* wrapper)
{
  double doubleValue = result;
  Tcl_SetDoubleObj(Tcl_GetObjResult(wrapper->GetInterpreter()), doubleValue);
}  

void ReturnReferenceTo<const double>::From(const double& result, const WrapperBase* wrapper)
{
  Tcl_SetDoubleObj(Tcl_GetObjResult(wrapper->GetInterpreter()), result);
}

} // namespace _wrap_
