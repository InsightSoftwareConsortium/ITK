/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapWrapperFacility.cxx
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
#include "wrapWrapperFacility.h"
#include "wrapPointer.h"
#include "wrapReference.h"
#include "wrapConversionTable.h"
#include "wrapInstanceTable.h"
#include "wrapWrapperTable.h"
#include "wrapTypeInfo.h"
#include "wrapWrapperBase.h"

#include <map>

namespace _wrap_
{


/**
 * Get a WrapperFacility object set up to deal with the given Tcl
 * interpreter.  If one exists, it will be returned.  Otherwise, a new
 * one will be created.
 */
WrapperFacility* WrapperFacility::GetForInterpreter(Tcl_Interp* interp)
{
  static std::map<const Tcl_Interp*, WrapperFacility*>
    interpreterWrapperFacilityMap;
  
  // See if an WrapperFacility exists for the given interpreter.
  if(interpreterWrapperFacilityMap.count(interp) == 0)
    {
    // No, we must create a new WrapperFacility for this interpreter.
    interpreterWrapperFacilityMap[interp] = new WrapperFacility(interp);
    }
  
  // Return the WrapperFacility.
  return interpreterWrapperFacilityMap[interp];  
}

Tcl_Interp* WrapperFacility::GetInterpreter() const
{
  return m_Interpreter;
}

ConversionTable* WrapperFacility::GetConversionTable() const
{
  return m_ConversionTable;
}

InstanceTable* WrapperFacility::GetInstanceTable() const
{
  return m_InstanceTable;
}

WrapperTable* WrapperFacility::GetWrapperTable() const
{
  return m_WrapperTable;
}


WrapperFacility::WrapperFacility(Tcl_Interp* interp):
  m_Interpreter(interp)
{
  this->Initialize();
  this->InitializeForInterpreter();
}

WrapperFacility::~WrapperFacility()
{
}

extern Tcl_ObjType TclPointerType;
extern Tcl_ObjType TclReferenceType;

void WrapperFacility::Initialize()
{
  static bool initialized = false;
  
  if(initialized)
    {
    return;
    }
  
  // Initialize predefined type information.  This only executes once,
  // no matter how many interpreters are using it.
  _wrap_::TypeInfo::Initialize();
  
  // Register the Tcl object types we need.
  Tcl_RegisterObjType(&_wrap_::TclPointerType);
  Tcl_RegisterObjType(&_wrap_::TclReferenceType);
  
  initialized = true;
}


void WrapperFacility::InitializeForInterpreter()
{
  m_ConversionTable = new ConversionTable();
  m_InstanceTable = new InstanceTable(m_Interpreter);
  m_WrapperTable = new WrapperTable(m_Interpreter);
  
  Tcl_CreateObjCommand(m_Interpreter, "wrap::ListMethods",
                       &ListMethodsCommandFunction, 0, 0);
  
  Tcl_PkgProvide(m_Interpreter, "Wrap", "1.0");
}


int WrapperFacility::ListMethodsCommand(int objc, Tcl_Obj* CONST objv[]) const
{
  static const char usage[] =
    "Usage: ListMethods <id>\n"
    "  Where <id> is an object name, pointer, reference.";
  
  WrapperBase* wrapper = NULL;
  
  if(objc > 1)
    {
    Pointer p;
    Reference r;
    const Type* type = NULL;
    
    if(TclObjectTypeIsPointer(objv[1]))
      {
      Tcl_GetPointerFromObj(m_Interpreter, objv[1], &p);
      type = p.GetPointedToType().GetType();
      }
    else if(TclObjectTypeIsReference(objv[1]))
      {
      Tcl_GetReferenceFromObj(m_Interpreter, objv[1], &r);
      type = r.GetReferencedType().GetType();
      }
    else
      {
      String objectName = Tcl_GetStringFromObj(objv[1], NULL);
      if(m_InstanceTable->Exists(objectName))
        {
        type = m_InstanceTable->GetType(objectName).GetType();
        }
      else if(StringRepIsPointer(objectName)
              && (Tcl_GetPointerFromObj(m_Interpreter, objv[1], &p) == TCL_OK))
        {
        type = p.GetPointedToType().GetType();
        }
      else if(StringRepIsReference(objectName)
              && (Tcl_GetReferenceFromObj(m_Interpreter, objv[1], &r) == TCL_OK))
        {
        type = r.GetReferencedType().GetType();
        }    
      }
    
    if(type)
      {
      wrapper = m_WrapperTable->GetWrapper(type);
      }
    }
  
  Tcl_ResetResult(m_Interpreter);
  if(wrapper)
    {
    return wrapper->ListMethods();
    }
  else
    {
    Tcl_AppendResult(m_Interpreter, usage, NULL);
    return TCL_ERROR;
    }
}


int WrapperFacility
::ListMethodsCommandFunction(ClientData clientData, Tcl_Interp* interp,
                             int objc, Tcl_Obj* CONST objv[])
{
  WrapperFacility* wrapperFacility =
    WrapperFacility::GetForInterpreter(interp);
  return wrapperFacility->ListMethodsCommand(objc, objv);
}


} // namespace _wrap_

