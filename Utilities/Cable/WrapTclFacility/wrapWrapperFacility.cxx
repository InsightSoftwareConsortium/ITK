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
  m_ConversionTable = new ConversionTable();
  m_InstanceTable = new InstanceTable(m_Interpreter);
  m_WrapperTable = new WrapperTable(m_Interpreter);
  Tcl_PkgProvide(m_Interpreter, "Wrap", "1.0");
}

WrapperFacility::~WrapperFacility()
{
}

extern Tcl_ObjType TclPointerType;
extern Tcl_ObjType TclReferenceType;

/**
 * Initialization function for a Tcl interpreter.   This will only execute
 * exactly once for a given interpreter.
 *
 * This just registers the Pointer and Reference Tcl object types needed
 * by the wrappers with the interpreter.
 */
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


#if 0
/**
 * The function called back from a Tcl interpreter...
 */
int WrapperFacility::ListMethodsCommand(ClientData, Tcl_Interp* interp,
                                        int objc, Tcl_Obj* CONST objv[])
{  
}
#endif

} // namespace _wrap_

