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
#include "wrapTypeInfo.h"
#include "wrapWrapperBase.h"

#include <map>
#include <queue>
#include <stack>

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
  
  Tcl_CreateObjCommand(m_Interpreter, "wrap::ListMethods",
                       &ListMethodsCommandFunction, 0, 0);
  Tcl_CreateObjCommand(m_Interpreter, "wrap::TypeOf",
                       &TypeOfCommandFunction, 0, 0);
  Tcl_CreateObjCommand(m_Interpreter, "wrap::Delete",
                       &DeleteCommandFunction, 0, 0);
  
  Tcl_PkgProvide(m_Interpreter, "Wrap", "1.0");
}


int WrapperFacility::ListMethodsCommand(int objc, Tcl_Obj* CONST objv[]) const
{
  static const char usage[] =
    "Usage: ListMethods <id>\n"
    "  Where <id> is an object name, pointer, or reference.";
  
  const Type* type = NULL;
  
  if(objc > 1)
    {
    Pointer p;
    Reference r;
    
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
    }
  
  // Cast down to the ClassType.  We don't use ClassType::SafeDownCast()
  // because we want a return of NULL for a failed cast.
  const ClassType* classType = dynamic_cast<const ClassType*>(type);
  Tcl_ResetResult(m_Interpreter);
  if(!classType)
    {
    Tcl_AppendResult(m_Interpreter, usage, NULL);
    return TCL_ERROR;
    }

  // A queue to do a BFS of this class and its parents, but without
  // duplicates.
  std::queue<const ClassType*> classQueue;
  std::set<const ClassType*> visited;
  
  // A stack used to reverse the order of the queue after the
  // breadth-first traversal.
  std::stack<const ClassType*> outputStack;
    
  // Start with the search at this class.
  visited.insert(classType);
  classQueue.push(classType);
  while(!classQueue.empty())
    {
    // Get the next class off the queue.
    const ClassType* curClass = classQueue.front(); classQueue.pop();
    outputStack.push(curClass);
      
    // Walk up to the class's parents.
    for(ClassTypes::const_iterator parent = curClass->ParentsBegin();
        parent != curClass->ParentsEnd(); ++parent)
      {
      if(visited.count(*parent) == 0)
        {
        visited.insert(*parent);
        classQueue.push(*parent);
        }
      }
    }
      
  while(!outputStack.empty())
    {
    // Get the next class off the stack.
    const ClassType* curClass = outputStack.top(); outputStack.pop();
    // If the class has a wrapper, list its methods.
    const WrapperBase* wrapper = this->GetWrapper(curClass);
    if(wrapper)
      {
      wrapper->ListMethods();
      }
    }
  
  return TCL_OK;
}

int WrapperFacility::TypeOfCommand(int objc, Tcl_Obj* CONST objv[]) const
{
  static const char usage[] =
    "Usage: TypeOf <expr>\n"
    "  Where <expr> is any Tcl expression.";
  
  CvQualifiedType cvType;
  
  if(objc > 1)
    {
    cvType = this->GetObjectType(objv[1]);
    }
  
  Tcl_ResetResult(m_Interpreter);
  if(!cvType.GetType())
    {
    Tcl_AppendResult(m_Interpreter, usage, NULL);
    return TCL_ERROR;
    }
  
  String typeName = cvType.GetName();
  Tcl_AppendResult(m_Interpreter, const_cast<char*>(typeName.c_str()), NULL);
  
  return TCL_OK;
}


int WrapperFacility::DeleteCommand(int objc, Tcl_Obj* CONST objv[]) const
{
  static const char usage[] =
    "Usage: Delete <name-of-object>";
  
  Tcl_ResetResult(m_Interpreter);
  if(objc > 1)
    {
    String objectName = Tcl_GetStringFromObj(objv[1], NULL);
    if(m_InstanceTable->Exists(objectName))
      {
      m_InstanceTable->DeleteObject(objectName);
      return TCL_OK;
      }
    else
      {
      Tcl_AppendResult(m_Interpreter, "Don't know about object with name ",
                       const_cast<char*>(objectName.c_str()), NULL);
      return TCL_ERROR;
      }
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

int WrapperFacility
::TypeOfCommandFunction(ClientData clientData, Tcl_Interp* interp,
                         int objc, Tcl_Obj* CONST objv[])
{
  WrapperFacility* wrapperFacility =
    WrapperFacility::GetForInterpreter(interp);
  return wrapperFacility->TypeOfCommand(objc, objv);
}


int WrapperFacility
::DeleteCommandFunction(ClientData clientData, Tcl_Interp* interp,
                        int objc, Tcl_Obj* CONST objv[])
{
  WrapperFacility* wrapperFacility =
    WrapperFacility::GetForInterpreter(interp);
  return wrapperFacility->DeleteCommand(objc, objv);
}


/**
 * Try to figure out the name of the type of the given Tcl object.
 * If the type cannot be determined, a default of "char*" is returned.
 * Used for type-based overload resolution.
 */
CvQualifiedType WrapperFacility::GetObjectType(Tcl_Obj* obj) const
{
  // First try to use type information from Tcl.
  if(TclObjectTypeIsPointer(obj))
    {
    Pointer p;
    Tcl_GetPointerFromObj(m_Interpreter, obj, &p);
    return TypeInfo::GetPointerType(p.GetPointedToType(), false, false);
    }
  else if(TclObjectTypeIsReference(obj))
    {
    Reference r;
    Tcl_GetReferenceFromObj(m_Interpreter, obj, &r);
    return TypeInfo::GetReferenceType(r.GetReferencedType());
    }
  else if(TclObjectTypeIsBoolean(obj))
    {
    return CvPredefinedType<bool>::type;
    }
  else if(TclObjectTypeIsInt(obj))
    {
    return CvPredefinedType<int>::type;
    }
  else if(TclObjectTypeIsLong(obj))
    {
    return CvPredefinedType<long>::type;
    }
  else if(TclObjectTypeIsDouble(obj))
    {
    return CvPredefinedType<double>::type;
    }
  // No Tcl type information.  Try converting from string representation.
  else
    {
    String objectName = Tcl_GetStringFromObj(obj, NULL);
    if(m_InstanceTable->Exists(objectName))
      {
      return m_InstanceTable->GetType(objectName);
      }
    else if(StringRepIsPointer(objectName))
      {
      Pointer p;
      if(Tcl_GetPointerFromObj(m_Interpreter, obj, &p) == TCL_OK)
        {
        return TypeInfo::GetPointerType(p.GetPointedToType(), false, false);
        }
      }
    else if(StringRepIsReference(objectName))
      {
      Reference r;
      if(Tcl_GetReferenceFromObj(m_Interpreter, obj, &r) == TCL_OK)
        {
        return TypeInfo::GetReferenceType(r.GetReferencedType());
        }
      }
    else
      {
      // No wrapping type information available.  Try to convert to
      // some basic types.
      long l;
      double d;
      if(Tcl_GetLongFromObj(m_Interpreter, obj, &l) == TCL_OK)
        {
        return CvPredefinedType<long>::type;
        }
      else if(Tcl_GetDoubleFromObj(m_Interpreter, obj, &d) == TCL_OK)
        {
        return CvPredefinedType<double>::type;
        }
      }
    }
  
  // Could not determine the type.  Default to char*.
  return CvPredefinedType<char*>::type;
}


/**
 * Try to figure out how to extract a C++ object from the given Tcl
 * object.  If the object type cannot be determined, char* is assumed.
 * In either case, an Argument which refers to the object is returned.
 */
Argument WrapperFacility::GetObjectArgument(Tcl_Obj* obj) const
{
  // Need a location to hold the Argument until returned.
  Argument argument;
  
  // First, see if Tcl has given us the type information.
  if(TclObjectTypeIsPointer(obj))
    {
    Pointer p;
    Tcl_GetPointerFromObj(m_Interpreter, obj, &p);
    argument.SetToPointer(p.GetObject(),
                          TypeInfo::GetPointerType(p.GetPointedToType(),
                                                   false, false));
    }
  else if(TclObjectTypeIsReference(obj))
    {
    Reference r;
    Tcl_GetReferenceFromObj(m_Interpreter, obj, &r);
    argument.SetToObject(r.GetObject(), r.GetReferencedType());
    }
  else if(TclObjectTypeIsBoolean(obj))
    {
    int i;
    Tcl_GetBooleanFromObj(m_Interpreter, obj, &i);
    bool b = (i!=0);
    argument.SetToBool(b);
    }
  else if(TclObjectTypeIsInt(obj))
    {
    int i;
    Tcl_GetIntFromObj(m_Interpreter, obj, &i);
    argument.SetToInt(i);
    }
  else if(TclObjectTypeIsLong(obj))
    {
    long l;
    Tcl_GetLongFromObj(m_Interpreter, obj, &l);
    argument.SetToLong(l);
    }
  else if(TclObjectTypeIsDouble(obj))
    {
    double d;
    Tcl_GetDoubleFromObj(m_Interpreter, obj, &d);
    argument.SetToDouble(d);
    }
  else
    {
    // Tcl has not given us the type information.  Try converting from
    // string representation.
    Pointer p;
    Reference r;

    // See if it the name of an instance.
    String objectName = Tcl_GetStringFromObj(obj, NULL);
    if(m_InstanceTable->Exists(objectName))
      {        
      argument.SetToObject(m_InstanceTable->GetObject(objectName),
                           m_InstanceTable->GetType(objectName));
      }
    else if(StringRepIsPointer(objectName)
            && (Tcl_GetPointerFromObj(m_Interpreter, obj, &p) == TCL_OK))
      {
      argument.SetToPointer(p.GetObject(),
                            TypeInfo::GetPointerType(p.GetPointedToType(),
                                                     false, false));
      }
    else if(StringRepIsReference(objectName)
            && (Tcl_GetReferenceFromObj(m_Interpreter, obj, &r) == TCL_OK))
      {
      argument.SetToObject(r.GetObject(), r.GetReferencedType());
      }
    else
      {
      // No type information available from string representation.
      // Try to convert to some basic Tcl types.
      long l;
      double d;
      if(Tcl_GetLongFromObj(m_Interpreter, obj, &l) == TCL_OK)
        {
        argument.SetToLong(l);
        }
      else if(Tcl_GetDoubleFromObj(m_Interpreter, obj, &d) == TCL_OK)
        {
        argument.SetToDouble(d);
        }
      else
        {
        // Can't identify the object type.  We will have to assume char*.
        argument.SetToPointer(Tcl_GetStringFromObj(obj, NULL),
                              CvPredefinedType<char*>::type);
        }
      }
    }
  
  // Return the result.
  return argument;
}

/**
 * Get the conversion function from the wrapper's ConversionTable for
 * the specified conversion.  The table will automatically try to add
 * cv-qualifiers to the "from" type to find a conversion.
 */
ConversionFunction
WrapperFacility::GetConversionFunction(const CvQualifiedType& from,
                                       const Type* to) const
{
  return m_ConversionTable->GetConversion(from, to);
}


/**
 * Return whether a wrapper for the given type exists.
 */
bool WrapperFacility::WrapperExists(const Type* type) const
{
  return (m_WrapperMap.count(type) > 0);
}
  

/**
 * Register a wrapper for the given type.
 */
void WrapperFacility::SetWrapper(const Type* type, WrapperBase* wrapper)
{
  m_WrapperMap[type] = wrapper;
}
  
 
/**
 * Retrieve the wrapper for the given type.  If none exists, NULL is
 * returned.
 */
WrapperBase*
WrapperFacility::GetWrapper(const Type* type) const
{
  WrapperMap::const_iterator i = m_WrapperMap.find(type);
  if(i != m_WrapperMap.end())
    {
    return i->second;
    }
  return NULL;
}

} // namespace _wrap_

