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

#ifdef _MSC_VER
// Get rid of warnings about bool conversions for the registered
// fundamental type conversions defined below.
#pragma warning (disable: 4800)
#endif

#include "wrapWrapperFacility.h"
#include "wrapTclCxxObject.h"
#include "wrapTypeInfo.h"
#include "wrapConverters.h"
#include "wrapWrapperBase.h"

#include <map>
#include <set>
#include <queue>
#include <stack>


// A macro to define the static method that will be registered with a
// Tcl interpreter to pass through a call to the instance of the
// facility associated with that interpreter.
#define _wrap_DEFINE_COMMAND_FUNCTION(command)                          \
int WrapperFacility                                                     \
::command##CommandFunction(ClientData clientData, Tcl_Interp* interp,   \
                           int objc, Tcl_Obj* CONST objv[])             \
{                                                                       \
  return WrapperFacility::GetForInterpreter(interp)                     \
    ->command##Command(objc, objv);                                     \
}


namespace _wrap_
{

// Map from type to the wrapper class for it.
typedef std::map<const Type*, WrapperBase*>  WrapperMapBase;
struct WrapperFacility::WrapperMap: public WrapperMapBase
{
  typedef WrapperMapBase::iterator iterator;
  typedef WrapperMapBase::const_iterator const_iterator;
  typedef WrapperMapBase::value_type value_type;
};

///! Internal class used to store an enumeration value instance.
struct EnumEntry
{
  EnumEntry(): m_Object(0), m_Type(0) {}
  EnumEntry(void* object, const Type* type): m_Object(object), m_Type(type) {}
  EnumEntry(const EnumEntry& r): m_Object(r.m_Object), m_Type(r.m_Type) {}
  void* m_Object;
  const Type* m_Type;
};  

// Map from enumeration value name to an EnumEntry referring to it.
typedef std::map<String, EnumEntry> EnumMapBase;
struct WrapperFacility::EnumMap: public EnumMapBase
{
  typedef EnumMapBase::iterator iterator;
  typedef EnumMapBase::const_iterator const_iterator;
  typedef EnumMapBase::value_type value_type;
};

// Map from type to delete function.
typedef std::map<const Type*,
                 WrapperFacility::DeleteFunction>  DeleteFunctionMapBase;
struct WrapperFacility::DeleteFunctionMap: public DeleteFunctionMapBase
{
  typedef DeleteFunctionMapBase::iterator iterator;
  typedef DeleteFunctionMapBase::const_iterator const_iterator;
  typedef DeleteFunctionMapBase::value_type value_type;
};


// Map from object pointer and type to CxxObject instance.
typedef std::pair<Anything::ObjectType, const Type*> CxxObjectMapKey;
typedef std::map<CxxObjectMapKey, CxxObject*> CxxObjectMapBase;
struct WrapperFacility::CxxObjectMap: public CxxObjectMapBase
{
  typedef CxxObjectMapBase::iterator iterator;
  typedef CxxObjectMapBase::const_iterator const_iterator;
  typedef CxxObjectMapBase::value_type value_type;
};

// Map from function pointer and type to CxxObject instance.
typedef Anything::FunctionType CxxFunctionMapKey;
typedef std::map<CxxFunctionMapKey, CxxObject*> CxxFunctionMapBase;
struct WrapperFacility::CxxFunctionMap: public CxxFunctionMapBase
{
  typedef CxxFunctionMapBase::iterator iterator;
  typedef CxxFunctionMapBase::const_iterator const_iterator;
  typedef CxxFunctionMapBase::value_type value_type;
};

// Map from conversion to conversion function.
typedef std::pair<CvQualifiedType, const Type*> ConversionKey;
typedef std::map<ConversionKey, ConversionFunction> ConversionMapBase;
struct WrapperFacility::ConversionMap: public ConversionMapBase
{
  typedef ConversionMapBase::iterator iterator;
  typedef ConversionMapBase::const_iterator const_iterator;
  typedef ConversionMapBase::value_type value_type;  
};


/**
 * Get a WrapperFacility object set up to deal with the given Tcl
 * interpreter.  If one exists, it will be returned.  Otherwise, a new
 * one will be created.
 */
WrapperFacility* WrapperFacility::GetForInterpreter(Tcl_Interp* interp)
{
  // See if the interpreter has already been given a WrapperFacility.
  ClientData data = Tcl_GetAssocData(interp, "WrapTclFacility", 0);
  if(data)
    {
    return static_cast<WrapperFacility*>(data);
    }
  
  // No, we must create a new WrapperFacility for this interpreter.
  WrapperFacility* newWrapperFacility = new WrapperFacility(interp);
  Tcl_SetAssocData(interp, "WrapTclFacility", &InterpreterFreeCallback,
                   newWrapperFacility);
  return newWrapperFacility;
}

  
/**
 * Get the Tcl interpreter for which this WrapperFacility has been
 * configured.
 */
Tcl_Interp* WrapperFacility::GetInterpreter() const
{
  return m_Interpreter;
}


/**
 * The constructor initializes the facility to work with the given
 * interpreter.
 */
WrapperFacility::WrapperFacility(Tcl_Interp* interp):
  m_Interpreter(interp),
  m_EnumMap(new EnumMap),
  m_WrapperMap(new WrapperMap),
  m_DeleteFunctionMap(new DeleteFunctionMap),
  m_CxxObjectMap(new CxxObjectMap),
  m_CxxFunctionMap(new CxxFunctionMap),
  m_ConversionMap(new ConversionMap)
{
  // Make sure the class has been initialized globally.
  WrapperFacility::ClassInitialize();
  
  // Initialize the predefined conversions.
  this->InitializePredefinedConversions();

  this->InitializeForInterpreter();
#ifdef _wrap_DEBUG_SUPPORT
  m_Debug = false;
#endif
}


/**
 * The destructor frees all enumeration values that have been
 * registered and deletes the facility's conversion table.
 */
WrapperFacility::~WrapperFacility()
{
  // The interpreter is being deleted.  Make sure no code below tries
  // to use it.
  m_Interpreter = 0;

  // Delete object instances first because they may want to use the
  // facility for something.  Loop over the maps and explicitly delete
  // every instance left.  Do not call CxxObject::Delete because that
  // will try to remove its instance of from this map!
  for(CxxFunctionMap::const_iterator f = m_CxxFunctionMap->begin();
      f != m_CxxFunctionMap->end(); ++f)
    {
    delete f->second;
    }
  for(CxxObjectMap::const_iterator o = m_CxxObjectMap->begin();
      o != m_CxxObjectMap->end(); ++o)
    {
    delete o->second;
    }

  // Delete enumeration object values.
  for(EnumMap::const_iterator e = m_EnumMap->begin();
      e != m_EnumMap->end(); ++e)
    {
    this->DeleteObject(e->second.m_Object, e->second.m_Type);
    }
  
  // Delete class wrapper objects.
  for(WrapperMap::const_iterator w = m_WrapperMap->begin();
      w != m_WrapperMap->end(); ++w)
    {
    delete w->second;
    }
  
  delete m_WrapperMap;
  delete m_EnumMap;
  delete m_DeleteFunctionMap;
  delete m_CxxObjectMap;
  delete m_CxxFunctionMap;
  delete m_ConversionMap;
}


/**
 * Called by the constructor to initialize the facility for its
 * interpreter.
 */
void WrapperFacility::InitializeForInterpreter()
{
  Tcl_CreateObjCommand(m_Interpreter, "wrap::ListMethods",
                       &ListMethodsCommandFunction, 0, 0);
  Tcl_CreateObjCommand(m_Interpreter, "wrap::TypeOf",
                       &TypeOfCommandFunction, 0, 0);
  Tcl_CreateObjCommand(m_Interpreter, "wrap::Interpreter",
                       &InterpreterCommandFunction, 0, 0);
  Tcl_CreateObjCommand(m_Interpreter, "wrap::DebugOn",
                       &DebugOnCommandFunction, 0, 0);
  Tcl_CreateObjCommand(m_Interpreter, "wrap::DebugOff",
                       &DebugOffCommandFunction, 0, 0);
  
  Tcl_PkgProvide(m_Interpreter, "WrapTclFacility", "1.0");
}


/**
 * When a Tcl interpreter is deleted, this is called to free its
 * WrapperFacility.
 */
void WrapperFacility::InterpreterFreeCallback(ClientData data,
                                              Tcl_Interp* interp)
{
  delete static_cast<WrapperFacility*>(data);
}


int WrapperFacility::ListMethodsCommand(int objc, Tcl_Obj* CONST objv[]) const
{
  static const char usage[] =
    "Usage: ListMethods <id>\n"
    "  Where <id> is a C++ class type object, pointer, or reference.";
  
  const Type* type = NULL;
  
  if(objc > 1)
    {
    if(TclObjectTypeIsCxxObject(objv[1]))
      {
      CxxObject* cxxObject=0;
      Tcl_GetCxxObjectFromObj(m_Interpreter, objv[1], &cxxObject);
      type = cxxObject->GetType();
      }
    else
      {
      CxxObject* cxxObject=0;
      if(StringRepIsCxxObject(Tcl_GetStringFromObj(objv[1], NULL))
         && (Tcl_GetCxxObjectFromObj(m_Interpreter, objv[1], &cxxObject) == TCL_OK))
        {
        type = cxxObject->GetType();
        }
      }
    if(type->IsPointerType())
      {
      type = PointerType::SafeDownCast(type)->GetPointedToType().GetType();
      }
    else if(type->IsReferenceType())
      {
      type = ReferenceType::SafeDownCast(type)->GetReferencedType().GetType();
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


/**
 * This implements the wrapper facility command "wrap::Interpreter".
 * It returns a pointer to the Tcl interpreter itself that can be
 * passed as an argument of type "Tcl_Interp*" of a wrapped function
 * or method.
 */
int WrapperFacility::InterpreterCommand(int, Tcl_Obj* CONST[]) const
{
  CxxObject* cxxObject =
    this->GetCxxObjectFor(Anything(m_Interpreter),
                          CvPredefinedType<Tcl_Interp*>::type.GetType());
  Tcl_SetObjResult(m_Interpreter, Tcl_NewCxxObjectObj(cxxObject));
  return TCL_OK;
}


/**
 * Command to turn on debug output.  This is defined whether or not
 * debug support is compiled in.  It will report that debug support is
 * not available if it is called.
 */
int WrapperFacility::DebugOnCommand(int, Tcl_Obj* CONST[])
{
#ifdef _wrap_DEBUG_SUPPORT
  Tcl_SetObjResult(m_Interpreter,
                   Tcl_NewStringObj("Debug output on.", -1));
  m_Debug = true;
  return TCL_OK;
#else
  Tcl_ResetResult(m_Interpreter);
  Tcl_AppendResult(m_Interpreter, "Debug output support not compiled in!", 0);
  return TCL_ERROR;  
#endif  
}


/**
 * Command to turn off debug output.  This is defined whether or not
 * debug support is compiled in.  It will report that debug support is
 * not available if it is called.
 */
int WrapperFacility::DebugOffCommand(int, Tcl_Obj* CONST[])
{
#ifdef _wrap_DEBUG_SUPPORT
  Tcl_SetObjResult(m_Interpreter,
                   Tcl_NewStringObj("Debug output off.", -1));
  m_Debug = false;
  return TCL_OK;
#else
  Tcl_ResetResult(m_Interpreter);
  Tcl_AppendResult(m_Interpreter, "Debug output support not compiled in!", 0);
  return TCL_ERROR;
#endif  
}

_wrap_DEFINE_COMMAND_FUNCTION(DebugOn)
_wrap_DEFINE_COMMAND_FUNCTION(DebugOff)
_wrap_DEFINE_COMMAND_FUNCTION(ListMethods)
_wrap_DEFINE_COMMAND_FUNCTION(TypeOf)
_wrap_DEFINE_COMMAND_FUNCTION(Interpreter)

/**
 * Try to figure out the name of the type of the given Tcl object.
 * If the type cannot be determined, a default of "char*" is returned.
 * Used for type-based overload resolution.
 */
CvQualifiedType WrapperFacility::GetObjectType(Tcl_Obj* obj) const
{
  // First try to use type information from Tcl.
  if(TclObjectTypeIsCxxObject(obj))
    {
    CxxObject* cxxObject=0;
    Tcl_GetCxxObjectFromObj(m_Interpreter, obj, &cxxObject);
    return cxxObject->GetType()->GetCvQualifiedType(false, false);
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
    String stringRep = Tcl_GetStringFromObj(obj, 0);
    // First check if the string names an enumeration constant, then
    // try to convert the string representation to various object
    // types.
    EnumMap::const_iterator e = m_EnumMap->find(stringRep);
    if(e != m_EnumMap->end())
      {
      return e->second.m_Type->GetCvQualifiedType(false, false);
      }    
    else if(StringRepIsCxxObject(stringRep))
      {
      CxxObject* cxxObject=0;
      if(Tcl_GetCxxObjectFromObj(m_Interpreter, obj, &cxxObject) == TCL_OK)
        {
        return cxxObject->GetType()->GetCvQualifiedType(false, false);
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
  if(TclObjectTypeIsCxxObject(obj))
    {
    CxxObject* cxxObject=0;
    Tcl_GetCxxObjectFromObj(m_Interpreter, obj, &cxxObject);
    const Type* type = cxxObject->GetType();
    if(type->IsPointerType())
      {
      argument.SetToPointer(cxxObject->GetObject(),
                            type->GetCvQualifiedType(false, false));
      }
    else if(type->IsReferenceType())
      {
      argument.SetToObject(cxxObject->GetObject(),
                           ReferenceType::SafeDownCast(type)->GetReferencedType());
      }
    else
      {
      argument.SetToObject(cxxObject->GetObject(),
                           type->GetCvQualifiedType(false, false));
      }
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
    CxxObject* cxxObject=0;
      
    // See if it the name of an instance.
    String stringRep = Tcl_GetStringFromObj(obj, NULL);

    // First check if the string names an enumeration constant, then
    // try to convert the string representation to various object
    // types.
    EnumMap::const_iterator e = m_EnumMap->find(stringRep);
    if(e != m_EnumMap->end())
      {
      argument.SetToObject(e->second.m_Object,
                           e->second.m_Type->GetCvQualifiedType(false, false));
      }    
    else if(StringRepIsCxxObject(stringRep)
            && (Tcl_GetCxxObjectFromObj(m_Interpreter, obj, &cxxObject) == TCL_OK))
      {
      const Type* type = cxxObject->GetType();
      if(type->IsPointerType())
        {
        argument.SetToPointer(cxxObject->GetObject(),
                              type->GetCvQualifiedType(false, false));
        }
      else if(type->IsReferenceType())
        {
        argument.SetToObject(cxxObject->GetObject(),
                             ReferenceType::SafeDownCast(type)->GetReferencedType());
        }
      else
        {
        argument.SetToObject(cxxObject->GetObject(),
                             type->GetCvQualifiedType(false, false));
        }
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
 * Return whether a wrapper for the given type exists.
 */
bool WrapperFacility::WrapperExists(const Type* type) const
{
  return (m_WrapperMap->count(type) > 0);
}
  

/**
 * Register a wrapper for the given type.
 */
void WrapperFacility::SetWrapper(const Type* type, WrapperBase* wrapper)
{
  // Delete any existing Wrapper for the type.
  WrapperMap::const_iterator w = m_WrapperMap->find(type);
  if(w != m_WrapperMap->end())
    {
    delete w->second;
    }
  
  // Set the new Wrapper for the type.
  m_WrapperMap->insert(WrapperMap::value_type(type, wrapper));
}
  
 
/**
 * Retrieve the wrapper for the given type.  If none exists, NULL is
 * returned.
 */
WrapperBase*
WrapperFacility::GetWrapper(const Type* type) const
{
  WrapperMap::const_iterator i = m_WrapperMap->find(type);
  if(i != m_WrapperMap->end())
    {
    return i->second;
    }
  return NULL;
}


/**
 * Setup an enumeration constant so that it can be referenced by name.
 * This deletes any instance of a value already having the given name.
 */
void WrapperFacility::SetEnumerationConstant(const String& name,
                                             void* object,
                                             const Type* type)
{
  EnumMap::const_iterator e = m_EnumMap->find(name);
  if(e != m_EnumMap->end())
    {
    this->DeleteObject(e->second.m_Object, e->second.m_Type);
    }
  m_EnumMap->insert(EnumMap::value_type(name, EnumEntry(object, type)));
}  


/**
 * Set the delete function to be used for deleting objects of the
 * given type.
 */
void WrapperFacility::SetDeleteFunction(const Type* type, DeleteFunction func)
{
  m_DeleteFunctionMap->insert(DeleteFunctionMap::value_type(type, func));
}


/**
 * Delete an object at the given address with the given type.  This
 * assumes that a delete function has been registered for the given
 * type.
 */
void WrapperFacility::DeleteObject(const void* object, const Type* type) const
{
  DeleteFunctionMap::const_iterator df = m_DeleteFunctionMap->find(type);
  if(df != m_DeleteFunctionMap->end())
    {
    (df->second)(object);
    }
  else
    {
    // Uh oh!
    }
}


/**
 * Get a CxxObject instance for the given object and type.  If none
 * exists, one will be created.
 */
CxxObject* WrapperFacility::GetCxxObjectFor(const Anything& anything,
                                            const Type* type) const
{
  if(type->IsFunctionType())
    {
    CxxFunctionMapKey key = anything.function;
    CxxFunctionMap::const_iterator f = m_CxxFunctionMap->find(key);
    if(f != m_CxxFunctionMap->end())
      {
      return f->second;
      }
    else
      {
      CxxObject* cxxObject = new CxxObject(anything, type, this);
      m_CxxFunctionMap->insert(CxxFunctionMap::value_type(key, cxxObject));
      return cxxObject;
      }
    }
  else
    {
    CxxObjectMapKey key(anything.object, type);
    CxxObjectMap::const_iterator o = m_CxxObjectMap->find(key);
    if(o != m_CxxObjectMap->end())
      {
      return o->second;
      }
    else
      {
      CxxObject* cxxObject = new CxxObject(anything, type, this);
      m_CxxObjectMap->insert(CxxObjectMap::value_type(key, cxxObject));
      return cxxObject;
      }
    }
}


/**
 * Delete the CxxObject instance for the given object and type.
 */
void WrapperFacility::DeleteCxxObjectFor(const Anything& anything,
                                         const Type* type) const
{
  if(type->IsFunctionType())
    {
    CxxFunctionMapKey key = anything.function;
    CxxFunctionMap::const_iterator f = m_CxxFunctionMap->find(key);
    if(f != m_CxxFunctionMap->end())
      {
      delete f->second;
      m_CxxFunctionMap->erase(key);
      }
    }
  else
    {
    CxxObjectMapKey key(anything.object, type);
    CxxObjectMap::const_iterator o = m_CxxObjectMap->find(key);
    if(o != m_CxxObjectMap->end())
      {
      delete o->second;
      m_CxxObjectMap->erase(key);
      }
    }
}


/**
 * Set the conversion function for the given conversion.
 */
void WrapperFacility::SetConversion(const CvQualifiedType& from,
                                    const Type* to, ConversionFunction f) const
{
  ConversionKey conversionKey(from, to);
  m_ConversionMap->insert(ConversionMap::value_type(conversionKey, f));
}


namespace
{
void* ConvertToPointerToVoid(Anything anything)
{
  return anything.object;
}

const void* ConvertToPointerToConstVoid(Anything anything)
{
  return anything.object;
}
}

/**
 * Retrieve the function for the given conversion.  If an exact match for
 * the "from" type is not found, an attempt is made to find one that is
 * more cv-qualified.  If none exists, NULL is returned.
 */
ConversionFunction
WrapperFacility::GetConversion(const CvQualifiedType& from,
                               const Type* to) const
{
  // Try to find exact match for "from" type.
  ConversionKey conversionKey(from, to);
  ConversionMap::const_iterator i = m_ConversionMap->find(conversionKey);
  if(i != m_ConversionMap->end())
    {
    return i->second;
    }
  
  // If the "from" type is a reference type, we can try adding
  // qualifiers to the type it references.
  if(from.GetType()->IsReferenceType())
    {
    CvQualifiedType referencedType =
      ReferenceType::SafeDownCast(from.GetType())->GetReferencedType();
    conversionKey = ConversionKey(TypeInfo::GetReferenceType(referencedType.GetMoreQualifiedType(true, false)), to);
    i = m_ConversionMap->find(conversionKey);
    if(i != m_ConversionMap->end())
      {
      return i->second;
      }
    conversionKey = ConversionKey(TypeInfo::GetReferenceType(referencedType.GetMoreQualifiedType(false, true)), to);
    i = m_ConversionMap->find(conversionKey);
    if(i != m_ConversionMap->end())
      {
      return i->second;
      }
    conversionKey = ConversionKey(TypeInfo::GetReferenceType(referencedType.GetMoreQualifiedType(true, true)), to);
    i = m_ConversionMap->find(conversionKey);
    if(i != m_ConversionMap->end())
      {
      return i->second;
      }
    }
  // If the "from" type is not a reference type, we can try adding
  // qualifiers to it.
  else
    {
    // Try adding a const qualifier to the "from" type.
    conversionKey = ConversionKey(from.GetMoreQualifiedType(true, false), to);
    i = m_ConversionMap->find(conversionKey);
    if(i != m_ConversionMap->end())
      {
      return i->second;
      }
    // Try adding a volatile qualifier to the "from" type.
    conversionKey = ConversionKey(from.GetMoreQualifiedType(false, true), to);
    i = m_ConversionMap->find(conversionKey);
    if(i != m_ConversionMap->end())
      {
      return i->second;
      }
    // Try adding both const and volatile qualifiers to the "from" type.
    conversionKey = ConversionKey(from.GetMoreQualifiedType(true, true), to);
    i = m_ConversionMap->find(conversionKey);
    if(i != m_ConversionMap->end())
      {
      return i->second;
      }
    }
  
  // A special hack for conversion of any pointer type to pointer to
  // void.
  if(to->IsPointerType() && from.IsPointerType())
    {
    CvQualifiedType toType = PointerType::SafeDownCast(to)->GetPointedToType();
    if(toType.IsFundamentalType()
       && FundamentalType::SafeDownCast(toType.GetType())->IsVoid())
      {
      CvQualifiedType fromType = PointerType::SafeDownCast(from.GetType())->GetPointedToType();
      if(!fromType.IsFunctionType())
        {
        if(!fromType.IsConst() && !fromType.IsVolatile())
          {
          return reinterpret_cast<ConversionFunction>(&ConvertToPointerToVoid);
          }
        if(fromType.IsConst() && !fromType.IsVolatile())
          {
          return reinterpret_cast<ConversionFunction>(&ConvertToPointerToConstVoid);
          }
        }
      }
    }
  
  // Couldn't find a conversion.
  return NULL;
}

// Macro to shorten InitializePredefinedConversions function body.
#define _wrap_REGISTER_FUNDAMENTAL_TYPE_CONVERSIONS(T1, T2) \
this->SetConversion(CvPredefinedType<const T1>::type, \
                    CvPredefinedType<T2>::type.GetType(), \
                    Converter::ConversionByConstructor<T1, T2>::GetConversionFunction()); \
this->SetConversion(CvPredefinedType<const T2>::type, \
                    CvPredefinedType<T1>::type.GetType(), \
                    Converter::ConversionByConstructor<T2, T1>::GetConversionFunction())

/**
 * Register basic type conversion functions for this WrapperFacility.
 * Only called from the constructor.
 *
 * The "from" type for any conversion added here should be
 * const-friendly, if possible.  This way, conversion from a non-const
 * type can still chain up to the conversion from the const type, thus
 * avoiding duplication of the conversion function.
 */
void WrapperFacility::InitializePredefinedConversions() const
{
  _wrap_REGISTER_FUNDAMENTAL_TYPE_CONVERSIONS(bool, char);
  _wrap_REGISTER_FUNDAMENTAL_TYPE_CONVERSIONS(bool, short);
  _wrap_REGISTER_FUNDAMENTAL_TYPE_CONVERSIONS(bool, int);
  _wrap_REGISTER_FUNDAMENTAL_TYPE_CONVERSIONS(bool, long);
  _wrap_REGISTER_FUNDAMENTAL_TYPE_CONVERSIONS(int, unsigned char);
  _wrap_REGISTER_FUNDAMENTAL_TYPE_CONVERSIONS(int, unsigned short);
  _wrap_REGISTER_FUNDAMENTAL_TYPE_CONVERSIONS(int, unsigned int);
  _wrap_REGISTER_FUNDAMENTAL_TYPE_CONVERSIONS(int, unsigned long);
  _wrap_REGISTER_FUNDAMENTAL_TYPE_CONVERSIONS(long, unsigned char);
  _wrap_REGISTER_FUNDAMENTAL_TYPE_CONVERSIONS(long, unsigned short);
  _wrap_REGISTER_FUNDAMENTAL_TYPE_CONVERSIONS(long, unsigned int);
  _wrap_REGISTER_FUNDAMENTAL_TYPE_CONVERSIONS(long, unsigned long);
  _wrap_REGISTER_FUNDAMENTAL_TYPE_CONVERSIONS(long, char);
  _wrap_REGISTER_FUNDAMENTAL_TYPE_CONVERSIONS(long, short);
  _wrap_REGISTER_FUNDAMENTAL_TYPE_CONVERSIONS(long, int);
  _wrap_REGISTER_FUNDAMENTAL_TYPE_CONVERSIONS(int, float);
  _wrap_REGISTER_FUNDAMENTAL_TYPE_CONVERSIONS(long, float);
  _wrap_REGISTER_FUNDAMENTAL_TYPE_CONVERSIONS(int, double);
  _wrap_REGISTER_FUNDAMENTAL_TYPE_CONVERSIONS(long, double);
  _wrap_REGISTER_FUNDAMENTAL_TYPE_CONVERSIONS(float, double);
}


/**
 * This is called by the constructor of WrapperFacility to make sure
 * that the facility has been initialized.
 */
void WrapperFacility::ClassInitialize()
{
  // Make sure this function is only executed once.
  static bool initialized = false;
  if(initialized) { return; }
  
  // Call other class' initialization functions in a safe order.
  TypeInfo::ClassInitialize();
  TclCxxObject::ClassInitialize();

  initialized = true;
}

} // namespace _wrap_

#ifdef _wrap_DEBUG_SUPPORT

#ifdef _WIN32
#  include "wrapWin32OutputWindow.h"
#else
#  include <iostream>
#endif

namespace _wrap_
{

/**
 * Test whether the debug output flag is currently on.
 */
bool WrapperFacility::DebugIsOn() const
{
  return m_Debug;
}


/**
 * Write the given text to the debug output buffer.
 */
void WrapperFacility::OutputDebugText(const char* text) const
{
#ifdef _WIN32
  Win32OutputWindow::GetInstance()->DisplayText(text);
#else
  std::cout << text;
#endif
}

} // namespace _wrap_
#endif

