/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapWrapperFacility.h
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
#ifndef _wrapWrapperFacility_h
#define _wrapWrapperFacility_h

#include "wrapUtils.h"
#include "wrapArgument.h"

namespace _wrap_
{

class ConversionTable;
class WrapperBase;
class CxxObject;

/**
 * An instance of this class is associated with each interpreter.  It
 * provides the basic wrapper facility commands in the "wrap::"
 * namespace.  It keeps track of the ConversionTable and set of
 * wrapper classes that have been setup for the interpreter.
 */
class _wrap_EXPORT WrapperFacility
{
public:
  typedef WrapperFacility Self;
  static WrapperFacility* GetForInterpreter(Tcl_Interp*);

  Tcl_Interp* GetInterpreter() const;
  ConversionTable* GetConversionTable() const;
  
  bool WrapperExists(const Type* type) const;
  void SetWrapper(const Type*, WrapperBase*);
  WrapperBase* GetWrapper(const Type*) const;
  
  CvQualifiedType GetObjectType(Tcl_Obj* obj) const;
  Argument GetObjectArgument(Tcl_Obj* obj) const;
  ConversionFunction GetConversionFunction(const CvQualifiedType& from,
                                           const Type* to) const;
  
  void SetEnumerationConstant(const String&, void*, const Type*);
  
  ///! The type of a function that deletes an object.
  typedef void (*DeleteFunction)(const void*);
  void SetDeleteFunction(const Type*, DeleteFunction);
  void DeleteObject(const void*, const Type*) const;
  
  CxxObject* GetCxxObjectFor(const Anything&, const Type*) const;
  void DeleteCxxObjectFor(const Anything&, const Type*) const;
  void SetConversion(const CvQualifiedType&, const Type*, ConversionFunction) const;
  ConversionFunction GetConversion(const CvQualifiedType&, const Type*) const;
  
private:
  WrapperFacility(Tcl_Interp*);
  ~WrapperFacility();

  void InitializeForInterpreter();
  static void InterpreterFreeCallback(ClientData, Tcl_Interp*);
  int ListMethodsCommand(int, Tcl_Obj*CONST[]) const;
  int TypeOfCommand(int, Tcl_Obj*CONST[]) const;
  int InterpreterCommand(int, Tcl_Obj*CONST[]) const;
  int DebugOnCommand(int, Tcl_Obj*CONST[]);
  int DebugOffCommand(int, Tcl_Obj*CONST[]);
  static int ListMethodsCommandFunction(ClientData, Tcl_Interp*,
                                        int, Tcl_Obj*CONST[]);
  static int TypeOfCommandFunction(ClientData, Tcl_Interp*,
                                   int, Tcl_Obj*CONST[]);
  static int InterpreterCommandFunction(ClientData, Tcl_Interp*,
                                        int, Tcl_Obj*CONST[]);
  static int DebugOnCommandFunction(ClientData, Tcl_Interp*,
                                    int, Tcl_Obj*CONST[]);
  static int DebugOffCommandFunction(ClientData, Tcl_Interp*,
                                     int, Tcl_Obj*CONST[]);

  ///! The Tcl interpreter to which this facility is attached.
  Tcl_Interp* m_Interpreter;

  struct EnumMap;
  ///! The table of enumeration values that have been registered.
  EnumMap* m_EnumMap;

  struct WrapperMap;
  ///! Map from type to wrapper function.
  WrapperMap* m_WrapperMap;

  struct DeleteFunctionMap;
  ///! Table of registered delete functions.
  DeleteFunctionMap* m_DeleteFunctionMap;  

  struct CxxObjectMap;
  ///! Map from object location and type to the CxxObject instance for it.
  CxxObjectMap* m_CxxObjectMap;

  struct CxxFunctionMap;
  ///! Map from function pointer value to CxxObject instance for it.
  CxxFunctionMap* m_CxxFunctionMap;
  
  struct ConversionMap;
  ///! Map from conversion from/to pair to a conversion function.
  ConversionMap* m_ConversionMap;
  
public:
  void InitializePredefinedConversions() const;
  static void ClassInitialize();
private:
  
  // Include debugging-related code if debug support is on.
#ifdef _wrap_DEBUG_SUPPORT
private:
  bool m_Debug;
public:
  bool DebugIsOn() const;
  void OutputDebugText(const char*) const;
#endif
};


/**
 * A function to delete an object of any type.  Instantiations of this
 * can be registered as delete functions with a WrapperFacility.
 */
template <typename T>
struct OldObjectOf
{
  static void Delete(const void* object)
    {
    delete const_cast<T*>(static_cast<const T*>(object));
    }
};


} // namespace _wrap_

#endif
