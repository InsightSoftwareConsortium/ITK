#include <iostream>
#include <list>
#include <map>
#include <set>
#include <cstdio>

#include "internalRep.h"
#include "configRep.h"

/*

  Changes to prototype:
   - Add support for static method calls.
   - Add similar support for public constructor calls.
   - Add support for prefix operator registration.

 */


typedef std::map<String, std::vector<const Function*> >  MethodMap;
typedef MethodMap::const_iterator  MethodMapIterator;

/**
 * Get the wrapping name of the given Class.  This is an encoded form
 * of the fully qualified name that is a valid C identifier.
 */
String GetWrapName(const Class* c)
{
  return GetValid_C_Identifier(c->GetQualifiedName());
}


/**
 * Get the wrapping name of the given Function.  This is an encoded form
 * of the name that is a valid C identifier.
 */
String GetWrapName(const Function* f)
{
  return GetValid_C_Identifier(f->GetName());
}


/**
 * Record the type names needed by the interface to the given function.
 * Also records the type names of dereferenced pointers and references.
 */
template <typename OutputIterator>
void AddTypeNamesFromFunction(const Function* f, OutputIterator o)
{
  const Type* t=NULL;
  
  if(f->GetReturns() && f->GetReturns()->GetType())
    {
    t = f->GetReturns()->GetType();
    *o++ = t->GetName();
    if(t->IsFunctionPointer())
      {
      }
    else if(t->IsPointerType())
      {
      const PointerType* pt = (PointerType*)t;
      *o++ = pt->GetPointedToType()->GetName();
      }
    else if(t->IsReferenceType())
      {
      const ReferenceType* rt = (ReferenceType*)t;
      *o++ = rt->GetReferencedType()->GetName();
      }
    }
  for(ArgumentsIterator a = f->GetArguments().begin();
      a != f->GetArguments().end();
      ++a)
    {
    t = (*a)->GetType();
    if(t->IsFunctionPointer())
      {
      }
    else if(t->IsPointerType())
      {
      const PointerType* pt = (PointerType*)t;
      *o++ = pt->GetPointedToType()->GetName();
      }
    else if(t->IsReferenceType())
      {
      const ReferenceType* rt = (ReferenceType*)t;
      *o++ = rt->GetReferencedType()->GetName();
      }
    else
      {
      *o++ = t->GetName();
      }
    }
}


/**
 * Loop over all the public methods in the class, and record the type names
 * needed by their interfaces.  Also include the name of the class itself.
 */
template <typename OutputIterator>
void AddTypeNamesFromClass(const Class* c, OutputIterator o)
{
  *o++ = c->GetQualifiedName();
  for(MethodsIterator methodItr = c->GetMethods().begin();
      methodItr != c->GetMethods().end();
      ++methodItr)
    {
    Method* method = *methodItr;
    if(!method->IsDestructor() && (method->GetAccess() == Public))
      {
      AddTypeNamesFromFunction(method, o);
      }
    }
}


/**
 * Output the code that appears at the top of each generated wrapper file.
 *
 * Needs to iterate over all the WrapTypes.
 */
void OutputFileHeader(FILE* outFile,
                      WrapTypesIterator first,
                      WrapTypesIterator last)
{
  typedef std::set<String> IncludeFiles;
  typedef std::set<const Class*> Classes;
  IncludeFiles  includeFiles;
  Classes       classes;
  // Build a set of class names and include files for the wrap types.
  for(WrapTypesIterator w = first; w != last; ++w)
    {
    classes.insert(classes.begin(), w->second->GetClass().RealPointer());
    const Location* loc = w->second->GetClass()->GetLocation();
    if(loc)
      {
      includeFiles.insert(includeFiles.begin(), loc->GetFile());
      }
    }
  
  fprintf(outFile,
          "/**\n"
          " * Automatically generated wrappers.  Do not edit!\n"
          " * Wrappers for classes:\n");

  for(Classes::iterator c = classes.begin();
      c != classes.end(); ++c)
    {
    fprintf(outFile,
            " *   %s\n",
            (*c)->GetQualifiedName().c_str());
    }
  
  fprintf(outFile,
          " */\n"
          "\n"
          "#include \"_wrap_Utils.h\"\n"
          "#include \"_wrap_TypeInfo.h\"\n"
          "#include \"_wrap_Calls.h\"\n"
          "#include \"_wrap_Wrappers.h\"\n"
          "#include <map>\n"
          "\n");

  fprintf(outFile,
          "/**\n"
          " * Include types being wrapped.\n"
          " */\n");
  for(IncludeFiles::iterator i = includeFiles.begin();
      i != includeFiles.end(); ++i)
    {
    fprintf(outFile,
            "#include \"%s\"\n",
            i->c_str());
    }
  
  fprintf(outFile,
          "\n"
          "_wrap_NAMESPACE_BEGIN\n"
          "\n");
  
  fprintf(outFile,
          "/**\n"
          " * The method map for each type.\n"
          " */\n");
  // Print out the method map definitions.
  for(Classes::iterator c = classes.begin();
      c != classes.end(); ++c)
    {
    fprintf(outFile,
            "static std::map<String, void (*)(%s&, Tcl_Interp*, int, Tcl_Obj*CONST*)>\n"
            "methodMap_%s;\n"
            "\n",
            (*c)->GetQualifiedName().c_str(),
            GetWrapName(*c).c_str());
    }
}


/**
 * Output the type name definitions.
 */
template <typename InputIterator>
void OutputTypeNames(FILE* outFile, InputIterator first, InputIterator last)
{
  fprintf(outFile,
          "\n");
  
  for(InputIterator t = first; t != last; ++t)
    {
    String type = *t;
    fprintf(outFile,
            "declare_TypeNameOf(%s);\n"
            " define_TypeNameOf(%s);\n",
            type.c_str(),
            type.c_str());
    }
  
  fprintf(outFile,
          "\n");
}


/**
 * Output the code that appears at the bottom of each generated wrapper file.
 */
void OutputFileFooter(FILE* outFile)
{
  fprintf(outFile,
          "\n"
          "_wrap_NAMESPACE_END\n"
          "\n");
}


// Remove the reference type and any qualifiers like const
String GetNonReferenceType(const Type* t)
{
  String ret = t->GetNameWithoutCV();
  return ret.substr(0, ret.length()-1);
}

bool IsBuiltin(String const& type)
{
  const char* builtin[] = { "double&", "int&", "long&", "unsigned&",
			    "unsigned long&", "unsigned long int&",
			    "short&", "unsigned short&", "char&", 0
  };
  cerr  << "check built in " << type.c_str() << endl;
  for(int i=0; builtin[i]; i++)
    {
    if(type == builtin[i])
      {
      return true;
      }
    }
  return false;
}

bool IsBuiltinConstReference(const ReferenceType* t)
{
  if(t->GetReferencedType()->IsConst() )
    {
    String realtype = t->GetNameWithoutCV();
    if( IsBuiltin(realtype))
      {
      cout << "IsBuiltin " << t->GetName() << " " << realtype.c_str() << endl;
      return true;
      }
    
    }
  return false;
}

//----------------------------------------------------------------------------
// Begin temporary area.  Code in this area must be changed later to handle
// overload resolution correctly.  Also must be updated to deal with
// default arguments.

/**
 * Output the test inside a method wrapper's if statement for
 * whether the input indends this function to be called.
 */
void OutputMethodTest(FILE* outFile, const Function* func, const char* indent)
{
  // Make sure argument count is correct.
  fprintf(outFile, "(objc == %d+2)", func->GetArgumentCount());
  // Check the argument types.
  int argNum = 0;
  for(ArgumentsIterator a = func->GetArguments().begin();
      a != func->GetArguments().end(); ++a)
    {
    fprintf(outFile, "\n%s&& ", indent);
    const Type* t = (*a)->GetType();
    if(t->IsPointerType())
      {
      const PointerType* pt = (PointerType*)t;
      String innerType = pt->GetPointedToType()->GetName();
      // if it is a char* then do not check the type, but
      // rather just use the tcl string later
      if(innerType != "char")
	{
	fprintf(outFile, "ObjectCanBePointerTo<%s >::Test(interp, objv[%d])",
		innerType.c_str(), argNum+2);
	}
      else
	{
	fprintf(outFile, "true");
	}
      }
    else if(t->IsReferenceType())
      {
      const ReferenceType* rt = (ReferenceType*)t;
      // if it is a const reference, then treat it like a real type
      if(IsBuiltinConstReference(rt))
	{
	fprintf(outFile, "ObjectCanBe<%s >::Test(interp, objv[%d])",
		GetNonReferenceType(t).c_str(), argNum+2);
	}
      else
	{
	String innerType = rt->GetReferencedType()->GetName();
	fprintf(outFile, "ObjectCanBeReferenceTo<%s >::Test(interp, objv[%d])",
		innerType.c_str(), argNum+2);
	}
      }
    else
      {
      fprintf(outFile, "ObjectCanBe<%s >::Test(interp, objv[%d])",
              t->GetName().c_str(), argNum+2);
      }

    ++argNum;
    }
}


/**
 * Output the actual call to a method in its wrapper.
 */
void OutputMethodCall(FILE* outFile, const Function* func)
{
  const Type* returnType=NULL;
  if(func->GetReturns() && func->GetReturns()->GetType())
    {
    if(func->GetReturns()->GetType()->GetName() != "void")
      returnType = func->GetReturns()->GetType();
    }
  
  fprintf(outFile, "    ");
  if(returnType)
    {
    if(returnType->IsPointerType())
      {
      const PointerType* pt = (PointerType*)returnType;
      // BUG FIX ME, I have to get rid of const here for some reason
      String innerType = pt->GetPointedToType()->GetNameWithoutCV();
      // ReturnPointerTo<const type> causes a syntax error...
      fprintf(outFile, "ReturnPointerTo<%s >::From(interp,\n      ",
              innerType.c_str());
      // if it is a pointer, then use const_cast for the case of const char*
      if(innerType == "char")
	{
	fprintf(outFile, "const_cast<%s*>(",
		innerType.c_str());
	}
      else
	{
	fprintf(outFile, "(",
		innerType.c_str());
	}
      
      }
    else if(returnType->IsReferenceType())
      {
      const ReferenceType* rt = (ReferenceType*)returnType;
      // if it is a const reference, then treat it like a real type
      if(IsBuiltin(rt->GetName())) // GetNonReferenceType(rt)))
	{
	fprintf(outFile, "ReturnInstanceOf<%s >::From(interp,\n      ",
		GetNonReferenceType(returnType).c_str());
	}
      else
	{
	String innerType = rt->GetReferencedType()->GetNameWithoutCV();
	fprintf(outFile, "ReturnReferenceTo<%s >::From(interp,\n      ",
		innerType.c_str());
	}
      
      }
    else
      {
      fprintf(outFile, "ReturnInstanceOf<%s >::From(interp,\n      ",
              returnType->GetName().c_str());
      }
    }
  fprintf(outFile, "instance.%s(",
          func->GetCallName().c_str());

  int argNum = 0;
  for(ArgumentsIterator a = func->GetArguments().begin();
      a != func->GetArguments().end();++a)
    {
    const Type* t = (*a)->GetType();
    if(t->IsPointerType())
      {
      const PointerType* pt = (PointerType*)t;
      String innerType = pt->GetPointedToType()->GetName();
      // For char* arguments, just use the tcl string and don't convert
      if(innerType == "char")
	{
	fprintf(outFile, "objv[%d]->bytes",  argNum+2);
	}
      else
	{
	fprintf(outFile, "ObjectAsPointerTo<%s >::Get(interp, objv[%d])",
		innerType.c_str(), argNum+2);
	}
      }
    else if(t->IsReferenceType())
      {
      const ReferenceType* rt = (ReferenceType*)t;
      // if it is a const reference, then treat it like a real type
      if(IsBuiltinConstReference(rt))
	{
	fprintf(outFile, "ObjectAs<%s >::Get(interp, objv[%d])",
		GetNonReferenceType(t).c_str(), argNum+2);
	}
      else
	{
	String innerType = rt->GetReferencedType()->GetName();
	fprintf(outFile, "ObjectAsReferenceTo<%s >::Get(interp, objv[%d])",
		innerType.c_str(), argNum+2);
	}
      }
    else
      {
      fprintf(outFile, "ObjectAs<%s >::Get(interp, objv[%d])",
              t->GetName().c_str(), argNum+2);
      }

    ++argNum;
    if(argNum != func->GetArgumentCount())
      fprintf(outFile, ",\n        ");
    }
  if(returnType)
    {
    // close the const cast
    if(returnType->IsPointerType())
      {
      fprintf(outFile, ")");
      }
    fprintf(outFile, "));\n");
    }
  else
    {
    fprintf(outFile, ");\n");
    }
}

/**
 *  Check to see if the function can be wrapped in tcl.
 *  Currently functions that pass pointers to functions can not
 *  be wrapped.
 */
bool FunctionCanBeWrapped(Function const* f)
{ 
  for(ArgumentsIterator a = f->GetArguments().begin();
      a != f->GetArguments().end();
      ++a)
    {
    const Type* t = (*a)->GetType();
    if(t->IsFunctionPointer())
      {
      return false;
      }
    }
  return true;
}


/**
 * Wrap a group of methods with the same name.  Code to do
 * parameter-type-based overload resolution must be generated.
 *
 * Dereferencing an input iterator must produce a Function*.
 */
template <typename InputIterator>
void wrapMethodGroup(FILE* outFile, const Class* aClass,
                     InputIterator first, InputIterator last)
{
  if(first == last) return;
  
  InputIterator currentFunctionIterator = first;
  const Function* func = *currentFunctionIterator++;

  fprintf(outFile,
          "/**\n"
          " * Wrap %s::%s() methods.\n"
          " */\n",
          aClass->GetQualifiedName().c_str(),
          func->GetCallName().c_str());
  fprintf(outFile,
          "static void Method_%s__%s(\n"
          "  %s& instance, Tcl_Interp* interp, int objc, Tcl_Obj* CONST objv[])\n"
          "{\n",
          GetWrapName(aClass).c_str(),
          GetWrapName(func).c_str(),
          aClass->GetQualifiedName().c_str());
  
  fprintf(outFile,
          "  if(");
  OutputMethodTest(outFile, func, "     ");
  fprintf(outFile,
          ")\n");
  fprintf(outFile,
          "    {\n");
  OutputMethodCall(outFile, func);
  fprintf(outFile,
          "    }\n");
  
  while(currentFunctionIterator != last)
    {
    func = *currentFunctionIterator++;
    fprintf(outFile,
	    "  else if(");
    OutputMethodTest(outFile, func, "          ");
    fprintf(outFile,
	    ")\n");
    fprintf(outFile,
	    "    {\n");
    OutputMethodCall(outFile, func); 
    fprintf(outFile,
	    "    }\n");
    }
  
  fprintf(outFile,
          "  else\n"
          "    {\n"
          "    throw UnknownMethodError(interp, \"%s\", \"%s\", objv+2, objc-2);\n"
          "    }\n",
          aClass->GetQualifiedName().c_str(),
          func->GetCallName().c_str());  
  
  fprintf(outFile,
          "}\n"
          "\n");
}

// End temporary area.
//----------------------------------------------------------------------------


/**
 * Generate the code for the main wrapper function of the class.  The function
 * generated will be called by the Tcl interpreter, and is responsible for
 * determining which actual method wrapper to call.
 */
void OutputMethodWrapper(FILE* outFile,const Class* aClass)
{
  fprintf(outFile,
          "/**\n"
          " * Main wrapper function for class:\n"
          " * %s\n"
          " */\n",
          aClass->GetQualifiedName().c_str());
  fprintf(outFile,
          "static int WrapperFor_%s(\n"
          "  ClientData clientData, Tcl_Interp* interp, int objc, Tcl_Obj* CONST objv[])\n"
          "{\n",
          GetWrapName(aClass).c_str());
  fprintf(outFile,
          "  // Get the command name.  This will be either an instance name,\n"
          "  // or the name of type %s.\n"
          "  String commandName = Tcl_GetStringFromObj(objv[0], NULL);\n"
          "  \n"
          "  if(commandName == \"%s\")\n"
          "    {\n"
          "    // The command is the name of type %s.\n"
          "    // Create a new instance.\n"
          "    char* instanceName = Tcl_GetStringFromObj(objv[1], NULL);\n"
          "    \n"
          "    InstanceSet(instanceName, NewObjectOf<%s >::Create(), \"%s\");\n"
          "    Tcl_CreateObjCommand(interp, instanceName, WrapperFor_%s,\n"
          "                         (ClientData)NULL, (Tcl_CmdDeleteProc*)NULL);\n"
          "    return TCL_OK;\n"
          "    }\n",
          aClass->GetQualifiedName().c_str(),
          aClass->GetQualifiedName().c_str(),
          aClass->GetQualifiedName().c_str(),
          aClass->GetQualifiedName().c_str(),
          aClass->GetQualifiedName().c_str(),
          GetWrapName(aClass).c_str());
  fprintf(outFile,
          "  try\n"
          "    {\n"
          "    // The command is the name of an instance or reference.\n"
          "    String methodName = Tcl_GetStringFromObj(objv[1], NULL);\n"
          "    \n"
          "    if(methodMap_%s.count(methodName) > 0)\n"
          "      {\n"
          "      if(InstanceExists(commandName))\n"
          "        {\n"
          "        methodMap_%s[methodName](InstanceAs<%s >::Get(commandName), interp, objc, objv);\n"
          "        }\n"
          "      else if(ReferenceExists(commandName))\n"
          "        {\n"
          "        methodMap_%s[methodName](ReferenceAs<%s >::Get(commandName), interp, objc, objv);\n"
          "        }\n"
	  "      else\n"
          "        {\n"
          "        throw _wrap_UnknownCommandNameException(commandName);\n"
          "        }\n"
          "      }\n",  
	  GetWrapName(aClass).c_str(),
          GetWrapName(aClass).c_str(),
          aClass->GetQualifiedName().c_str(),
          GetWrapName(aClass).c_str(),
          aClass->GetQualifiedName().c_str()); 
  // output code to chain up the base classes
  fprintf(outFile,
          "  else\n"
          "    {\n"   
	  "    String methodName = Tcl_GetStringFromObj(objv[1], NULL);\n");
  for(BaseClassContainer::const_iterator i = aClass->GetBaseClasses().begin();
      i != aClass->GetBaseClasses().end(); ++i)
    {
    fprintf(outFile,
	    "    if(WrapperExists(\"%s\"))\n"
	    "      {\n"
	    "      return WrapperFunction(\"%s\")(clientData, interp, objc, objv);\n"
	    "      }\n",  (*i)->GetQualifiedName().c_str(), (*i)->GetQualifiedName().c_str());
    }
  fprintf(outFile,
	  "    throw UnknownMethodError(interp, \"%s\", methodName, objv+2, objc-2);\n"
	  "    }\n",
          aClass->GetQualifiedName().c_str(),
          aClass->GetQualifiedName().c_str());
  
  fprintf(outFile,   
          "  }\n"
          "  catch(String error)\n"
          "    {\n"
          "    ReportErrorMessage(interp, error);\n"
          "    FreeTemporaries(interp, objv, objc);\n"
          "    return TCL_ERROR;\n"
          "    }\n"
          "  \n"
          "  FreeTemporaries(interp, objv, objc);\n"
          "  return TCL_OK;\n"
          "}\n"
          "\n");
}


/**
 * Generate wrappers for the given WrapType.
 */
void WrapClass(FILE* outFile, const WrapType* wrapType)
{
  const Class* aClass = wrapType->GetClass();
  

  /**
   * Write out the specialized create and delete functions for this type,
   * if they exist.
   */
  const Create* createFunction = wrapType->GetCreate();
  if(createFunction)
    {
    createFunction->PrintFunction(outFile, wrapType->GetName());
    fprintf(outFile,
            "\n");
    }

  const Delete* deleteFunction = wrapType->GetDelete();
  if(deleteFunction)
    {
    deleteFunction->PrintFunction(outFile, wrapType->GetName());
    fprintf(outFile,
            "\n");
    }
  
  /**
   * Find all the class's public methods.
   * They will be put in groups of the same name in the method map.
   * The methods include Method s and OperatorMethod s.
   */
  MethodMap methodMap;  
  for(MethodsIterator methodItr = aClass->GetMethods().begin();
      methodItr != aClass->GetMethods().end();
      ++methodItr)
    {
    const Method* method = *methodItr;
    if((method->IsMethod() || method->IsOperatorMethod())
       && (method->GetAccess() == Public )
       && (FunctionCanBeWrapped(method) ))
      {
      methodMap[method->GetName()].push_back(method);
      }
    }

  // Generate the wrapper functions for each method group.
  for(MethodMapIterator m = methodMap.begin() ; m != methodMap.end() ; ++m)
    {
    wrapMethodGroup(outFile, aClass, m->second.begin(), m->second.end());
    }

  OutputMethodWrapper(outFile, aClass);
  
  // Write out the method map initialization function.
  fprintf(outFile,
          "/**\n"
          " * Initialize %s wrapper.\n"
          " */\n",
          aClass->GetQualifiedName().c_str());
  
  fprintf(outFile,
          "static void InitWrapper_%s(Tcl_Interp* interp)\n"
          "{\n",
          GetWrapName(aClass).c_str());
  
  for(MethodMapIterator m = methodMap.begin() ; m != methodMap.end() ; ++m)
    {
    fprintf(outFile,
            "  methodMap_%s[\"%s\"] = Method_%s__%s;\n",
            GetWrapName(aClass).c_str(),
            m->first.c_str(),
            GetWrapName(aClass).c_str(),
            GetValid_C_Identifier(m->first).c_str());
    }

  fprintf(outFile,
          "  \n"
          "  Tcl_CreateObjCommand(interp, \"%s\", WrapperFor_%s,\n"
          "                       (ClientData)NULL, (Tcl_CmdDeleteProc*)NULL);\n"
          "  \n"
          "  RegisterWrapperFunction(\"%s\", WrapperFor_%s);\n"
          "  RegisterDeleteFunction(\"%s\", OldObjectOf<%s >::Delete);\n",
          aClass->GetQualifiedName().c_str(),
          GetWrapName(aClass).c_str(),
          aClass->GetQualifiedName().c_str(),
          GetWrapName(aClass).c_str(),
          aClass->GetQualifiedName().c_str(),
          aClass->GetQualifiedName().c_str());
  fprintf(outFile,
          "}\n"
          "\n");
}


/**
 * Generate the initialization function to call all the individual
 * wrapper initalizers.
 */
void OutputInitializer(FILE* outFile, const String& fileName,
                       WrapTypesIterator first,
                       WrapTypesIterator last)
{
  fprintf(outFile,
          "/**\n"
          " * Initialize all the wrappers in this file.\n"
          " */\n"
          "void %s_Initializer(Tcl_Interp* interp)\n"
          "{\n",
          fileName.c_str());
  
  for(WrapTypesIterator w = first; w != last; ++w)
    {
    fprintf(outFile,
            "  InitWrapper_%s(interp);\n",
            GetWrapName(w->second->GetClass()).c_str());
    }
  
  fprintf(outFile,
          "}\n"
          "\n");
}                      


/**
 * Generate TCL wrappers for the types specified by the configuration.
 */
void GenerateTcl(const Namespace* globalNamespace,
                 const WrapperConfiguration* configuration,
                 const char* outputDirectory)
{
  FILE* outFile = configuration->GetOutputFile(outputDirectory);
  WrapTypesIterator first = configuration->GetWrapTypes().begin();
  WrapTypesIterator last = configuration->GetWrapTypes().end();

  // Output the header comments and wrap namespace opening.
  OutputFileHeader(outFile, first, last);

  // List and output the type name information.
  std::set<String> typeNames;
  for(WrapTypesIterator t = first; t != last; ++t)
    {
    AddTypeNamesFromClass(t->second->GetClass(),
                          inserter(typeNames, typeNames.begin()));
    }
  OutputTypeNames(outFile, typeNames.begin(), typeNames.end());

  // Output wrappers for each type.
  for(WrapTypesIterator t = first; t != last; ++t)
    {
    WrapClass(outFile, t->second);
    }  

  // Output the wrapper initialization function for this file.
  OutputInitializer(outFile, configuration->GetOutputName(),
                    first, last);
  
  // Output the footer comments and wrap namespace closing.
  OutputFileFooter(outFile);
  
  fclose(outFile);
}

