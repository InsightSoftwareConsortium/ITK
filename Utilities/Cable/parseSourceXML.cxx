#include <stack>
#include <map>
#include <string>

#include "parseSourceXML.h"

/**
 * Map from element name to its beginning handler.
 */
static std::map<String, void (*)(const Attributes&)>  beginHandlers;

/**
 * Map from element name to its ending handler.
 */
static std::map<String, void (*)(void)>  endHandlers;


/**
 * A stack of XML elements used during parsing of the XML source.
 */
static std::stack<InternalObject::Pointer>  elementStack;

/**
 * Store the global namespace.  It will also be at the bottom of the
 * element stack if the XML source is correct.
 */
static Namespace::Pointer  GlobalNamespace;


static void Initialize(void);
static void StartElement(void *, const char *, const char **);
static void EndElement(void *, const char *);


/**
 * Parse XML source from the given input.  Return a pointer to the
 * global namespace found during the parse.
 */
Namespace::Pointer ParseSourceXML(FILE* inFile)
{
  /**
   * Prepare the XML parser.
   */
  char buf[BUFSIZ];
  XML_Parser parser = XML_ParserCreate(NULL);
  bool done = false;
  
  Initialize();

  XML_SetElementHandler(parser, StartElement, EndElement);

  /**
   * Parse the entire XML source.
   */
  while(!done)
    {
    size_t len = fread(buf, 1, sizeof(buf), inFile);
    done = len < sizeof(buf);
    if (!XML_Parse(parser, buf, len, done))
      {
      fprintf(stderr, "ParseSourceXML(): %s at line %d\n",
              XML_ErrorString(XML_GetErrorCode(parser)),
              XML_GetCurrentLineNumber(parser));
      exit(1);
      }
    }

  /**
   * Done with the parser.
   */
  XML_ParserFree(parser);
  
  /**
   * The element stack should now have just the global namespace left
   * on top.
   */
  if(elementStack.size() > 1)
    {
    fprintf(stderr,
            "ParseSourceXML(): Input ends with unfinished elements!\n");
    exit(1);
    }

  fclose(inFile);
  
  /**
   * Return the global namespace.
   */
  return GlobalNamespace;
}


/**
 * Get the top of the element stack.
 */
static InternalObject::Pointer CurrentElement(void)
{
  return elementStack.top();
}


/**
 * An unexpected element type is on top of the stack.
 */
class ElementStackTypeException: public ParseException
{
public:
  ElementStackTypeException(const char* file, int line,
                            const char* e, TypeOfObject t):
    ParseException(file, line), m_Expected(e), m_Got(t) {}
  void Print(FILE* f) const
    {
      fprintf(f, "Expected \"%s\" on top of element stack, but got %d.\n",
              m_Expected.c_str(), m_Got);
    }
private:
  String m_Expected;
  TypeOfObject m_Got;
};


/**
 * Get the top of the element stack as a context pointer.
 */
static Context::Pointer CurrentContext(void)
{
  TypeOfObject t = elementStack.top()->GetTypeOfObject();
  if((t != Namespace_id)
     && (t != Class_id)
     && (t != Struct_id)
     && (t != Union_id))
    throw ElementStackTypeException(__FILE__, __LINE__, "Any Context", t);
  
  return (Context*)elementStack.top().RealPointer();
}


/**
 * Get the top of the element stack cast as a Namespace.
 */
static Namespace::Pointer CurrentNamespace(void)
{
  TypeOfObject t = elementStack.top()->GetTypeOfObject();
  if((t != Namespace_id))
    throw ElementStackTypeException(__FILE__, __LINE__, "Namespace", t);
  
  return (Namespace*)elementStack.top().RealPointer();
}


/**
 * Get the top of the element stack cast as a Class.
 */
static Class::Pointer CurrentClass(void)
{
  TypeOfObject t = elementStack.top()->GetTypeOfObject();
  if((t != Class_id)
     && (t != Struct_id)
     && (t != Union_id))
    throw ElementStackTypeException(__FILE__, __LINE__, "Any Class", t);
  
  return (Class*)elementStack.top().RealPointer();
}


/**
 * Get the current type off the top of the element stack.
 */
static Type::Pointer CurrentType(void)
{
  TypeOfObject t = elementStack.top()->GetTypeOfObject();
  if((t != NamedType_id)
     && (t != PointerType_id)
     && (t != ReferenceType_id)
     && (t != FunctionType_id)
     && (t != MethodType_id)
     && (t != OffsetType_id)
     && (t != ArrayType_id))
    throw ElementStackTypeException(__FILE__, __LINE__, "Any Type", t);
  
  return (Type*)elementStack.top().RealPointer();
}


/**
 * Get the current function off the top of the element stack.
 */
static Function::Pointer CurrentFunction(void)
{
  TypeOfObject t = elementStack.top()->GetTypeOfObject();
  if((t != Function_id)
    && (t != Method_id)
    && (t != Constructor_id)
    && (t != Destructor_id)
    && (t != Converter_id)
    && (t != OperatorFunction_id)
    && (t != OperatorMethod_id))
    throw ElementStackTypeException(__FILE__, __LINE__, "Any Function", t);

  return (Function*)elementStack.top().RealPointer();
}


/**
 * Get the current argument off the top of the element stack.
 */
static Argument::Pointer CurrentArgument(void)
{
  TypeOfObject t = elementStack.top()->GetTypeOfObject();
  if((t != Argument_id))
    throw ElementStackTypeException(__FILE__, __LINE__, "Argument", t);

  return (Argument*)elementStack.top().RealPointer();
}


/**
 * Get the current type off the top of the stack as a PointerType.
 */
static PointerType::Pointer CurrentPointerType(void)
{
  TypeOfObject t = elementStack.top()->GetTypeOfObject();
  if((t != PointerType_id))
    throw ElementStackTypeException(__FILE__, __LINE__, "PointerType", t);
  
  return (PointerType*)elementStack.top().RealPointer();
}


/**
 * Get the current type off the top of the stack as a ReferenceType.
 */
static ReferenceType::Pointer CurrentReferenceType(void)
{
  TypeOfObject t = elementStack.top()->GetTypeOfObject();
  if((t != ReferenceType_id))
    throw ElementStackTypeException(__FILE__, __LINE__, "ReferenceType", t);
  
  return (ReferenceType*)elementStack.top().RealPointer();
}


/**
 * Get the current type off the top of the stack as a FunctionType.
 */
static FunctionType::Pointer CurrentFunctionType(void)
{
  TypeOfObject t = elementStack.top()->GetTypeOfObject();
  if((t != FunctionType_id)
     && (t != MethodType_id))
    throw ElementStackTypeException(__FILE__, __LINE__, "Any FunctionType", t);
  
  return (FunctionType*)elementStack.top().RealPointer();
}


/**
 * Get the current type off the top of the stack as a MethodType.
 */
static MethodType::Pointer CurrentMethodType(void)
{
  TypeOfObject t = elementStack.top()->GetTypeOfObject();
  if(t != MethodType_id)
    throw ElementStackTypeException(__FILE__, __LINE__, "MethodType", t);
  
  return (MethodType*)elementStack.top().RealPointer();
}


/**
 * Get the current type off the top of the stack as a OffsetType.
 */
static OffsetType::Pointer CurrentOffsetType(void)
{
  TypeOfObject t = elementStack.top()->GetTypeOfObject();
  if((t != OffsetType_id))
    throw ElementStackTypeException(__FILE__, __LINE__, "OffsetType", t);
  
  return (OffsetType*)elementStack.top().RealPointer();
}


/**
 * Get the current type off the top of the stack as a ArrayType.
 */
static ArrayType::Pointer CurrentArrayType(void)
{
  TypeOfObject t = elementStack.top()->GetTypeOfObject();
  if((t != ArrayType_id))
    throw ElementStackTypeException(__FILE__, __LINE__, "ArrayType", t);
  
  return (ArrayType*)elementStack.top().RealPointer();
}


/**
 * Push a new element onto the element stack.
 */
static void PushElement(InternalObject* element)
{
  elementStack.push(element);
}

/**
 * Pop the top off the element stack.
 */
static void PopElement(void)
{
  elementStack.pop();

  // Sanity check.
  if(elementStack.empty())
    {
    fprintf(stderr, "Global namespace popped from stack!\n");
    exit(1);
    }
}



/*@{
 * String containing a common attribute value.
 */
static const String access_public("public");
static const String access_protected("protected");
static const String access_private("private");
//@}


/**
 * Define all the element handlers.
 */

/**
 * Begin handler for GlobalNamespace element.
 * The global namespace is the beginning and ending of the document.
 * Push it onto the bottom of the stack.
 */
static void begin_GlobalNamespace(const Attributes& atts)
{
  GlobalNamespace = Namespace::New("");
  PushElement(GlobalNamespace);
}

/**
 * End handler for GlobalNamespace element.
 */
static void end_GlobalNamespace(void)
{
  // We want the global namespace left on the stack.  Don't pop it off.
}


/**
 * Begin handler for Namespace element.
 * Add a new Namespace to the current context, which must be a Namespace.
 */
static void begin_Namespace(const Attributes& atts)
{
  String name = atts.Get("name");
  Namespace::Pointer newNamespace = Namespace::New(name);
  
  CurrentNamespace()->AddNamespace(newNamespace);
  PushElement(newNamespace);
}

/**
 * End handler for Namespace element.
 */
static void end_Namespace(void)
{
  PopElement();
}


/**
 * Begin handler for NamespaceAlias element.
 */
static void begin_NamespaceAlias(const Attributes& atts)
{
  UnimplementedNameHolder::Pointer newUnimplementedNameHolder
    = UnimplementedNameHolder::New();
  
  PushElement(newUnimplementedNameHolder);
}

/**
 * End handler for NamespaceAlias element.
 */
static void end_NamespaceAlias(void)
{
  PopElement();
}


/**
 * Begin handler for Typedef element.
 */
static void begin_Typedef(const Attributes& atts)
{
  UnimplementedTypeHolder::Pointer newUnimplementedTypeHolder =
    UnimplementedTypeHolder::New();
  
  // Only need typedef to absorb its internal information.  It will be
  // lost when it is popped off the stack by end_Typedef().
  PushElement(newUnimplementedTypeHolder);
}

/**
 * End handler for Typedef element.
 */
static void end_Typedef(void)
{
  PopElement();
}


/**
 * Begin handler for Class element.
 * Add a new Class to the current context, which can be a Namespace, Class,
 * Struct, or Union.
 */
static void begin_Class(const Attributes& atts)
{
  String name = atts.Get("name");
  String accessStr = atts.Get("access");
  Access access;

  if(accessStr == access_public)         access = Public;
  else if(accessStr == access_protected) access = Protected;
  else                                   access = Private;
  
  Class::Pointer newClass = Class::New(name, access);
  
  CurrentContext()->AddClass(newClass);
  PushElement(newClass);
}

/**
 * End handler for Class element.
 */
static void end_Class(void)
{
  PopElement();
}


/**
 * Begin handler for Struct element.
 * Add a new Struct to the current context, which can be a Namespace, Class,
 * Struct, or Union.
 */
static void begin_Struct(const Attributes& atts)
{
  String name = atts.Get("name");
  String accessStr = atts.Get("access");
  Access access;

  if(accessStr == access_public)         access = Public;
  else if(accessStr == access_protected) access = Protected;
  else                                   access = Private;
  
  Struct::Pointer newStruct = Struct::New(name, access);

  CurrentContext()->AddClass(newStruct);
  PushElement(newStruct);
}

/**
 * End handler for Struct element.
 */
static void end_Struct(void)
{
  PopElement();
}


/**
 * Begin handler for Union element.
 * Add a new Union to the current context, which can be a Namespace, Class,
 * Struct, or Union.
 */
static void begin_Union(const Attributes& atts)
{
  String name = atts.Get("name");
  String accessStr = atts.Get("access");
  Access access;

  if(accessStr == access_public)         access = Public;
  else if(accessStr == access_protected) access = Protected;
  else                                   access = Private;

  Union::Pointer newUnion = Union::New(name, access);
  
  CurrentContext()->AddClass(newUnion);
  PushElement(newUnion);
}

/**
 * End handler for Union element.
 */
static void end_Union(void)
{
  PopElement();
}


/**
 * Begin handler for Constructor element.
 * Add a Constructor the the current Class, Struct, or Union.
 */
static void begin_Constructor(const Attributes& atts)
{
  String accessStr = atts.Get("access");
  Access access;

  if(accessStr == access_public)         access = Public;
  else if(accessStr == access_protected) access = Protected;
  else                                   access = Private;
  
  Constructor::Pointer newConstructor = Constructor::New(access);

  CurrentClass()->AddMethod(newConstructor);
  PushElement(newConstructor);
}

/**
 * End handler for Constructor element.
 */
static void end_Constructor(void)
{
  PopElement();
}


/**
 * Begin handler for Destructor element.
 * Add a Destructor the the current Class, Struct, or Union.
 */
static void begin_Destructor(const Attributes& atts)
{
  String accessStr = atts.Get("access");
  Access access;
  
  if(accessStr == access_public)         access = Public;
  else if(accessStr == access_protected) access = Protected;
  else                                   access = Private;
  
  Destructor::Pointer newDestructor = Destructor::New(access);
  
  CurrentClass()->AddMethod(newDestructor);
  PushElement(newDestructor);
}

/**
 * End handler for Destructor element.
 */
static void end_Destructor(void)
{
  PopElement();
}


/**
 * Begin handler for Converter element.
 * Add a Converter to the current Class, Struct, or Union.
 */
static void begin_Converter(const Attributes& atts)
{
  String accessStr = atts.Get("access");
  Access access;
  
  if(accessStr == access_public)         access = Public;
  else if(accessStr == access_protected) access = Protected;
  else                                   access = Private;
  
  Converter::Pointer newConverter = Converter::New(access);
  
  CurrentClass()->AddMethod(newConverter);
  PushElement(newConverter);
}

/**
 * End handler for Converter element.
 */
static void end_Converter(void)
{
  PopElement();
}


/**
 * Begin handler for OperatorFunction element.
 * Add an OperatorFunction to the current Namespace.
 */
static void begin_OperatorFunction(const Attributes& atts)
{
  String name = atts.Get("name");  
  OperatorFunction::Pointer newOperatorFunction = OperatorFunction::New(name);
  
  CurrentNamespace()->AddFunction(newOperatorFunction);
  PushElement(newOperatorFunction);
}

/**
 * End handler for OperatorFunction element.
 */
static void end_OperatorFunction(void)
{
  PopElement();
}


/**
 * Begin handler for OperatorMethod element.
 * Add an OperatorMethod to the current Class, Struct, or Union.
 */
static void begin_OperatorMethod(const Attributes& atts)
{
  String name = atts.Get("name");  
  String accessStr = atts.Get("access");
  Access access;
  
  if(accessStr == access_public)         access = Public;
  else if(accessStr == access_protected) access = Protected;
  else                                   access = Private;
  
  OperatorMethod::Pointer newOperatorMethod = OperatorMethod::New(name, access);
  
  CurrentClass()->AddMethod(newOperatorMethod);
  PushElement(newOperatorMethod);
}

/**
 * End handler for OperatorMethod element.
 */
static void end_OperatorMethod(void)
{
  PopElement();
}


/**
 * Begin handler for Method element.
 * Add a Method to the current Class, Struct, or Union.
 */
static void begin_Method(const Attributes& atts)
{
  String name = atts.Get("name");  
  String accessStr = atts.Get("access");
  bool is_static = atts.GetAsBoolean("static");
  Access access;
  
  if(accessStr == access_public)         access = Public;
  else if(accessStr == access_protected) access = Protected;
  else                                   access = Private;
  
  Method::Pointer newMethod = Method::New(name, access, is_static);
  
  CurrentClass()->AddMethod(newMethod);
  PushElement(newMethod);
}

/**
 * End handler for Method element.
 */
static void end_Method(void)
{
  PopElement();
}


/**
 * Begin handler for Function element.
 * Add a Function to the current Namespace.
 */
static void begin_Function(const Attributes& atts)
{
  String name = atts.Get("name");  
  Function::Pointer newFunction = Function::New(name);
  
  CurrentNamespace()->AddFunction(newFunction);
  PushElement(newFunction);
}

/**
 * End handler for Function element.
 */
static void end_Function(void)
{
  PopElement();
}


/**
 * Begin handler for Argument element.
 * Add an argument to the function (or function type) currently being defined.
 */
static void begin_Argument(const Attributes& atts)
{  
  String name = atts.Get("name");  
  Argument::Pointer newArgument = Argument::New(name);

  TypeOfObject t = CurrentElement()->GetTypeOfObject();

  if((t == FunctionType_id)
     || (t == MethodType_id))
    {
    CurrentFunctionType()->AddArgument(newArgument);
    }
  else
    {
    CurrentFunction()->AddArgument(newArgument);
    }
  PushElement(newArgument);
}


/**
 * End handler for Argument element.
 */
static void end_Argument(void)
{
  PopElement();
}


/**
 * Begin handler for Returns element.
 */
static void begin_Returns(const Attributes& atts)
{
  Returns::Pointer newReturns = Returns::New();
  
  TypeOfObject t = CurrentElement()->GetTypeOfObject();

  if((t == FunctionType_id)
     || (t == MethodType_id))
    {
    CurrentFunctionType()->SetReturns(newReturns);
    }
  else
    {
    CurrentFunction()->SetReturns(newReturns);
    }
  
  PushElement(newReturns);
}

/**
 * End handler for Returns element.
 */
static void end_Returns(void)
{
  PopElement();
}


/**
 * Begin handler for DefaultArgument element.
 */
static void begin_DefaultArgument(const Attributes& atts)
{
}

/**
 * End handler for DefaultArgument element.
 */
static void end_DefaultArgument(void)
{
}


/**
 * Begin handler for Ellipsis element.
 * Set the ellipsis flag of the current function.
 */
static void begin_Ellipsis(const Attributes& atts)
{
  TypeOfObject t = CurrentElement()->GetTypeOfObject();

  if((t == FunctionType_id)
     || (t == MethodType_id))
    {
    CurrentFunctionType()->SetEllipsis(true);
    }
  else
    {
    CurrentFunction()->SetEllipsis(true);
    }
}

/**
 * End handler for Ellipsis element.
 */
static void end_Ellipsis(void)
{
}


/**
 * Begin handler for Variable element.
 */
static void begin_Variable(const Attributes& atts)
{
  UnimplementedTypeHolder::Pointer newUnimplementedTypeHolder =
    UnimplementedTypeHolder::New();
  
  // Only need typedef to absorb its internal information.  It will be
  // lost when it is popped off the stack by end_Variable().
  PushElement(newUnimplementedTypeHolder);
}

/**
 * End handler for Variable element.
 */
static void end_Variable(void)
{
  PopElement();
}


/**
 * Begin handler for Initializer element.
 */
static void begin_Initializer(const Attributes& atts)
{
}

/**
 * End handler for Initializer element.
 */
static void end_Initializer(void)
{
}


/**
 * Begin handler for Field element.
 */
static void begin_Field(const Attributes& atts)
{
  UnimplementedTypeHolder::Pointer newUnimplementedTypeHolder =
    UnimplementedTypeHolder::New();
  
  // Only need typedef to absorb its internal information.  It will be
  // lost when it is popped off the stack by end_Field().
  PushElement(newUnimplementedTypeHolder);
}

/**
 * End handler for Field element.
 */
static void end_Field(void)
{
  PopElement();
}


/**
 * Begin handler for Enum element.
 */
static void begin_Enum(const Attributes& atts)
{
  UnimplementedTypeHolder::Pointer newUnimplementedTypeHolder =
    UnimplementedTypeHolder::New();
  
  // Only need typedef to absorb its internal information.  It will be
  // lost when it is popped off the stack by end_Enum().
  PushElement(newUnimplementedTypeHolder);
}

/**
 * End handler for Enum element.
 */
static void end_Enum(void)
{
  PopElement();
}


/**
 * Begin handler for NamedType element.
 */
static void begin_NamedType(const Attributes& atts)
{
  NamedType::Pointer newNamedType = NamedType::New();

  CurrentElement()->SetInternalType(newNamedType);
  PushElement(newNamedType);
}

/**
 * End handler for NamedType element.
 */
static void end_NamedType(void)
{
  PopElement();  
}


/**
 * Begin handler for PointerType element.
 */
static void begin_PointerType(const Attributes& atts)
{
  PointerType::Pointer newPointerType = PointerType::New();
  
  CurrentElement()->SetInternalType(newPointerType);
  PushElement(newPointerType);
}

/**
 * End handler for PointerType element.
 */
static void end_PointerType(void)
{
  PopElement();
}


/**
 * Begin handler for ReferenceType element.
 */
static void begin_ReferenceType(const Attributes& atts)
{
  ReferenceType::Pointer newReferenceType = ReferenceType::New();
  
  CurrentElement()->SetInternalType(newReferenceType);
  PushElement(newReferenceType);
}

/**
 * End handler for ReferenceType element.
 */
static void end_ReferenceType(void)
{
  PopElement();
}


/**
 * Begin handler for FunctionType element.
 */
static void begin_FunctionType(const Attributes& atts)
{
  FunctionType::Pointer newFunctionType = FunctionType::New();
  
  CurrentElement()->SetInternalType(newFunctionType);
  PushElement(newFunctionType);
}

/**
 * End handler for FunctionType element.
 */
static void end_FunctionType(void)
{
  PopElement();
}


/**
 * Begin handler for MethodType element.
 */
static void begin_MethodType(const Attributes& atts)
{
  MethodType::Pointer newMethodType = MethodType::New();
  
  CurrentElement()->SetInternalType(newMethodType);
  PushElement(newMethodType);
}

/**
 * End handler for MethodType element.
 */
static void end_MethodType(void)
{
  PopElement();
}


/**
 * Begin handler for OffsetType element.
 */
static void begin_OffsetType(const Attributes& atts)
{
  OffsetType::Pointer newOffsetType = OffsetType::New();
  
  CurrentElement()->SetInternalType(newOffsetType);
  PushElement(newOffsetType);
}

/**
 * End handler for OffsetType element.
 */
static void end_OffsetType(void)
{
  PopElement();
}


/**
 * Begin handler for ArrayType element.
 */
static void begin_ArrayType(const Attributes& atts)
{
  int min = atts.GetAsInteger("min");
  int max = atts.GetAsInteger("max");
  ArrayType::Pointer newArrayType = ArrayType::New(min, max);
  
  CurrentElement()->SetInternalType(newArrayType);
  PushElement(newArrayType);
}

/**
 * End handler for ArrayType element.
 */
static void end_ArrayType(void)
{
  PopElement();
}


/**
 * Begin handler for QualifiedName element.
 */
static void begin_QualifiedName(const Attributes& atts)
{
  String name = atts.Get("name");
  QualifiedName::Pointer newQualifiedName = QualifiedName::New(name);

  CurrentElement()->SetInternalQualifiedName(newQualifiedName);
}

/**
 * End handler for QualifiedName element.
 */
static void end_QualifiedName(void)
{
}


/**
 * Begin handler for NameQualifier element.
 */
static void begin_NameQualifier(const Attributes& atts)
{
  String name = atts.Get("name");
  NameQualifier::Pointer newNameQualifier = NameQualifier::New(name);

  CurrentElement()->SetInternalQualifiedName(newNameQualifier);
  PushElement(newNameQualifier);
}

/**
 * End handler for NameQualifier element.
 */
static void end_NameQualifier(void)
{
  PopElement();
}


/**
 * Begin handler for BaseClass element.
 */
static void begin_BaseClass(const Attributes& atts)
{
  String accessStr = atts.Get("access");
  Access access;
  
  if(accessStr == access_public)         access = Public;
  else if(accessStr == access_protected) access = Protected;
  else                                   access = Private;
  
  BaseClass::Pointer newBaseClass = BaseClass::New(access);

  CurrentClass()->AddBaseClass(newBaseClass);
  PushElement(newBaseClass);
}

/**
 * End handler for BaseClass element.
 */
static void end_BaseClass(void)
{
  PopElement();
}


/**
 * Begin handler for BaseType element.
 */
static void begin_BaseType(const Attributes& atts)
{
  BaseType::Pointer newBaseType = BaseType::New();

  TypeOfObject t = CurrentElement()->GetTypeOfObject();
  
  if(t == MethodType_id)
    {
    CurrentMethodType()->SetBaseType(newBaseType);
    }
  else
    {
    CurrentOffsetType()->SetBaseType(newBaseType);
    }
  
  PushElement(newBaseType);
}

/**
 * End handler for BaseType element.
 */
static void end_BaseType(void)
{
  PopElement();
}


/**
 * Begin handler for Instantiation element.
 */
static void begin_Instantiation(const Attributes& atts)
{
}

/**
 * End handler for Instantiation element.
 */
static void end_Instantiation(void)
{
}


/**
 * Begin handler for TemplateArgument element.
 */
static void begin_TemplateArgument(const Attributes& atts)
{
}

/**
 * End handler for TemplateArgument element.
 */
static void end_TemplateArgument(void)
{
}


/**
 * Begin handler for External element.
 */
static void begin_External(const Attributes& atts)
{
}

/**
 * End handler for External element.
 */
static void end_External(void)
{
}


/**
 * Begin handler for IncompleteType element.
 */
static void begin_IncompleteType(const Attributes& atts)
{
}

/**
 * End handler for IncompleteType element.
 */
static void end_IncompleteType(void)
{
}


/**
 * Begin handler for Location element.
 */
static void begin_Location(const Attributes& atts)
{
  String file = atts.Get("file");
  int line = atts.GetAsInteger("line");

  Location::Pointer newLocation = Location::New(file, line);
  
  CurrentElement()->SetLocation(newLocation);
}

/**
 * End handler for Location element.
 */
static void end_Location(void)
{
}


/**
 * Begin handler for CV_Qualifiers element.
 */
static void begin_CV_Qualifiers(const Attributes& atts)
{
}

/**
 * End handler for CV_Qualifiers element.
 */
static void end_CV_Qualifiers(void)
{
}


/**
 * Begin handler for Unimplemented element.
 */
static void begin_Unimplemented(const Attributes& atts)
{
}

/**
 * End handler for Unimplemented element.
 */
static void end_Unimplemented(void)
{
}


/**
 * Setup the element handler mappings for each xml tag type.
 * Also initialize attribute information.
 */
void Initialize(void)
{
  beginHandlers["GlobalNamespace"]         = begin_GlobalNamespace;
  beginHandlers["Namespace"]               = begin_Namespace;
  beginHandlers["NamespaceAlias"]          = begin_NamespaceAlias;
  beginHandlers["Typedef"]                 = begin_Typedef;
  beginHandlers["Class"]                   = begin_Class;
  beginHandlers["Struct"]                  = begin_Struct;
  beginHandlers["Union"]                   = begin_Union;
  beginHandlers["Constructor"]             = begin_Constructor;
  beginHandlers["Destructor"]              = begin_Destructor;
  beginHandlers["Converter"]               = begin_Converter;
  beginHandlers["OperatorFunction"]        = begin_OperatorFunction;
  beginHandlers["OperatorMethod"]          = begin_OperatorMethod;
  beginHandlers["Method"]                  = begin_Method;
  beginHandlers["Function"]                = begin_Function;
  beginHandlers["Argument"]                = begin_Argument;
  beginHandlers["Returns"]                 = begin_Returns;
  beginHandlers["DefaultArgument"]         = begin_DefaultArgument;
  beginHandlers["Ellipsis"]                = begin_Ellipsis;
  beginHandlers["Variable"]                = begin_Variable;
  beginHandlers["Initializer"]             = begin_Initializer;
  beginHandlers["Field"]                   = begin_Field;
  beginHandlers["Enum"]                    = begin_Enum;
  beginHandlers["NamedType"]               = begin_NamedType;
  beginHandlers["PointerType"]             = begin_PointerType;
  beginHandlers["ReferenceType"]           = begin_ReferenceType;
  beginHandlers["FunctionType"]            = begin_FunctionType;
  beginHandlers["MethodType"]              = begin_MethodType;
  beginHandlers["OffsetType"]              = begin_OffsetType;
  beginHandlers["ArrayType"]               = begin_ArrayType;
  beginHandlers["QualifiedName"]           = begin_QualifiedName;
  beginHandlers["NameQualifier"]           = begin_NameQualifier;
  beginHandlers["BaseClass"]               = begin_BaseClass;
  beginHandlers["BaseType"]                = begin_BaseType;
  beginHandlers["Instantiation"]           = begin_Instantiation;
  beginHandlers["TemplateArgument"]        = begin_TemplateArgument;
  beginHandlers["External"]                = begin_External;
  beginHandlers["IncompleteType"]          = begin_IncompleteType;
  beginHandlers["Location"]                = begin_Location;
  beginHandlers["CV_Qualifiers"]           = begin_CV_Qualifiers;
  beginHandlers["Unimplemented"]           = begin_Unimplemented;
  
  endHandlers["GlobalNamespace"]         = end_GlobalNamespace;
  endHandlers["Namespace"]               = end_Namespace;
  endHandlers["NamespaceAlias"]          = end_NamespaceAlias;
  endHandlers["Typedef"]                 = end_Typedef;
  endHandlers["Class"]                   = end_Class;
  endHandlers["Struct"]                  = end_Struct;
  endHandlers["Union"]                   = end_Union;
  endHandlers["Constructor"]             = end_Constructor;
  endHandlers["Destructor"]              = end_Destructor;
  endHandlers["Converter"]               = end_Converter;
  endHandlers["OperatorFunction"]        = end_OperatorFunction;
  endHandlers["OperatorMethod"]          = end_OperatorMethod;
  endHandlers["Method"]                  = end_Method;
  endHandlers["Function"]                = end_Function;
  endHandlers["Argument"]                = end_Argument;
  endHandlers["Returns"]                 = end_Returns;
  endHandlers["DefaultArgument"]         = end_DefaultArgument;
  endHandlers["Ellipsis"]                = end_Ellipsis;
  endHandlers["Variable"]                = end_Variable;
  endHandlers["Initializer"]             = end_Initializer;
  endHandlers["Field"]                   = end_Field;
  endHandlers["Enum"]                    = end_Enum;
  endHandlers["NamedType"]               = end_NamedType;
  endHandlers["PointerType"]             = end_PointerType;
  endHandlers["ReferenceType"]           = end_ReferenceType;
  endHandlers["FunctionType"]            = end_FunctionType;
  endHandlers["MethodType"]              = end_MethodType;
  endHandlers["OffsetType"]              = end_OffsetType;
  endHandlers["ArrayType"]               = end_ArrayType;
  endHandlers["QualifiedName"]           = end_QualifiedName;
  endHandlers["NameQualifier"]           = end_NameQualifier;
  endHandlers["BaseClass"]               = end_BaseClass;
  endHandlers["BaseType"]                = end_BaseType;
  endHandlers["Instantiation"]           = end_Instantiation;
  endHandlers["TemplateArgument"]        = end_TemplateArgument;
  endHandlers["External"]                = end_External;
  endHandlers["IncompleteType"]          = end_IncompleteType;
  endHandlers["Location"]                = end_Location;
  endHandlers["CV_Qualifiers"]           = end_CV_Qualifiers;
  endHandlers["Unimplemented"]           = end_Unimplemented;
}



/**
 * Called when a new element is opened in the XML source.
 * Checks the tag name, and calls the appropriate handler with an
 * Attributes container.
 */
void StartElement(void *, const char *name, const char **atts)
{
  try{
  if(beginHandlers.count(name) > 0)
    {
    Attributes attributes;
    for(int i=0; atts[i] && atts[i+1] ; i += 2)
      {
      attributes.Set(atts[i], atts[i+1]);
      }
    beginHandlers[name](attributes);
    }
  else
    {
    throw UnknownElementTagException(__FILE__, __LINE__, name);
    }
  }
  catch (const ParseException& e)
    {
    e.PrintLocation(stderr);
    e.Print(stderr);
    exit(1);
    }
  catch (const String& e)
    {
    fprintf(stderr, "Caught exceptoin in StartElement():\n%s\n", e.c_str());
    exit(1);
    }
  catch (...)
    {
    fprintf(stderr, "Caught unknown exception in StartElement().\n");
    exit(1);
    }
}


/**
 * Called at the end of an element in the XML source opened when
 * StartElement was called.
 */
void EndElement(void *, const char *name)
{
  try {
  if(endHandlers.count(name) > 0)
    {
    endHandlers[name]();
    }
  else
    {
    throw UnknownElementTagException(__FILE__, __LINE__, name);
    }
  }
  catch (const ParseException& e)
    {
    e.PrintLocation(stderr);
    e.Print(stderr);
    exit(1);
    }
  catch (const String& e)
    {
    fprintf(stderr, "Caught exceptoin in EndElement():\n%s\n", e.c_str());
    exit(1);
    }
  catch (...)
    {
    fprintf(stderr, "Caught unknown exception in EndElement().\n");
    exit(1);
    }
}
