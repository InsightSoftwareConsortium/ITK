#include "xmlSourceParser.h"

namespace xml
{

/**
 * Create a new SourceParser and return a pointer to it.
 */
SourceParser::Pointer
SourceParser::New()
{
  return new Self;
}


/**
 * Constructor sets up an XML_Parser to make call-backs into this
 * SourceParser.
 */
SourceParser
::SourceParser():
  m_XML_Parser(XML_ParserCreate(NULL)),
  m_ElementStack(),
  m_GlobalNamespace(NULL)
{
  SourceParser::InitializeHandlers();
  
  XML_SetElementHandler(m_XML_Parser,
                        BeginElement_proxy,
                        EndElement_proxy);
  XML_SetUserData(m_XML_Parser,
                  this);
}


/**
 * Cleanup the XML_Parser
 */
SourceParser
::~SourceParser()
{
  XML_ParserFree(m_XML_Parser);
}


/**
 * Parse the XML from the given input stream until end-of-input is reached.
 */
void
SourceParser
::Parse(std::istream& inStream)
{
  char buf[BUFSIZ];
  bool done = false;  

  /**
   * Parse until end of input stream is reached.
   */
  while(!done)
    {
    inStream.read(buf, sizeof(buf));
    size_t len = inStream.gcount();
    done = (len < sizeof(buf));
    if(!XML_Parse(m_XML_Parser, buf, len, done))
      {
      fprintf(stderr, "SourceParser::Parse(): %s at line %d\n",
              XML_ErrorString(XML_GetErrorCode(m_XML_Parser)),
              XML_GetCurrentLineNumber(m_XML_Parser));
      }
    }
}


/**
 * Called when a new element is opened in the XML source.
 * Checks the tag name, and calls the appropriate handler with an
 * Attributes container.
 */
void
SourceParser
::BeginElement(const char *name, const char **atts)
{
  try {
  // Try to look up the handler for this element.
  BeginHandlers::iterator handler = beginHandlers.find(name);
  if(handler != beginHandlers.end())
    {
    // Found one, call it.
    Attributes attributes;
    for(int i=0; atts[i] && atts[i+1] ; i += 2)
      {
      attributes.Set(atts[i], atts[i+1]);
      }
    (this->*(handler->second))(attributes);
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
    fprintf(stderr, "Caught exceptoin in BeginElement():\n%s\n", e.c_str());
    exit(1);
    }
  catch (...)
    {
    fprintf(stderr, "Caught unknown exception in BeginElement().\n");
    exit(1);
    }
}


/**
 * Called at the end of an element in the XML source opened when
 * BeginElement was called.
 */
void
SourceParser
::EndElement(const char *name)
{
  try {
  // Try to look up the handler for this element.
  EndHandlers::iterator handler = endHandlers.find(name);
  if(handler != endHandlers.end())
    {
    // Found one, call it.
    (this->*(handler->second))();
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


/**
 * Get the top of the element stack.
 */
InternalObject::Pointer
SourceParser
::CurrentElement(void)
{
  return m_ElementStack.top();
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
Context::Pointer
SourceParser
::CurrentContext(void)
{
  TypeOfObject t = m_ElementStack.top()->GetTypeOfObject();
  if((t != Namespace_id)
     && (t != Class_id)
     && (t != Struct_id)
     && (t != Union_id))
    throw ElementStackTypeException(__FILE__, __LINE__, "Any Context", t);
  
  return (Context*)m_ElementStack.top().RealPointer();
}


/**
 * Get the top of the element stack cast as a Namespace.
 */
Namespace::Pointer
SourceParser
::CurrentNamespace(void)
{
  TypeOfObject t = m_ElementStack.top()->GetTypeOfObject();
  if((t != Namespace_id))
    throw ElementStackTypeException(__FILE__, __LINE__, "Namespace", t);
  
  return (Namespace*)m_ElementStack.top().RealPointer();
}


/**
 * Get the top of the element stack cast as a Class.
 */
Class::Pointer
SourceParser
::CurrentClass(void)
{
  TypeOfObject t = m_ElementStack.top()->GetTypeOfObject();
  if((t != Class_id)
     && (t != Struct_id)
     && (t != Union_id))
    throw ElementStackTypeException(__FILE__, __LINE__, "Any Class", t);
  
  return (Class*)m_ElementStack.top().RealPointer();
}


/**
 * Get the current type off the top of the element stack.
 */
Type::Pointer
SourceParser
::CurrentType(void)
{
  TypeOfObject t = m_ElementStack.top()->GetTypeOfObject();
  if((t != NamedType_id)
     && (t != PointerType_id)
     && (t != ReferenceType_id)
     && (t != FunctionType_id)
     && (t != MethodType_id)
     && (t != OffsetType_id)
     && (t != ArrayType_id))
    throw ElementStackTypeException(__FILE__, __LINE__, "Any Type", t);
  
  return (Type*)m_ElementStack.top().RealPointer();
}


/**
 * Get the current function off the top of the element stack.
 */
Function::Pointer
SourceParser
::CurrentFunction(void)
{
  TypeOfObject t = m_ElementStack.top()->GetTypeOfObject();
  if((t != Function_id)
    && (t != Method_id)
    && (t != Constructor_id)
    && (t != Destructor_id)
    && (t != Converter_id)
    && (t != OperatorFunction_id)
    && (t != OperatorMethod_id))
    throw ElementStackTypeException(__FILE__, __LINE__, "Any Function", t);

  return (Function*)m_ElementStack.top().RealPointer();
}


/**
 * Get the current argument off the top of the element stack.
 */
Argument::Pointer
SourceParser
::CurrentArgument(void)
{
  TypeOfObject t = m_ElementStack.top()->GetTypeOfObject();
  if((t != Argument_id))
    throw ElementStackTypeException(__FILE__, __LINE__, "Argument", t);

  return (Argument*)m_ElementStack.top().RealPointer();
}


/**
 * Get the current type off the top of the stack as a PointerType.
 */
PointerType::Pointer
SourceParser
::CurrentPointerType(void)
{
  TypeOfObject t = m_ElementStack.top()->GetTypeOfObject();
  if((t != PointerType_id))
    throw ElementStackTypeException(__FILE__, __LINE__, "PointerType", t);
  
  return (PointerType*)m_ElementStack.top().RealPointer();
}


/**
 * Get the current type off the top of the stack as a ReferenceType.
 */
ReferenceType::Pointer
SourceParser
::CurrentReferenceType(void)
{
  TypeOfObject t = m_ElementStack.top()->GetTypeOfObject();
  if((t != ReferenceType_id))
    throw ElementStackTypeException(__FILE__, __LINE__, "ReferenceType", t);
  
  return (ReferenceType*)m_ElementStack.top().RealPointer();
}


/**
 * Get the current type off the top of the stack as a FunctionType.
 */
FunctionType::Pointer
SourceParser
::CurrentFunctionType(void)
{
  TypeOfObject t = m_ElementStack.top()->GetTypeOfObject();
  if((t != FunctionType_id)
     && (t != MethodType_id))
    throw ElementStackTypeException(__FILE__, __LINE__, "Any FunctionType", t);
  
  return (FunctionType*)m_ElementStack.top().RealPointer();
}


/**
 * Get the current type off the top of the stack as a MethodType.
 */
MethodType::Pointer
SourceParser
::CurrentMethodType(void)
{
  TypeOfObject t = m_ElementStack.top()->GetTypeOfObject();
  if(t != MethodType_id)
    throw ElementStackTypeException(__FILE__, __LINE__, "MethodType", t);
  
  return (MethodType*)m_ElementStack.top().RealPointer();
}


/**
 * Get the current type off the top of the stack as a OffsetType.
 */
OffsetType::Pointer
SourceParser
::CurrentOffsetType(void)
{
  TypeOfObject t = m_ElementStack.top()->GetTypeOfObject();
  if((t != OffsetType_id))
    throw ElementStackTypeException(__FILE__, __LINE__, "OffsetType", t);
  
  return (OffsetType*)m_ElementStack.top().RealPointer();
}


/**
 * Get the current type off the top of the stack as a ArrayType.
 */
ArrayType::Pointer
SourceParser
::CurrentArrayType(void)
{
  TypeOfObject t = m_ElementStack.top()->GetTypeOfObject();
  if((t != ArrayType_id))
    throw ElementStackTypeException(__FILE__, __LINE__, "ArrayType", t);
  
  return (ArrayType*)m_ElementStack.top().RealPointer();
}


/**
 * Push a new element onto the element stack.
 */
void
SourceParser
::PushElement(InternalObject* element)
{
  m_ElementStack.push(element);
}

/**
 * Pop the top off the element stack.
 */
void
SourceParser
::PopElement(void)
{
  m_ElementStack.pop();

  // Sanity check.
  if(m_ElementStack.empty())
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
void
SourceParser
::begin_GlobalNamespace(const Attributes& atts)
{
  m_GlobalNamespace = Namespace::New("");
  PushElement(m_GlobalNamespace);
}

/**
 * End handler for GlobalNamespace element.
 */
void
SourceParser
::end_GlobalNamespace(void)
{
  // We want the global namespace left on the stack.  Don't pop it off.
}


/**
 * Begin handler for Namespace element.
 * Add a new Namespace to the current context, which must be a Namespace.
 */
void
SourceParser
::begin_Namespace(const Attributes& atts)
{
  String name = atts.Get("name");
  Namespace::Pointer newNamespace = Namespace::New(name);
  
  CurrentNamespace()->AddNamespace(newNamespace);
  PushElement(newNamespace);
}

/**
 * End handler for Namespace element.
 */
void
SourceParser
::end_Namespace(void)
{
  PopElement();
}


/**
 * Begin handler for NamespaceAlias element.
 */
void
SourceParser
::begin_NamespaceAlias(const Attributes& atts)
{
  UnimplementedNameHolder::Pointer newUnimplementedNameHolder
    = UnimplementedNameHolder::New();
  
  PushElement(newUnimplementedNameHolder);
}

/**
 * End handler for NamespaceAlias element.
 */
void
SourceParser
::end_NamespaceAlias(void)
{
  PopElement();
}


/**
 * Begin handler for Typedef element.
 */
void
SourceParser
::begin_Typedef(const Attributes& atts)
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
void
SourceParser
::end_Typedef(void)
{
  PopElement();
}


/**
 * Begin handler for Class element.
 * Add a new Class to the current context, which can be a Namespace, Class,
 * Struct, or Union.
 */
void
SourceParser
::begin_Class(const Attributes& atts)
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
void
SourceParser
::end_Class(void)
{
  PopElement();
}


/**
 * Begin handler for Struct element.
 * Add a new Struct to the current context, which can be a Namespace, Class,
 * Struct, or Union.
 */
void
SourceParser
::begin_Struct(const Attributes& atts)
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
void
SourceParser
::end_Struct(void)
{
  PopElement();
}


/**
 * Begin handler for Union element.
 * Add a new Union to the current context, which can be a Namespace, Class,
 * Struct, or Union.
 */
void
SourceParser
::begin_Union(const Attributes& atts)
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
void
SourceParser
::end_Union(void)
{
  PopElement();
}


/**
 * Begin handler for Constructor element.
 * Add a Constructor the the current Class, Struct, or Union.
 */
void
SourceParser
::begin_Constructor(const Attributes& atts)
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
void
SourceParser
::end_Constructor(void)
{
  PopElement();
}


/**
 * Begin handler for Destructor element.
 * Add a Destructor the the current Class, Struct, or Union.
 */
void
SourceParser
::begin_Destructor(const Attributes& atts)
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
void
SourceParser
::end_Destructor(void)
{
  PopElement();
}


/**
 * Begin handler for Converter element.
 * Add a Converter to the current Class, Struct, or Union.
 */
void
SourceParser
::begin_Converter(const Attributes& atts)
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
void
SourceParser
::end_Converter(void)
{
  PopElement();
}


/**
 * Begin handler for OperatorFunction element.
 * Add an OperatorFunction to the current Namespace.
 */
void
SourceParser
::begin_OperatorFunction(const Attributes& atts)
{
  String name = atts.Get("name");  
  OperatorFunction::Pointer newOperatorFunction = OperatorFunction::New(name);
  
  CurrentNamespace()->AddFunction(newOperatorFunction);
  PushElement(newOperatorFunction);
}

/**
 * End handler for OperatorFunction element.
 */
void
SourceParser
::end_OperatorFunction(void)
{
  PopElement();
}


/**
 * Begin handler for OperatorMethod element.
 * Add an OperatorMethod to the current Class, Struct, or Union.
 */
void
SourceParser
::begin_OperatorMethod(const Attributes& atts)
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
void
SourceParser
::end_OperatorMethod(void)
{
  PopElement();
}


/**
 * Begin handler for Method element.
 * Add a Method to the current Class, Struct, or Union.
 */
void
SourceParser
::begin_Method(const Attributes& atts)
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
void
SourceParser
::end_Method(void)
{
  PopElement();
}


/**
 * Begin handler for Function element.
 * Add a Function to the current Namespace.
 */
void
SourceParser
::begin_Function(const Attributes& atts)
{
  String name = atts.Get("name");  
  Function::Pointer newFunction = Function::New(name);
  
  CurrentNamespace()->AddFunction(newFunction);
  PushElement(newFunction);
}

/**
 * End handler for Function element.
 */
void
SourceParser
::end_Function(void)
{
  PopElement();
}


/**
 * Begin handler for Argument element.
 * Add an argument to the function (or function type) currently being defined.
 */
void
SourceParser
::begin_Argument(const Attributes& atts)
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
void
SourceParser
::end_Argument(void)
{
  PopElement();
}


/**
 * Begin handler for Returns element.
 */
void
SourceParser
::begin_Returns(const Attributes& atts)
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
void
SourceParser
::end_Returns(void)
{
  PopElement();
}


/**
 * Begin handler for DefaultArgument element.
 */
void
SourceParser
::begin_DefaultArgument(const Attributes& atts)
{
}

/**
 * End handler for DefaultArgument element.
 */
void
SourceParser
::end_DefaultArgument(void)
{
}


/**
 * Begin handler for Ellipsis element.
 * Set the ellipsis flag of the current function.
 */
void
SourceParser
::begin_Ellipsis(const Attributes& atts)
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
void
SourceParser
::end_Ellipsis(void)
{
}


/**
 * Begin handler for Variable element.
 */
void
SourceParser
::begin_Variable(const Attributes& atts)
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
void
SourceParser
::end_Variable(void)
{
  PopElement();
}


/**
 * Begin handler for Initializer element.
 */
void
SourceParser
::begin_Initializer(const Attributes& atts)
{
}

/**
 * End handler for Initializer element.
 */
void
SourceParser
::end_Initializer(void)
{
}


/**
 * Begin handler for Field element.
 */
void
SourceParser
::begin_Field(const Attributes& atts)
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
void
SourceParser
::end_Field(void)
{
  PopElement();
}


/**
 * Begin handler for Enum element.
 */
void
SourceParser
::begin_Enum(const Attributes& atts)
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
void
SourceParser
::end_Enum(void)
{
  PopElement();
}


/**
 * Begin handler for NamedType element.
 */
void
SourceParser
::begin_NamedType(const Attributes& atts)
{
  NamedType::Pointer newNamedType = NamedType::New();

  CurrentElement()->SetInternalType(newNamedType);
  PushElement(newNamedType);
}

/**
 * End handler for NamedType element.
 */
void
SourceParser
::end_NamedType(void)
{
  PopElement();  
}


/**
 * Begin handler for PointerType element.
 */
void
SourceParser
::begin_PointerType(const Attributes& atts)
{
  PointerType::Pointer newPointerType = PointerType::New();
  
  CurrentElement()->SetInternalType(newPointerType);
  PushElement(newPointerType);
}

/**
 * End handler for PointerType element.
 */
void
SourceParser
::end_PointerType(void)
{
  PopElement();
}


/**
 * Begin handler for ReferenceType element.
 */
void
SourceParser
::begin_ReferenceType(const Attributes& atts)
{
  ReferenceType::Pointer newReferenceType = ReferenceType::New();
  
  CurrentElement()->SetInternalType(newReferenceType);
  PushElement(newReferenceType);
}

/**
 * End handler for ReferenceType element.
 */
void
SourceParser
::end_ReferenceType(void)
{
  PopElement();
}


/**
 * Begin handler for FunctionType element.
 */
void
SourceParser
::begin_FunctionType(const Attributes& atts)
{
  FunctionType::Pointer newFunctionType = FunctionType::New();
  
  CurrentElement()->SetInternalType(newFunctionType);
  PushElement(newFunctionType);
}

/**
 * End handler for FunctionType element.
 */
void
SourceParser
::end_FunctionType(void)
{
  PopElement();
}


/**
 * Begin handler for MethodType element.
 */
void
SourceParser
::begin_MethodType(const Attributes& atts)
{
  MethodType::Pointer newMethodType = MethodType::New();
  
  CurrentElement()->SetInternalType(newMethodType);
  PushElement(newMethodType);
}

/**
 * End handler for MethodType element.
 */
void
SourceParser
::end_MethodType(void)
{
  PopElement();
}


/**
 * Begin handler for OffsetType element.
 */
void
SourceParser
::begin_OffsetType(const Attributes& atts)
{
  OffsetType::Pointer newOffsetType = OffsetType::New();
  
  CurrentElement()->SetInternalType(newOffsetType);
  PushElement(newOffsetType);
}

/**
 * End handler for OffsetType element.
 */
void
SourceParser
::end_OffsetType(void)
{
  PopElement();
}


/**
 * Begin handler for ArrayType element.
 */
void
SourceParser
::begin_ArrayType(const Attributes& atts)
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
void
SourceParser
::end_ArrayType(void)
{
  PopElement();
}


/**
 * Begin handler for QualifiedName element.
 */
void
SourceParser
::begin_QualifiedName(const Attributes& atts)
{
  String name = atts.Get("name");
  QualifiedName::Pointer newQualifiedName = QualifiedName::New(name);

  CurrentElement()->SetInternalQualifiedName(newQualifiedName);
}

/**
 * End handler for QualifiedName element.
 */
void
SourceParser
::end_QualifiedName(void)
{
}


/**
 * Begin handler for NameQualifier element.
 */
void
SourceParser
::begin_NameQualifier(const Attributes& atts)
{
  String name = atts.Get("name");
  NameQualifier::Pointer newNameQualifier = NameQualifier::New(name);

  CurrentElement()->SetInternalQualifiedName(newNameQualifier);
  PushElement(newNameQualifier);
}

/**
 * End handler for NameQualifier element.
 */
void
SourceParser
::end_NameQualifier(void)
{
  PopElement();
}


/**
 * Begin handler for BaseClass element.
 */
void
SourceParser
::begin_BaseClass(const Attributes& atts)
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
void
SourceParser
::end_BaseClass(void)
{
  PopElement();
}


/**
 * Begin handler for BaseType element.
 */
void
SourceParser
::begin_BaseType(const Attributes& atts)
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
void
SourceParser
::end_BaseType(void)
{
  PopElement();
}


/**
 * Begin handler for Instantiation element.
 */
void
SourceParser
::begin_Instantiation(const Attributes& atts)
{
}

/**
 * End handler for Instantiation element.
 */
void
SourceParser
::end_Instantiation(void)
{
}


/**
 * Begin handler for TemplateArgument element.
 */
void
SourceParser
::begin_TemplateArgument(const Attributes& atts)
{
}

/**
 * End handler for TemplateArgument element.
 */
void
SourceParser
::end_TemplateArgument(void)
{
}


/**
 * Begin handler for External element.
 */
void
SourceParser
::begin_External(const Attributes& atts)
{
}

/**
 * End handler for External element.
 */
void
SourceParser
::end_External(void)
{
}


/**
 * Begin handler for IncompleteType element.
 */
void
SourceParser
::begin_IncompleteType(const Attributes& atts)
{
}

/**
 * End handler for IncompleteType element.
 */
void
SourceParser
::end_IncompleteType(void)
{
}


/**
 * Begin handler for Location element.
 */
void
SourceParser
::begin_Location(const Attributes& atts)
{
  String file = atts.Get("file");
  int line = atts.GetAsInteger("line");

  Location::Pointer newLocation = Location::New(file, line);
  
  CurrentElement()->SetLocation(newLocation);
}

/**
 * End handler for Location element.
 */
void
SourceParser
::end_Location(void)
{
}


/**
 * Begin handler for CV_Qualifiers element.
 */
void
SourceParser
::begin_CV_Qualifiers(const Attributes& atts)
{
  CV_Qualifiers::Pointer cv = CV_Qualifiers::New(atts.GetAsInteger("const"),
						 atts.GetAsInteger("volatile"),
						 atts.GetAsInteger("restrict"));
  CurrentType()->SetCV_Qualifiers(cv);
}

/**
 * End handler for CV_Qualifiers element.
 */
void
SourceParser
::end_CV_Qualifiers(void)
{
}


/**
 * Begin handler for Unimplemented element.
 */
void
SourceParser
::begin_Unimplemented(const Attributes& atts)
{
}

/**
 * End handler for Unimplemented element.
 */
void
SourceParser
::end_Unimplemented(void)
{
}


/**
 * Map of SourceParser element begin handlers.
 */
SourceParser::BeginHandlers SourceParser::beginHandlers;

/**
 * Map of SourceParser element end handlers.
 */
SourceParser::EndHandlers SourceParser::endHandlers;


/**
 * This static method initializes the beginHandlers and endHandlers
 * maps.  It is called by the SourceParser constructor every time, but
 * only initializes the maps once.
 */
void
SourceParser
::InitializeHandlers(void)
{
  // Make sure we only initialize the maps once.
  static bool initialized = false;
  
  if(initialized) return;
  
  beginHandlers["GlobalNamespace"]  = &SourceParser::begin_GlobalNamespace;
  beginHandlers["Namespace"]        = &SourceParser::begin_Namespace;
  beginHandlers["NamespaceAlias"]   = &SourceParser::begin_NamespaceAlias;
  beginHandlers["Typedef"]          = &SourceParser::begin_Typedef;
  beginHandlers["Class"]            = &SourceParser::begin_Class;
  beginHandlers["Struct"]           = &SourceParser::begin_Struct;
  beginHandlers["Union"]            = &SourceParser::begin_Union;
  beginHandlers["Constructor"]      = &SourceParser::begin_Constructor;
  beginHandlers["Destructor"]       = &SourceParser::begin_Destructor;
  beginHandlers["Converter"]        = &SourceParser::begin_Converter;
  beginHandlers["OperatorFunction"] = &SourceParser::begin_OperatorFunction;
  beginHandlers["OperatorMethod"]   = &SourceParser::begin_OperatorMethod;
  beginHandlers["Method"]           = &SourceParser::begin_Method;
  beginHandlers["Function"]         = &SourceParser::begin_Function;
  beginHandlers["Argument"]         = &SourceParser::begin_Argument;
  beginHandlers["Returns"]          = &SourceParser::begin_Returns;
  beginHandlers["DefaultArgument"]  = &SourceParser::begin_DefaultArgument;
  beginHandlers["Ellipsis"]         = &SourceParser::begin_Ellipsis;
  beginHandlers["Variable"]         = &SourceParser::begin_Variable;
  beginHandlers["Initializer"]      = &SourceParser::begin_Initializer;
  beginHandlers["Field"]            = &SourceParser::begin_Field;
  beginHandlers["Enum"]             = &SourceParser::begin_Enum;
  beginHandlers["NamedType"]        = &SourceParser::begin_NamedType;
  beginHandlers["PointerType"]      = &SourceParser::begin_PointerType;
  beginHandlers["ReferenceType"]    = &SourceParser::begin_ReferenceType;
  beginHandlers["FunctionType"]     = &SourceParser::begin_FunctionType;
  beginHandlers["MethodType"]       = &SourceParser::begin_MethodType;
  beginHandlers["OffsetType"]       = &SourceParser::begin_OffsetType;
  beginHandlers["ArrayType"]        = &SourceParser::begin_ArrayType;
  beginHandlers["QualifiedName"]    = &SourceParser::begin_QualifiedName;
  beginHandlers["NameQualifier"]    = &SourceParser::begin_NameQualifier;
  beginHandlers["BaseClass"]        = &SourceParser::begin_BaseClass;
  beginHandlers["BaseType"]         = &SourceParser::begin_BaseType;
  beginHandlers["Instantiation"]    = &SourceParser::begin_Instantiation;
  beginHandlers["TemplateArgument"] = &SourceParser::begin_TemplateArgument;
  beginHandlers["External"]         = &SourceParser::begin_External;
  beginHandlers["IncompleteType"]   = &SourceParser::begin_IncompleteType;
  beginHandlers["Location"]         = &SourceParser::begin_Location;
  beginHandlers["CV_Qualifiers"]    = &SourceParser::begin_CV_Qualifiers;
  beginHandlers["Unimplemented"]    = &SourceParser::begin_Unimplemented;
  
  endHandlers["GlobalNamespace"]  = &SourceParser::end_GlobalNamespace;
  endHandlers["Namespace"]        = &SourceParser::end_Namespace;
  endHandlers["NamespaceAlias"]   = &SourceParser::end_NamespaceAlias;
  endHandlers["Typedef"]          = &SourceParser::end_Typedef;
  endHandlers["Class"]            = &SourceParser::end_Class;
  endHandlers["Struct"]           = &SourceParser::end_Struct;
  endHandlers["Union"]            = &SourceParser::end_Union;
  endHandlers["Constructor"]      = &SourceParser::end_Constructor;
  endHandlers["Destructor"]       = &SourceParser::end_Destructor;
  endHandlers["Converter"]        = &SourceParser::end_Converter;
  endHandlers["OperatorFunction"] = &SourceParser::end_OperatorFunction;
  endHandlers["OperatorMethod"]   = &SourceParser::end_OperatorMethod;
  endHandlers["Method"]           = &SourceParser::end_Method;
  endHandlers["Function"]         = &SourceParser::end_Function;
  endHandlers["Argument"]         = &SourceParser::end_Argument;
  endHandlers["Returns"]          = &SourceParser::end_Returns;
  endHandlers["DefaultArgument"]  = &SourceParser::end_DefaultArgument;
  endHandlers["Ellipsis"]         = &SourceParser::end_Ellipsis;
  endHandlers["Variable"]         = &SourceParser::end_Variable;
  endHandlers["Initializer"]      = &SourceParser::end_Initializer;
  endHandlers["Field"]            = &SourceParser::end_Field;
  endHandlers["Enum"]             = &SourceParser::end_Enum;
  endHandlers["NamedType"]        = &SourceParser::end_NamedType;
  endHandlers["PointerType"]      = &SourceParser::end_PointerType;
  endHandlers["ReferenceType"]    = &SourceParser::end_ReferenceType;
  endHandlers["FunctionType"]     = &SourceParser::end_FunctionType;
  endHandlers["MethodType"]       = &SourceParser::end_MethodType;
  endHandlers["OffsetType"]       = &SourceParser::end_OffsetType;
  endHandlers["ArrayType"]        = &SourceParser::end_ArrayType;
  endHandlers["QualifiedName"]    = &SourceParser::end_QualifiedName;
  endHandlers["NameQualifier"]    = &SourceParser::end_NameQualifier;
  endHandlers["BaseClass"]        = &SourceParser::end_BaseClass;
  endHandlers["BaseType"]         = &SourceParser::end_BaseType;
  endHandlers["Instantiation"]    = &SourceParser::end_Instantiation;
  endHandlers["TemplateArgument"] = &SourceParser::end_TemplateArgument;
  endHandlers["External"]         = &SourceParser::end_External;
  endHandlers["IncompleteType"]   = &SourceParser::end_IncompleteType;
  endHandlers["Location"]         = &SourceParser::end_Location;
  endHandlers["CV_Qualifiers"]    = &SourceParser::end_CV_Qualifiers;
  endHandlers["Unimplemented"]    = &SourceParser::end_Unimplemented;
  
  initialized = true;
}


/**
 * Begin element handler that is registered with the XML_Parser.
 * Passes call through to real parser object according to first argument.
 */
void
SourceParser
::BeginElement_proxy(void* parser, const char *name, const char **atts)
{
  static_cast<SourceParser*>(parser)->BeginElement(name, atts);
}


/**
 * End element handler that is registered with the XML_Parser.
 * Passes call through to real parser object according to first argument.
 */
void
SourceParser
::EndElement_proxy(void* parser, const char *name)
{
  static_cast<SourceParser*>(parser)->EndElement(name);
}


} // namespace xml
