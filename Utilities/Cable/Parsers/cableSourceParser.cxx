#include "sourceParser.h"

namespace source
{

/**
 * Create a new Parser and return a pointer to it.
 */
Parser::Pointer
Parser::New()
{
  return new Self;
}


/**
 * Constructor sets up an XML_Parser to make call-backs into this
 * Parser.
 */
Parser
::Parser():
  m_XML_Parser(XML_ParserCreate(NULL)),
  m_ElementStack(),
  m_GlobalNamespace(NULL)
{
  Parser::InitializeHandlers();
  
  XML_SetElementHandler(m_XML_Parser,
                        BeginElement_proxy,
                        EndElement_proxy);
  XML_SetUserData(m_XML_Parser,
                  this);
}


/**
 * Cleanup the XML_Parser
 */
Parser
::~Parser()
{
  XML_ParserFree(m_XML_Parser);
}


/**
 * Parse the XML from the given input stream until end-of-input is reached.
 */
void
Parser
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
      std::cerr << "Parser::Parse(): \""
                << XML_ErrorString(XML_GetErrorCode(m_XML_Parser))
                << "\" at source line "
                << XML_GetCurrentLineNumber(m_XML_Parser)
                << std::endl;
      }
    }
}


/**
 * Called when a new element is opened in the XML source.
 * Checks the tag name, and calls the appropriate handler with an
 * Attributes container.
 */
void
Parser
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
    throw UnknownElementTagException(name);
    }
  }
  catch (const ParseException& e)
    {
    e.PrintLocation(std::cerr, m_XML_Parser, "source");
    e.Print(std::cerr);
    }
  catch (const String& e)
    {
    std::cerr << "Caught exception in Parser::BeginElement():"
              << std::endl << e.c_str() << std::endl;
    }
  catch (...)
    {
    std::cerr << "Caught unknown exception in Parser::BeginElement()."
              << std::endl;
    }
}


/**
 * Called at the end of an element in the XML source opened when
 * BeginElement was called.
 */
void
Parser
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
    throw UnknownElementTagException(name);
    }
  }
  catch (const ParseException& e)
    {
    e.PrintLocation(std::cerr, m_XML_Parser, "source");
    e.Print(std::cerr);
    }
  catch (const String& e)
    {
    std::cerr << "Caught exception in Parser::EndElement():"
              << std::endl << e.c_str() << std::endl;
    }
  catch (...)
    {
    std::cerr << "Caught unknown exception in Parser::EndElement()."
              << std::endl;
    }
}


/**
 * Get the top of the element stack.
 */
InternalObject::Pointer
Parser
::CurrentElement()
{
  return m_ElementStack.top();
}



/**
 * An unexpected element type is on top of the stack.
 */
class ElementStackTypeException: public ParseException
{
public:
  ElementStackTypeException(const char* e, const char* t):
    ParseException(), m_Expected(e), m_Got(t) {}
  virtual ~ElementStackTypeException() {}
  void Print(std::ostream& os) const
    {
      os << "Expected \"" << m_Expected.c_str()
         << "\" as immediate enclosing element, but got \"" << m_Got << "\""
         << std::endl;
    }
private:
  String m_Expected;
  String m_Got;
};


/**
 * Get the top of the element stack as a context pointer.
 */
Context::Pointer
Parser
::CurrentContext()
{
  TypeOfObject t = m_ElementStack.top()->GetTypeOfObject();
  if((t != Namespace_id)
     && (t != Class_id)
     && (t != Struct_id)
     && (t != Union_id))
    throw ElementStackTypeException("Any Context",
                                    m_ElementStack.top()->GetClassName());
  
  return dynamic_cast<Context*>(m_ElementStack.top().RealPointer());
}


/**
 * Get the top of the element stack cast as a Namespace.
 */
Namespace::Pointer
Parser
::CurrentNamespace()
{
  TypeOfObject t = m_ElementStack.top()->GetTypeOfObject();
  if((t != Namespace_id))
    throw ElementStackTypeException("Namespace",
                                    m_ElementStack.top()->GetClassName());
  
  return dynamic_cast<Namespace*>(m_ElementStack.top().RealPointer());
}


/**
 * Get the top of the element stack cast as a Class.
 */
Class::Pointer
Parser
::CurrentClass()
{
  TypeOfObject t = m_ElementStack.top()->GetTypeOfObject();
  if((t != Class_id)
     && (t != Struct_id)
     && (t != Union_id))
    throw ElementStackTypeException("Any Class",
                                    m_ElementStack.top()->GetClassName());
  
  return dynamic_cast<Class*>(m_ElementStack.top().RealPointer());
}


/**
 * Get the current type off the top of the element stack.
 */
Type::Pointer
Parser
::CurrentType()
{
  TypeOfObject t = m_ElementStack.top()->GetTypeOfObject();
  if((t != NamedType_id)
     && (t != PointerType_id)
     && (t != ReferenceType_id)
     && (t != FunctionType_id)
     && (t != MethodType_id)
     && (t != OffsetType_id)
     && (t != ArrayType_id))
    throw ElementStackTypeException("Any Type",
                                    m_ElementStack.top()->GetClassName());
  
  return dynamic_cast<Type*>(m_ElementStack.top().RealPointer());
}


/**
 * Get the current function off the top of the element stack.
 */
Function::Pointer
Parser
::CurrentFunction()
{
  TypeOfObject t = m_ElementStack.top()->GetTypeOfObject();
  if((t != Function_id)
    && (t != Method_id)
    && (t != Constructor_id)
    && (t != Destructor_id)
    && (t != Converter_id)
    && (t != OperatorFunction_id)
    && (t != OperatorMethod_id))
    throw ElementStackTypeException("Any Function",
                                    m_ElementStack.top()->GetClassName());

  return dynamic_cast<Function*>(m_ElementStack.top().RealPointer());
}


/**
 * Get the current argument off the top of the element stack.
 */
Argument::Pointer
Parser
::CurrentArgument()
{
  TypeOfObject t = m_ElementStack.top()->GetTypeOfObject();
  if((t != Argument_id))
    throw ElementStackTypeException("Argument",
                                    m_ElementStack.top()->GetClassName());

  return dynamic_cast<Argument*>(m_ElementStack.top().RealPointer());
}


/**
 * Get the current type off the top of the stack as a PointerType.
 */
PointerType::Pointer
Parser
::CurrentPointerType()
{
  TypeOfObject t = m_ElementStack.top()->GetTypeOfObject();
  if((t != PointerType_id))
    throw ElementStackTypeException("PointerType",
                                    m_ElementStack.top()->GetClassName());
  
  return dynamic_cast<PointerType*>(m_ElementStack.top().RealPointer());
}


/**
 * Get the current type off the top of the stack as a ReferenceType.
 */
ReferenceType::Pointer
Parser
::CurrentReferenceType()
{
  TypeOfObject t = m_ElementStack.top()->GetTypeOfObject();
  if((t != ReferenceType_id))
    throw ElementStackTypeException("ReferenceType",
                                    m_ElementStack.top()->GetClassName());
  
  return dynamic_cast<ReferenceType*>(m_ElementStack.top().RealPointer());
}


/**
 * Get the current type off the top of the stack as a FunctionType.
 */
FunctionType::Pointer
Parser
::CurrentFunctionType()
{
  TypeOfObject t = m_ElementStack.top()->GetTypeOfObject();
  if((t != FunctionType_id)
     && (t != MethodType_id))
    throw ElementStackTypeException("Any FunctionType",
                                    m_ElementStack.top()->GetClassName());
  
  return dynamic_cast<FunctionType*>(m_ElementStack.top().RealPointer());
}


/**
 * Get the current type off the top of the stack as a MethodType.
 */
MethodType::Pointer
Parser
::CurrentMethodType()
{
  TypeOfObject t = m_ElementStack.top()->GetTypeOfObject();
  if(t != MethodType_id)
    throw ElementStackTypeException("MethodType",
                                    m_ElementStack.top()->GetClassName());
  
  return dynamic_cast<MethodType*>(m_ElementStack.top().RealPointer());
}


/**
 * Get the current type off the top of the stack as a OffsetType.
 */
OffsetType::Pointer
Parser
::CurrentOffsetType()
{
  TypeOfObject t = m_ElementStack.top()->GetTypeOfObject();
  if((t != OffsetType_id))
    throw ElementStackTypeException("OffsetType",
                                    m_ElementStack.top()->GetClassName());
  
  return dynamic_cast<OffsetType*>(m_ElementStack.top().RealPointer());
}


/**
 * Get the current type off the top of the stack as a ArrayType.
 */
ArrayType::Pointer
Parser
::CurrentArrayType()
{
  TypeOfObject t = m_ElementStack.top()->GetTypeOfObject();
  if((t != ArrayType_id))
    throw ElementStackTypeException("ArrayType",
                                    m_ElementStack.top()->GetClassName());
  
  return dynamic_cast<ArrayType*>(m_ElementStack.top().RealPointer());
}


/**
 * Push a new element onto the element stack.
 */
void
Parser
::PushElement(InternalObject* element)
{
  m_ElementStack.push(element);
}

/**
 * Pop the top off the element stack.
 */
void
Parser
::PopElement()
{
  m_ElementStack.pop();

  // Sanity check.
  if(m_ElementStack.empty())
    {
    throw String("Global namespace popped from element stack!");
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
Parser
::begin_GlobalNamespace(const Attributes& atts)
{
  m_GlobalNamespace = Namespace::New("");
  this->PushElement(m_GlobalNamespace);
}

/**
 * End handler for GlobalNamespace element.
 */
void
Parser
::end_GlobalNamespace()
{
  // We want the global namespace left on the stack.  Don't pop it off.
}


/**
 * Begin handler for Namespace element.
 * Add a new Namespace to the current context, which must be a Namespace.
 */
void
Parser
::begin_Namespace(const Attributes& atts)
{
  String name = atts.Get("name");
  Namespace::Pointer newNamespace = Namespace::New(name);
  
  this->CurrentNamespace()->AddNamespace(newNamespace);
  this->PushElement(newNamespace);
}

/**
 * End handler for Namespace element.
 */
void
Parser
::end_Namespace()
{
  this->PopElement();
}


/**
 * Begin handler for NamespaceAlias element.
 */
void
Parser
::begin_NamespaceAlias(const Attributes& atts)
{
  UnimplementedNameHolder::Pointer newUnimplementedNameHolder
    = UnimplementedNameHolder::New();
  
  this->PushElement(newUnimplementedNameHolder);
}

/**
 * End handler for NamespaceAlias element.
 */
void
Parser
::end_NamespaceAlias()
{
  this->PopElement();
}


/**
 * Begin handler for Typedef element.
 */
void
Parser
::begin_Typedef(const Attributes& atts)
{
  UnimplementedTypeHolder::Pointer newUnimplementedTypeHolder =
    UnimplementedTypeHolder::New();
  
  // Only need typedef to absorb its internal information.  It will be
  // lost when it is popped off the stack by end_Typedef().
  this->PushElement(newUnimplementedTypeHolder);
}

/**
 * End handler for Typedef element.
 */
void
Parser
::end_Typedef()
{
  this->PopElement();
}


/**
 * Begin handler for Class element.
 * Add a new Class to the current context, which can be a Namespace, Class,
 * Struct, or Union.
 */
void
Parser
::begin_Class(const Attributes& atts)
{
  String name = atts.Get("name");
  String accessStr = atts.Get("access");
  Access access;

  if(accessStr == access_public)         access = Public;
  else if(accessStr == access_protected) access = Protected;
  else                                   access = Private;
  
  Class::Pointer newClass = Class::New(name, access);
  
  this->CurrentContext()->AddClass(newClass);
  this->PushElement(newClass);
}

/**
 * End handler for Class element.
 */
void
Parser
::end_Class()
{
  this->PopElement();
}


/**
 * Begin handler for Struct element.
 * Add a new Struct to the current context, which can be a Namespace, Class,
 * Struct, or Union.
 */
void
Parser
::begin_Struct(const Attributes& atts)
{
  String name = atts.Get("name");
  String accessStr = atts.Get("access");
  Access access;

  if(accessStr == access_public)         access = Public;
  else if(accessStr == access_protected) access = Protected;
  else                                   access = Private;
  
  Struct::Pointer newStruct = Struct::New(name, access);

  this->CurrentContext()->AddClass(newStruct);
  this->PushElement(newStruct);
}

/**
 * End handler for Struct element.
 */
void
Parser
::end_Struct()
{
  this->PopElement();
}


/**
 * Begin handler for Union element.
 * Add a new Union to the current context, which can be a Namespace, Class,
 * Struct, or Union.
 */
void
Parser
::begin_Union(const Attributes& atts)
{
  String name = atts.Get("name");
  String accessStr = atts.Get("access");
  Access access;

  if(accessStr == access_public)         access = Public;
  else if(accessStr == access_protected) access = Protected;
  else                                   access = Private;

  Union::Pointer newUnion = Union::New(name, access);
  
  this->CurrentContext()->AddClass(newUnion);
  this->PushElement(newUnion);
}

/**
 * End handler for Union element.
 */
void
Parser
::end_Union()
{
  this->PopElement();
}


/**
 * Begin handler for Constructor element.
 * Add a Constructor the the current Class, Struct, or Union.
 */
void
Parser
::begin_Constructor(const Attributes& atts)
{
  String accessStr = atts.Get("access");
  Access access;

  if(accessStr == access_public)         access = Public;
  else if(accessStr == access_protected) access = Protected;
  else                                   access = Private;
  
  Constructor::Pointer newConstructor = Constructor::New(access);

  this->CurrentClass()->AddMethod(newConstructor);
  this->PushElement(newConstructor);
}

/**
 * End handler for Constructor element.
 */
void
Parser
::end_Constructor()
{
  this->PopElement();
}


/**
 * Begin handler for Destructor element.
 * Add a Destructor the the current Class, Struct, or Union.
 */
void
Parser
::begin_Destructor(const Attributes& atts)
{
  String accessStr = atts.Get("access");
  Access access;
  
  if(accessStr == access_public)         access = Public;
  else if(accessStr == access_protected) access = Protected;
  else                                   access = Private;
  
  Destructor::Pointer newDestructor = Destructor::New(access);
  
  this->CurrentClass()->AddMethod(newDestructor);
  this->PushElement(newDestructor);
}

/**
 * End handler for Destructor element.
 */
void
Parser
::end_Destructor()
{
  this->PopElement();
}


/**
 * Begin handler for Converter element.
 * Add a Converter to the current Class, Struct, or Union.
 */
void
Parser
::begin_Converter(const Attributes& atts)
{
  String accessStr = atts.Get("access");
  Access access;
  
  if(accessStr == access_public)         access = Public;
  else if(accessStr == access_protected) access = Protected;
  else                                   access = Private;
  
  Converter::Pointer newConverter = Converter::New(access);
  
  this->CurrentClass()->AddMethod(newConverter);
  this->PushElement(newConverter);
}

/**
 * End handler for Converter element.
 */
void
Parser
::end_Converter()
{
  this->PopElement();
}


/**
 * Begin handler for OperatorFunction element.
 * Add an OperatorFunction to the current Namespace.
 */
void
Parser
::begin_OperatorFunction(const Attributes& atts)
{
  String name = atts.Get("name");  
  OperatorFunction::Pointer newOperatorFunction = OperatorFunction::New(name);
  
  this->CurrentNamespace()->AddFunction(newOperatorFunction);
  this->PushElement(newOperatorFunction);
}

/**
 * End handler for OperatorFunction element.
 */
void
Parser
::end_OperatorFunction()
{
  this->PopElement();
}


/**
 * Begin handler for OperatorMethod element.
 * Add an OperatorMethod to the current Class, Struct, or Union.
 */
void
Parser
::begin_OperatorMethod(const Attributes& atts)
{
  String name = atts.Get("name");  
  String accessStr = atts.Get("access");
  Access access;
  
  if(accessStr == access_public)         access = Public;
  else if(accessStr == access_protected) access = Protected;
  else                                   access = Private;
  
  OperatorMethod::Pointer newOperatorMethod = OperatorMethod::New(name, access);
  
  this->CurrentClass()->AddMethod(newOperatorMethod);
  this->PushElement(newOperatorMethod);
}

/**
 * End handler for OperatorMethod element.
 */
void
Parser
::end_OperatorMethod()
{
  this->PopElement();
}


/**
 * Begin handler for Method element.
 * Add a Method to the current Class, Struct, or Union.
 */
void
Parser
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
  
  this->CurrentClass()->AddMethod(newMethod);
  this->PushElement(newMethod);
}

/**
 * End handler for Method element.
 */
void
Parser
::end_Method()
{
  this->PopElement();
}


/**
 * Begin handler for Function element.
 * Add a Function to the current Namespace.
 */
void
Parser
::begin_Function(const Attributes& atts)
{
  String name = atts.Get("name");  
  Function::Pointer newFunction = Function::New(name);
  
  this->CurrentNamespace()->AddFunction(newFunction);
  this->PushElement(newFunction);
}

/**
 * End handler for Function element.
 */
void
Parser
::end_Function()
{
  this->PopElement();
}


/**
 * Begin handler for Argument element.
 * Add an argument to the function (or function type) currently being defined.
 */
void
Parser
::begin_Argument(const Attributes& atts)
{  
  String name = atts.Get("name");  
  Argument::Pointer newArgument = Argument::New(name);

  TypeOfObject t = this->CurrentElement()->GetTypeOfObject();

  if((t == FunctionType_id)
     || (t == MethodType_id))
    {
    this->CurrentFunctionType()->AddArgument(newArgument);
    }
  else
    {
    this->CurrentFunction()->AddArgument(newArgument);
    }
  this->PushElement(newArgument);
}


/**
 * End handler for Argument element.
 */
void
Parser
::end_Argument()
{
  this->PopElement();
}


/**
 * Begin handler for Returns element.
 */
void
Parser
::begin_Returns(const Attributes& atts)
{
  Returns::Pointer newReturns = Returns::New();
  
  TypeOfObject t = this->CurrentElement()->GetTypeOfObject();

  if((t == FunctionType_id)
     || (t == MethodType_id))
    {
    this->CurrentFunctionType()->SetReturns(newReturns);
    }
  else
    {
    this->CurrentFunction()->SetReturns(newReturns);
    }
  
  this->PushElement(newReturns);
}

/**
 * End handler for Returns element.
 */
void
Parser
::end_Returns()
{
  this->PopElement();
}


/**
 * Begin handler for DefaultArgument element.
 */
void
Parser
::begin_DefaultArgument(const Attributes& atts)
{
}

/**
 * End handler for DefaultArgument element.
 */
void
Parser
::end_DefaultArgument()
{
}


/**
 * Begin handler for Ellipsis element.
 * Set the ellipsis flag of the current function.
 */
void
Parser
::begin_Ellipsis(const Attributes& atts)
{
  TypeOfObject t = this->CurrentElement()->GetTypeOfObject();

  if((t == FunctionType_id)
     || (t == MethodType_id))
    {
    this->CurrentFunctionType()->SetEllipsis(true);
    }
  else
    {
    this->CurrentFunction()->SetEllipsis(true);
    }
}

/**
 * End handler for Ellipsis element.
 */
void
Parser
::end_Ellipsis()
{
}


/**
 * Begin handler for Variable element.
 */
void
Parser
::begin_Variable(const Attributes& atts)
{
  UnimplementedTypeHolder::Pointer newUnimplementedTypeHolder =
    UnimplementedTypeHolder::New();
  
  // Only need typedef to absorb its internal information.  It will be
  // lost when it is popped off the stack by end_Variable().
  this->PushElement(newUnimplementedTypeHolder);
}

/**
 * End handler for Variable element.
 */
void
Parser
::end_Variable()
{
  this->PopElement();
}


/**
 * Begin handler for Initializer element.
 */
void
Parser
::begin_Initializer(const Attributes& atts)
{
}

/**
 * End handler for Initializer element.
 */
void
Parser
::end_Initializer()
{
}


/**
 * Begin handler for Field element.
 */
void
Parser
::begin_Field(const Attributes& atts)
{
  UnimplementedTypeHolder::Pointer newUnimplementedTypeHolder =
    UnimplementedTypeHolder::New();
  
  // Only need typedef to absorb its internal information.  It will be
  // lost when it is popped off the stack by end_Field().
  this->PushElement(newUnimplementedTypeHolder);
}

/**
 * End handler for Field element.
 */
void
Parser
::end_Field()
{
  this->PopElement();
}


/**
 * Begin handler for Enum element.
 */
void
Parser
::begin_Enum(const Attributes& atts)
{
  UnimplementedTypeHolder::Pointer newUnimplementedTypeHolder =
    UnimplementedTypeHolder::New();
  
  // Only need typedef to absorb its internal information.  It will be
  // lost when it is popped off the stack by end_Enum().
  this->PushElement(newUnimplementedTypeHolder);
}

/**
 * End handler for Enum element.
 */
void
Parser
::end_Enum()
{
  this->PopElement();
}


/**
 * Begin handler for NamedType element.
 */
void
Parser
::begin_NamedType(const Attributes& atts)
{
  NamedType::Pointer newNamedType = NamedType::New();

  this->CurrentElement()->SetInternalType(newNamedType);
  this->PushElement(newNamedType);
}

/**
 * End handler for NamedType element.
 */
void
Parser
::end_NamedType()
{
  this->PopElement();  
}


/**
 * Begin handler for PointerType element.
 */
void
Parser
::begin_PointerType(const Attributes& atts)
{
  PointerType::Pointer newPointerType = PointerType::New();
  
  this->CurrentElement()->SetInternalType(newPointerType);
  this->PushElement(newPointerType);
}

/**
 * End handler for PointerType element.
 */
void
Parser
::end_PointerType()
{
  this->PopElement();
}


/**
 * Begin handler for ReferenceType element.
 */
void
Parser
::begin_ReferenceType(const Attributes& atts)
{
  ReferenceType::Pointer newReferenceType = ReferenceType::New();
  
  this->CurrentElement()->SetInternalType(newReferenceType);
  this->PushElement(newReferenceType);
}

/**
 * End handler for ReferenceType element.
 */
void
Parser
::end_ReferenceType()
{
  this->PopElement();
}


/**
 * Begin handler for FunctionType element.
 */
void
Parser
::begin_FunctionType(const Attributes& atts)
{
  FunctionType::Pointer newFunctionType = FunctionType::New();
  
  this->CurrentElement()->SetInternalType(newFunctionType);
  this->PushElement(newFunctionType);
}

/**
 * End handler for FunctionType element.
 */
void
Parser
::end_FunctionType()
{
  this->PopElement();
}


/**
 * Begin handler for MethodType element.
 */
void
Parser
::begin_MethodType(const Attributes& atts)
{
  MethodType::Pointer newMethodType = MethodType::New();
  
  this->CurrentElement()->SetInternalType(newMethodType);
  this->PushElement(newMethodType);
}

/**
 * End handler for MethodType element.
 */
void
Parser
::end_MethodType()
{
  this->PopElement();
}


/**
 * Begin handler for OffsetType element.
 */
void
Parser
::begin_OffsetType(const Attributes& atts)
{
  OffsetType::Pointer newOffsetType = OffsetType::New();
  
  this->CurrentElement()->SetInternalType(newOffsetType);
  this->PushElement(newOffsetType);
}

/**
 * End handler for OffsetType element.
 */
void
Parser
::end_OffsetType()
{
  this->PopElement();
}


/**
 * Begin handler for ArrayType element.
 */
void
Parser
::begin_ArrayType(const Attributes& atts)
{
  int min = atts.GetAsInteger("min");
  int max = atts.GetAsInteger("max");
  ArrayType::Pointer newArrayType = ArrayType::New(min, max);
  
  this->CurrentElement()->SetInternalType(newArrayType);
  this->PushElement(newArrayType);
}

/**
 * End handler for ArrayType element.
 */
void
Parser
::end_ArrayType()
{
  this->PopElement();
}


/**
 * Begin handler for EnumType element.
 */
void
Parser
::begin_EnumType(const Attributes&)
{
  // Use the UnimplementedNameHolder to catch the Location element
  // inside the EnumType element.  
  UnimplementedNameHolder::Pointer newUnimplementedNameHolder
    = UnimplementedNameHolder::New();
  
  this->PushElement(newUnimplementedNameHolder);
}

/**
 * End handler for EnumType element.
 */
void
Parser
::end_EnumType()
{
  this->PopElement();
}


/**
 * Begin handler for QualifiedName element.
 */
void
Parser
::begin_QualifiedName(const Attributes& atts)
{
  String name = atts.Get("name");
  QualifiedName::Pointer newQualifiedName = QualifiedName::New(name);

  this->CurrentElement()->SetInternalQualifiedName(newQualifiedName);
}

/**
 * End handler for QualifiedName element.
 */
void
Parser
::end_QualifiedName()
{
}


/**
 * Begin handler for NameQualifier element.
 */
void
Parser
::begin_NameQualifier(const Attributes& atts)
{
  String name = atts.Get("name");
  NameQualifier::Pointer newNameQualifier = NameQualifier::New(name);

  this->CurrentElement()->SetInternalQualifiedName(newNameQualifier);
  this->PushElement(newNameQualifier);
}

/**
 * End handler for NameQualifier element.
 */
void
Parser
::end_NameQualifier()
{
  this->PopElement();
}


/**
 * Begin handler for BaseClass element.
 */
void
Parser
::begin_BaseClass(const Attributes& atts)
{
  String accessStr = atts.Get("access");
  Access access;
  
  if(accessStr == access_public)         access = Public;
  else if(accessStr == access_protected) access = Protected;
  else                                   access = Private;
  
  BaseClass::Pointer newBaseClass = BaseClass::New(access);

  this->CurrentClass()->AddBaseClass(newBaseClass);
  this->PushElement(newBaseClass);
}

/**
 * End handler for BaseClass element.
 */
void
Parser
::end_BaseClass()
{
  this->PopElement();
}


/**
 * Begin handler for BaseType element.
 */
void
Parser
::begin_BaseType(const Attributes& atts)
{
  BaseType::Pointer newBaseType = BaseType::New();

  TypeOfObject t = this->CurrentElement()->GetTypeOfObject();
  
  if(t == MethodType_id)
    {
    this->CurrentMethodType()->SetBaseType(newBaseType);
    }
  else
    {
    this->CurrentOffsetType()->SetBaseType(newBaseType);
    }
  
  this->PushElement(newBaseType);
}

/**
 * End handler for BaseType element.
 */
void
Parser
::end_BaseType()
{
  this->PopElement();
}


/**
 * Begin handler for Instantiation element.
 */
void
Parser
::begin_Instantiation(const Attributes& atts)
{
}

/**
 * End handler for Instantiation element.
 */
void
Parser
::end_Instantiation()
{
}


/**
 * Begin handler for TemplateArgument element.
 */
void
Parser
::begin_TemplateArgument(const Attributes& atts)
{
}

/**
 * End handler for TemplateArgument element.
 */
void
Parser
::end_TemplateArgument()
{
}


/**
 * Begin handler for External element.
 */
void
Parser
::begin_External(const Attributes& atts)
{
}

/**
 * End handler for External element.
 */
void
Parser
::end_External()
{
}


/**
 * Begin handler for IncompleteType element.
 */
void
Parser
::begin_IncompleteType(const Attributes& atts)
{
}

/**
 * End handler for IncompleteType element.
 */
void
Parser
::end_IncompleteType()
{
}


/**
 * Begin handler for Location element.
 */
void
Parser
::begin_Location(const Attributes& atts)
{
  String file = atts.Get("file");
  int line = atts.GetAsInteger("line");

  Location::Pointer newLocation = Location::New(file, line);
  
  this->CurrentElement()->SetLocation(newLocation);
}

/**
 * End handler for Location element.
 */
void
Parser
::end_Location()
{
}


/**
 * Begin handler for CvQualifiers element.
 */
void
Parser
::begin_CvQualifiers(const Attributes& atts)
{
  CvQualifiers::Pointer cv = CvQualifiers::New(atts.GetAsBoolean("const"),
                                  						 atts.GetAsBoolean("volatile"),
						                                   atts.GetAsBoolean("restrict"));
  this->CurrentType()->SetCvQualifiers(cv);
}

/**
 * End handler for CvQualifiers element.
 */
void
Parser
::end_CvQualifiers()
{
}


/**
 * Begin handler for Unimplemented element.
 */
void
Parser
::begin_Unimplemented(const Attributes& atts)
{
}

/**
 * End handler for Unimplemented element.
 */
void
Parser
::end_Unimplemented()
{
}


/**
 * Map of Parser element begin handlers.
 */
Parser::BeginHandlers Parser::beginHandlers;

/**
 * Map of Parser element end handlers.
 */
Parser::EndHandlers Parser::endHandlers;


/**
 * This static method initializes the beginHandlers and endHandlers
 * maps.  It is called by the Parser constructor every time, but
 * only initializes the maps once.
 */
void
Parser
::InitializeHandlers()
{
  // Make sure we only initialize the maps once.
  static bool initialized = false;
  
  if(initialized) return;
  
  beginHandlers["GlobalNamespace"]  = &Parser::begin_GlobalNamespace;
  beginHandlers["Namespace"]        = &Parser::begin_Namespace;
  beginHandlers["NamespaceAlias"]   = &Parser::begin_NamespaceAlias;
  beginHandlers["Typedef"]          = &Parser::begin_Typedef;
  beginHandlers["Class"]            = &Parser::begin_Class;
  beginHandlers["Struct"]           = &Parser::begin_Struct;
  beginHandlers["Union"]            = &Parser::begin_Union;
  beginHandlers["Constructor"]      = &Parser::begin_Constructor;
  beginHandlers["Destructor"]       = &Parser::begin_Destructor;
  beginHandlers["Converter"]        = &Parser::begin_Converter;
  beginHandlers["OperatorFunction"] = &Parser::begin_OperatorFunction;
  beginHandlers["OperatorMethod"]   = &Parser::begin_OperatorMethod;
  beginHandlers["Method"]           = &Parser::begin_Method;
  beginHandlers["Function"]         = &Parser::begin_Function;
  beginHandlers["Argument"]         = &Parser::begin_Argument;
  beginHandlers["Returns"]          = &Parser::begin_Returns;
  beginHandlers["DefaultArgument"]  = &Parser::begin_DefaultArgument;
  beginHandlers["Ellipsis"]         = &Parser::begin_Ellipsis;
  beginHandlers["Variable"]         = &Parser::begin_Variable;
  beginHandlers["Initializer"]      = &Parser::begin_Initializer;
  beginHandlers["Field"]            = &Parser::begin_Field;
  beginHandlers["Enum"]             = &Parser::begin_Enum;
  beginHandlers["NamedType"]        = &Parser::begin_NamedType;
  beginHandlers["PointerType"]      = &Parser::begin_PointerType;
  beginHandlers["ReferenceType"]    = &Parser::begin_ReferenceType;
  beginHandlers["FunctionType"]     = &Parser::begin_FunctionType;
  beginHandlers["MethodType"]       = &Parser::begin_MethodType;
  beginHandlers["OffsetType"]       = &Parser::begin_OffsetType;
  beginHandlers["ArrayType"]        = &Parser::begin_ArrayType;
  beginHandlers["EnumType"]         = &Parser::begin_EnumType;
  beginHandlers["QualifiedName"]    = &Parser::begin_QualifiedName;
  beginHandlers["NameQualifier"]    = &Parser::begin_NameQualifier;
  beginHandlers["BaseClass"]        = &Parser::begin_BaseClass;
  beginHandlers["BaseType"]         = &Parser::begin_BaseType;
  beginHandlers["Instantiation"]    = &Parser::begin_Instantiation;
  beginHandlers["TemplateArgument"] = &Parser::begin_TemplateArgument;
  beginHandlers["External"]         = &Parser::begin_External;
  beginHandlers["IncompleteType"]   = &Parser::begin_IncompleteType;
  beginHandlers["Location"]         = &Parser::begin_Location;
  beginHandlers["CvQualifiers"]     = &Parser::begin_CvQualifiers;
  beginHandlers["Unimplemented"]    = &Parser::begin_Unimplemented;
  
  endHandlers["GlobalNamespace"]  = &Parser::end_GlobalNamespace;
  endHandlers["Namespace"]        = &Parser::end_Namespace;
  endHandlers["NamespaceAlias"]   = &Parser::end_NamespaceAlias;
  endHandlers["Typedef"]          = &Parser::end_Typedef;
  endHandlers["Class"]            = &Parser::end_Class;
  endHandlers["Struct"]           = &Parser::end_Struct;
  endHandlers["Union"]            = &Parser::end_Union;
  endHandlers["Constructor"]      = &Parser::end_Constructor;
  endHandlers["Destructor"]       = &Parser::end_Destructor;
  endHandlers["Converter"]        = &Parser::end_Converter;
  endHandlers["OperatorFunction"] = &Parser::end_OperatorFunction;
  endHandlers["OperatorMethod"]   = &Parser::end_OperatorMethod;
  endHandlers["Method"]           = &Parser::end_Method;
  endHandlers["Function"]         = &Parser::end_Function;
  endHandlers["Argument"]         = &Parser::end_Argument;
  endHandlers["Returns"]          = &Parser::end_Returns;
  endHandlers["DefaultArgument"]  = &Parser::end_DefaultArgument;
  endHandlers["Ellipsis"]         = &Parser::end_Ellipsis;
  endHandlers["Variable"]         = &Parser::end_Variable;
  endHandlers["Initializer"]      = &Parser::end_Initializer;
  endHandlers["Field"]            = &Parser::end_Field;
  endHandlers["Enum"]             = &Parser::end_Enum;
  endHandlers["NamedType"]        = &Parser::end_NamedType;
  endHandlers["PointerType"]      = &Parser::end_PointerType;
  endHandlers["ReferenceType"]    = &Parser::end_ReferenceType;
  endHandlers["FunctionType"]     = &Parser::end_FunctionType;
  endHandlers["MethodType"]       = &Parser::end_MethodType;
  endHandlers["OffsetType"]       = &Parser::end_OffsetType;
  endHandlers["ArrayType"]        = &Parser::end_ArrayType;
  endHandlers["EnumType"]         = &Parser::end_EnumType;
  endHandlers["QualifiedName"]    = &Parser::end_QualifiedName;
  endHandlers["NameQualifier"]    = &Parser::end_NameQualifier;
  endHandlers["BaseClass"]        = &Parser::end_BaseClass;
  endHandlers["BaseType"]         = &Parser::end_BaseType;
  endHandlers["Instantiation"]    = &Parser::end_Instantiation;
  endHandlers["TemplateArgument"] = &Parser::end_TemplateArgument;
  endHandlers["External"]         = &Parser::end_External;
  endHandlers["IncompleteType"]   = &Parser::end_IncompleteType;
  endHandlers["Location"]         = &Parser::end_Location;
  endHandlers["CvQualifiers"]     = &Parser::end_CvQualifiers;
  endHandlers["Unimplemented"]    = &Parser::end_Unimplemented;
  
  initialized = true;
}


/**
 * Begin element handler that is registered with the XML_Parser.
 * Passes call through to real parser object according to first argument.
 */
void
Parser
::BeginElement_proxy(void* parser, const char *name, const char **atts)
{
  static_cast<Parser*>(parser)->BeginElement(name, atts);
}


/**
 * End element handler that is registered with the XML_Parser.
 * Passes call through to real parser object according to first argument.
 */
void
Parser
::EndElement_proxy(void* parser, const char *name)
{
  static_cast<Parser*>(parser)->EndElement(name);
}


} // namespace source
