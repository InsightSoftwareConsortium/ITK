#include <stack>
#include <map>
#include <string>

#include "parseSourceXML.h"

/**
 * Begin handlers map.
 */
static BeginHandlers beginHandlers;

/**
 * End handlers map.
 */
static EndHandlers endHandlers;

/**
 * A stack of XML elements used during parsing of the XML source.
 */
stack<InternalObject::Pointer>  elementStack;

/**
 * Store the global namespace.  It will also be at the bottom of the
 * element stack if the XML source is correct.
 */
Namespace::Pointer  GlobalNamespace;


/**
 * Get the top of the element stack.
 */
InternalObject::Pointer CurrentElement(void)
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
Context::Pointer CurrentContext(void)
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
Namespace::Pointer CurrentNamespace(void)
{
  TypeOfObject t = elementStack.top()->GetTypeOfObject();
  if((t != Namespace_id))
    throw ElementStackTypeException(__FILE__, __LINE__, "Namespace", t);
  
  return (Namespace*)elementStack.top().RealPointer();
}


/**
 * Get the top of the element stack cast as a Class.
 */
Class::Pointer CurrentClass(void)
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
Type::Pointer CurrentType(void)
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
Function::Pointer CurrentFunction(void)
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
Argument::Pointer CurrentArgument(void)
{
  TypeOfObject t = elementStack.top()->GetTypeOfObject();
  if((t != Argument_id))
    throw ElementStackTypeException(__FILE__, __LINE__, "Argument", t);

  return (Argument*)elementStack.top().RealPointer();
}


/**
 * Get the current type off the top of the stack as a PointerType.
 */
PointerType::Pointer CurrentPointerType(void)
{
  TypeOfObject t = elementStack.top()->GetTypeOfObject();
  if((t != PointerType_id))
    throw ElementStackTypeException(__FILE__, __LINE__, "PointerType", t);
  
  return (PointerType*)elementStack.top().RealPointer();
}


/**
 * Get the current type off the top of the stack as a ReferenceType.
 */
ReferenceType::Pointer CurrentReferenceType(void)
{
  TypeOfObject t = elementStack.top()->GetTypeOfObject();
  if((t != ReferenceType_id))
    throw ElementStackTypeException(__FILE__, __LINE__, "ReferenceType", t);
  
  return (ReferenceType*)elementStack.top().RealPointer();
}


/**
 * Get the current type off the top of the stack as a FunctionType.
 */
FunctionType::Pointer CurrentFunctionType(void)
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
MethodType::Pointer CurrentMethodType(void)
{
  TypeOfObject t = elementStack.top()->GetTypeOfObject();
  if(t != MethodType_id)
    throw ElementStackTypeException(__FILE__, __LINE__, "MethodType", t);
  
  return (MethodType*)elementStack.top().RealPointer();
}


/**
 * Get the current type off the top of the stack as a OffsetType.
 */
OffsetType::Pointer CurrentOffsetType(void)
{
  TypeOfObject t = elementStack.top()->GetTypeOfObject();
  if((t != OffsetType_id))
    throw ElementStackTypeException(__FILE__, __LINE__, "OffsetType", t);
  
  return (OffsetType*)elementStack.top().RealPointer();
}


/**
 * Get the current type off the top of the stack as a ArrayType.
 */
ArrayType::Pointer CurrentArrayType(void)
{
  TypeOfObject t = elementStack.top()->GetTypeOfObject();
  if((t != ArrayType_id))
    throw ElementStackTypeException(__FILE__, __LINE__, "ArrayType", t);
  
  return (ArrayType*)elementStack.top().RealPointer();
}


/**
 * Push a new element onto the element stack.
 */
void PushElement(InternalObject* element)
{
  elementStack.push(element);
}

/**
 * Pop the top off the element stack.
 */
void PopElement(void)
{
  elementStack.pop();

  // Sanity check.
  if(elementStack.empty())
    {
    fprintf(stderr, "Global namespace popped from stack!\n");
    exit(1);
    }
}


static void Initialize(void);

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

  HandlersPair hp(&beginHandlers, &endHandlers);
  
  XML_SetUserData(parser, &hp);
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

  /**
   * Return the global namespace.
   */
  return GlobalNamespace;
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
void begin_GlobalNamespace(const Attributes& atts)
{
  GlobalNamespace = Namespace::New("");
  PushElement(GlobalNamespace);
}

/**
 * End handler for GlobalNamespace element.
 */
void end_GlobalNamespace(void)
{
  // We want the global namespace left on the stack.  Don't pop it off.
}


/**
 * Begin handler for Namespace element.
 * Add a new Namespace to the current context, which must be a Namespace.
 */
void begin_Namespace(const Attributes& atts)
{
  String name = atts.Get("name");
  Namespace::Pointer newNamespace = Namespace::New(name);
  
  CurrentNamespace()->AddNamespace(newNamespace);
  PushElement(newNamespace);
}

/**
 * End handler for Namespace element.
 */
void end_Namespace(void)
{
  PopElement();
}


/**
 * Begin handler for NamespaceAlias element.
 */
void begin_NamespaceAlias(const Attributes& atts)
{
  UnimplementedNameHolder::Pointer newUnimplementedNameHolder
    = UnimplementedNameHolder::New();
  
  PushElement(newUnimplementedNameHolder);
}

/**
 * End handler for NamespaceAlias element.
 */
void end_NamespaceAlias(void)
{
  PopElement();
}


/**
 * Begin handler for Typedef element.
 */
void begin_Typedef(const Attributes& atts)
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
void end_Typedef(void)
{
  PopElement();
}


/**
 * Begin handler for Class element.
 * Add a new Class to the current context, which can be a Namespace, Class,
 * Struct, or Union.
 */
void begin_Class(const Attributes& atts)
{
  String name = atts.Get("name");
  Class::Pointer newClass = Class::New(name);
  
  CurrentContext()->AddClass(newClass);
  PushElement(newClass);
}

/**
 * End handler for Class element.
 */
void end_Class(void)
{
  PopElement();
}


/**
 * Begin handler for Struct element.
 * Add a new Struct to the current context, which can be a Namespace, Class,
 * Struct, or Union.
 */
void begin_Struct(const Attributes& atts)
{
  String name = atts.Get("name");
  Struct::Pointer newStruct = Struct::New(name);

  CurrentContext()->AddClass(newStruct);
  PushElement(newStruct);
}

/**
 * End handler for Struct element.
 */
void end_Struct(void)
{
  PopElement();
}


/**
 * Begin handler for Union element.
 * Add a new Union to the current context, which can be a Namespace, Class,
 * Struct, or Union.
 */
void begin_Union(const Attributes& atts)
{
  String name = atts.Get("name");
  Union::Pointer newUnion = Union::New(name);
  
  CurrentContext()->AddClass(newUnion);
  PushElement(newUnion);
}

/**
 * End handler for Union element.
 */
void end_Union(void)
{
  PopElement();
}


/**
 * Begin handler for Constructor element.
 * Add a Constructor the the current Class, Struct, or Union.
 */
void begin_Constructor(const Attributes& atts)
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
void end_Constructor(void)
{
  PopElement();
}


/**
 * Begin handler for Destructor element.
 * Add a Destructor the the current Class, Struct, or Union.
 */
void begin_Destructor(const Attributes& atts)
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
void end_Destructor(void)
{
  PopElement();
}


/**
 * Begin handler for Converter element.
 * Add a Converter to the current Class, Struct, or Union.
 */
void begin_Converter(const Attributes& atts)
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
void end_Converter(void)
{
  PopElement();
}


/**
 * Begin handler for OperatorFunction element.
 * Add an OperatorFunction to the current Namespace.
 */
void begin_OperatorFunction(const Attributes& atts)
{
  String name = atts.Get("name");  
  OperatorFunction::Pointer newOperatorFunction = OperatorFunction::New(name);
  
  CurrentNamespace()->AddFunction(newOperatorFunction);
  PushElement(newOperatorFunction);
}

/**
 * End handler for OperatorFunction element.
 */
void end_OperatorFunction(void)
{
  PopElement();
}


/**
 * Begin handler for OperatorMethod element.
 * Add an OperatorMethod to the current Class, Struct, or Union.
 */
void begin_OperatorMethod(const Attributes& atts)
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
void end_OperatorMethod(void)
{
  PopElement();
}


/**
 * Begin handler for Method element.
 * Add a Method to the current Class, Struct, or Union.
 */
void begin_Method(const Attributes& atts)
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
void end_Method(void)
{
  PopElement();
}


/**
 * Begin handler for Function element.
 * Add a Function to the current Namespace.
 */
void begin_Function(const Attributes& atts)
{
  String name = atts.Get("name");  
  Function::Pointer newFunction = Function::New(name);
  
  CurrentNamespace()->AddFunction(newFunction);
  PushElement(newFunction);
}

/**
 * End handler for Function element.
 */
void end_Function(void)
{
  PopElement();
}


/**
 * Begin handler for Argument element.
 * Add an argument to the function (or function type) currently being defined.
 */
void begin_Argument(const Attributes& atts)
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
void end_Argument(void)
{
  PopElement();
}


/**
 * Begin handler for Returns element.
 */
void begin_Returns(const Attributes& atts)
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
void end_Returns(void)
{
  PopElement();
}


/**
 * Begin handler for DefaultArgument element.
 */
void begin_DefaultArgument(const Attributes& atts)
{
}

/**
 * End handler for DefaultArgument element.
 */
void end_DefaultArgument(void)
{
}


/**
 * Begin handler for Ellipsis element.
 * Set the ellipsis flag of the current function.
 */
void begin_Ellipsis(const Attributes& atts)
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
void end_Ellipsis(void)
{
}


/**
 * Begin handler for Variable element.
 */
void begin_Variable(const Attributes& atts)
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
void end_Variable(void)
{
  PopElement();
}


/**
 * Begin handler for Initializer element.
 */
void begin_Initializer(const Attributes& atts)
{
}

/**
 * End handler for Initializer element.
 */
void end_Initializer(void)
{
}


/**
 * Begin handler for Field element.
 */
void begin_Field(const Attributes& atts)
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
void end_Field(void)
{
  PopElement();
}


/**
 * Begin handler for Enum element.
 */
void begin_Enum(const Attributes& atts)
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
void end_Enum(void)
{
  PopElement();
}


/**
 * Begin handler for NamedType element.
 */
void begin_NamedType(const Attributes& atts)
{
  NamedType::Pointer newNamedType = NamedType::New();

  CurrentElement()->SetInternalType(newNamedType);
  PushElement(newNamedType);
}

/**
 * End handler for NamedType element.
 */
void end_NamedType(void)
{
  PopElement();  
}


/**
 * Begin handler for PointerType element.
 */
void begin_PointerType(const Attributes& atts)
{
  PointerType::Pointer newPointerType = PointerType::New();
  
  CurrentElement()->SetInternalType(newPointerType);
  PushElement(newPointerType);
}

/**
 * End handler for PointerType element.
 */
void end_PointerType(void)
{
  PopElement();
}


/**
 * Begin handler for ReferenceType element.
 */
void begin_ReferenceType(const Attributes& atts)
{
  ReferenceType::Pointer newReferenceType = ReferenceType::New();
  
  CurrentElement()->SetInternalType(newReferenceType);
  PushElement(newReferenceType);
}

/**
 * End handler for ReferenceType element.
 */
void end_ReferenceType(void)
{
  PopElement();
}


/**
 * Begin handler for FunctionType element.
 */
void begin_FunctionType(const Attributes& atts)
{
  FunctionType::Pointer newFunctionType = FunctionType::New();
  
  CurrentElement()->SetInternalType(newFunctionType);
  PushElement(newFunctionType);
}

/**
 * End handler for FunctionType element.
 */
void end_FunctionType(void)
{
  PopElement();
}


/**
 * Begin handler for MethodType element.
 */
void begin_MethodType(const Attributes& atts)
{
  MethodType::Pointer newMethodType = MethodType::New();
  
  CurrentElement()->SetInternalType(newMethodType);
  PushElement(newMethodType);
}

/**
 * End handler for MethodType element.
 */
void end_MethodType(void)
{
  PopElement();
}


/**
 * Begin handler for OffsetType element.
 */
void begin_OffsetType(const Attributes& atts)
{
  OffsetType::Pointer newOffsetType = OffsetType::New();
  
  CurrentElement()->SetInternalType(newOffsetType);
  PushElement(newOffsetType);
}

/**
 * End handler for OffsetType element.
 */
void end_OffsetType(void)
{
  PopElement();
}


/**
 * Begin handler for ArrayType element.
 */
void begin_ArrayType(const Attributes& atts)
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
void end_ArrayType(void)
{
  PopElement();
}


/**
 * Begin handler for QualifiedName element.
 */
void begin_QualifiedName(const Attributes& atts)
{
  String name = atts.Get("name");
  QualifiedName::Pointer newQualifiedName = QualifiedName::New(name);

  CurrentElement()->SetInternalQualifiedName(newQualifiedName);
}

/**
 * End handler for QualifiedName element.
 */
void end_QualifiedName(void)
{
}


/**
 * Begin handler for NameQualifier element.
 */
void begin_NameQualifier(const Attributes& atts)
{
  String name = atts.Get("name");
  NameQualifier::Pointer newNameQualifier = NameQualifier::New(name);

  CurrentElement()->SetInternalQualifiedName(newNameQualifier);
  PushElement(newNameQualifier);
}

/**
 * End handler for NameQualifier element.
 */
void end_NameQualifier(void)
{
  PopElement();
}


/**
 * Begin handler for BaseClass element.
 */
void begin_BaseClass(const Attributes& atts)
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
void end_BaseClass(void)
{
  PopElement();
}


/**
 * Begin handler for BaseType element.
 */
void begin_BaseType(const Attributes& atts)
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
void end_BaseType(void)
{
  PopElement();
}


/**
 * Begin handler for Instantiation element.
 */
void begin_Instantiation(const Attributes& atts)
{
}

/**
 * End handler for Instantiation element.
 */
void end_Instantiation(void)
{
}


/**
 * Begin handler for TemplateArgument element.
 */
void begin_TemplateArgument(const Attributes& atts)
{
}

/**
 * End handler for TemplateArgument element.
 */
void end_TemplateArgument(void)
{
}


/**
 * Begin handler for External element.
 */
void begin_External(const Attributes& atts)
{
}

/**
 * End handler for External element.
 */
void end_External(void)
{
}


/**
 * Begin handler for IncompleteType element.
 */
void begin_IncompleteType(const Attributes& atts)
{
}

/**
 * End handler for IncompleteType element.
 */
void end_IncompleteType(void)
{
}


/**
 * Begin handler for Location element.
 */
void begin_Location(const Attributes& atts)
{
  String file = atts.Get("file");
  int line = atts.GetAsInteger("line");

  Location::Pointer newLocation = Location::New(file, line);
  
  CurrentElement()->SetLocation(newLocation);
}

/**
 * End handler for Location element.
 */
void end_Location(void)
{
}


/**
 * Begin handler for CV_Qualifiers element.
 */
void begin_CV_Qualifiers(const Attributes& atts)
{
}

/**
 * End handler for CV_Qualifiers element.
 */
void end_CV_Qualifiers(void)
{
}


/**
 * Begin handler for Unimplemented element.
 */
void begin_Unimplemented(const Attributes& atts)
{
}

/**
 * End handler for Unimplemented element.
 */
void end_Unimplemented(void)
{
}


/*@{
 * The name of an XML element tag.
 */
static const String tag_GlobalNamespace("GlobalNamespace");
static const String tag_Namespace("Namespace");
static const String tag_NamespaceAlias("NamespaceAlias");
static const String tag_Typedef("Typedef");
static const String tag_Class("Class");
static const String tag_Struct("Struct");
static const String tag_Union("Union");
static const String tag_Constructor("Constructor");
static const String tag_Destructor("Destructor");
static const String tag_Converter("Converter");
static const String tag_OperatorFunction("OperatorFunction");
static const String tag_OperatorMethod("OperatorMethod");
static const String tag_Method("Method");
static const String tag_Function("Function");
static const String tag_Argument("Argument");
static const String tag_Returns("Returns");
static const String tag_DefaultArgument("DefaultArgument");
static const String tag_Ellipsis("Ellipsis");
static const String tag_Variable("Variable");
static const String tag_Initializer("Initializer");
static const String tag_Field("Field");
static const String tag_Enum("Enum");
static const String tag_NamedType("NamedType");
static const String tag_PointerType("PointerType");
static const String tag_ReferenceType("ReferenceType");
static const String tag_FunctionType("FunctionType");
static const String tag_MethodType("MethodType");
static const String tag_OffsetType("OffsetType");
static const String tag_ArrayType("ArrayType");
static const String tag_QualifiedName("QualifiedName");
static const String tag_NameQualifier("NameQualifier");
static const String tag_BaseClass("BaseClass");
static const String tag_BaseType("BaseType");
static const String tag_Instantiation("Instantiation");
static const String tag_TemplateArgument("TemplateArgument");
static const String tag_External("External");
static const String tag_IncompleteType("IncompleteType");
static const String tag_Location("Location");
static const String tag_CV_Qualifiers("CV_Qualifiers");
static const String tag_Unimplemented("Unimplemented");
//@}


/**
 * Setup the element handler mappings for each xml tag type.
 * Also initialize attribute information.
 */
void Initialize(void)
{
  beginHandlers[tag_GlobalNamespace]         = begin_GlobalNamespace;
  beginHandlers[tag_Namespace]               = begin_Namespace;
  beginHandlers[tag_NamespaceAlias]          = begin_NamespaceAlias;
  beginHandlers[tag_Typedef]                 = begin_Typedef;
  beginHandlers[tag_Class]                   = begin_Class;
  beginHandlers[tag_Struct]                  = begin_Struct;
  beginHandlers[tag_Union]                   = begin_Union;
  beginHandlers[tag_Constructor]             = begin_Constructor;
  beginHandlers[tag_Destructor]              = begin_Destructor;
  beginHandlers[tag_Converter]               = begin_Converter;
  beginHandlers[tag_OperatorFunction]        = begin_OperatorFunction;
  beginHandlers[tag_OperatorMethod]          = begin_OperatorMethod;
  beginHandlers[tag_Method]                  = begin_Method;
  beginHandlers[tag_Function]                = begin_Function;
  beginHandlers[tag_Argument]                = begin_Argument;
  beginHandlers[tag_Returns]                 = begin_Returns;
  beginHandlers[tag_DefaultArgument]         = begin_DefaultArgument;
  beginHandlers[tag_Ellipsis]                = begin_Ellipsis;
  beginHandlers[tag_Variable]                = begin_Variable;
  beginHandlers[tag_Initializer]             = begin_Initializer;
  beginHandlers[tag_Field]                   = begin_Field;
  beginHandlers[tag_Enum]                    = begin_Enum;
  beginHandlers[tag_NamedType]               = begin_NamedType;
  beginHandlers[tag_PointerType]             = begin_PointerType;
  beginHandlers[tag_ReferenceType]           = begin_ReferenceType;
  beginHandlers[tag_FunctionType]            = begin_FunctionType;
  beginHandlers[tag_MethodType]              = begin_MethodType;
  beginHandlers[tag_OffsetType]              = begin_OffsetType;
  beginHandlers[tag_ArrayType]               = begin_ArrayType;
  beginHandlers[tag_QualifiedName]           = begin_QualifiedName;
  beginHandlers[tag_NameQualifier]           = begin_NameQualifier;
  beginHandlers[tag_BaseClass]               = begin_BaseClass;
  beginHandlers[tag_BaseType]                = begin_BaseType;
  beginHandlers[tag_Instantiation]           = begin_Instantiation;
  beginHandlers[tag_TemplateArgument]        = begin_TemplateArgument;
  beginHandlers[tag_External]                = begin_External;
  beginHandlers[tag_IncompleteType]          = begin_IncompleteType;
  beginHandlers[tag_Location]                = begin_Location;
  beginHandlers[tag_CV_Qualifiers]           = begin_CV_Qualifiers;
  beginHandlers[tag_Unimplemented]           = begin_Unimplemented;
  
  endHandlers[tag_GlobalNamespace]         = end_GlobalNamespace;
  endHandlers[tag_Namespace]               = end_Namespace;
  endHandlers[tag_NamespaceAlias]          = end_NamespaceAlias;
  endHandlers[tag_Typedef]                 = end_Typedef;
  endHandlers[tag_Class]                   = end_Class;
  endHandlers[tag_Struct]                  = end_Struct;
  endHandlers[tag_Union]                   = end_Union;
  endHandlers[tag_Constructor]             = end_Constructor;
  endHandlers[tag_Destructor]              = end_Destructor;
  endHandlers[tag_Converter]               = end_Converter;
  endHandlers[tag_OperatorFunction]        = end_OperatorFunction;
  endHandlers[tag_OperatorMethod]          = end_OperatorMethod;
  endHandlers[tag_Method]                  = end_Method;
  endHandlers[tag_Function]                = end_Function;
  endHandlers[tag_Argument]                = end_Argument;
  endHandlers[tag_Returns]                 = end_Returns;
  endHandlers[tag_DefaultArgument]         = end_DefaultArgument;
  endHandlers[tag_Ellipsis]                = end_Ellipsis;
  endHandlers[tag_Variable]                = end_Variable;
  endHandlers[tag_Initializer]             = end_Initializer;
  endHandlers[tag_Field]                   = end_Field;
  endHandlers[tag_Enum]                    = end_Enum;
  endHandlers[tag_NamedType]               = end_NamedType;
  endHandlers[tag_PointerType]             = end_PointerType;
  endHandlers[tag_ReferenceType]           = end_ReferenceType;
  endHandlers[tag_FunctionType]            = end_FunctionType;
  endHandlers[tag_MethodType]              = end_MethodType;
  endHandlers[tag_OffsetType]              = end_OffsetType;
  endHandlers[tag_ArrayType]               = end_ArrayType;
  endHandlers[tag_QualifiedName]           = end_QualifiedName;
  endHandlers[tag_NameQualifier]           = end_NameQualifier;
  endHandlers[tag_BaseClass]               = end_BaseClass;
  endHandlers[tag_BaseType]                = end_BaseType;
  endHandlers[tag_Instantiation]           = end_Instantiation;
  endHandlers[tag_TemplateArgument]        = end_TemplateArgument;
  endHandlers[tag_External]                = end_External;
  endHandlers[tag_IncompleteType]          = end_IncompleteType;
  endHandlers[tag_Location]                = end_Location;
  endHandlers[tag_CV_Qualifiers]           = end_CV_Qualifiers;
  endHandlers[tag_Unimplemented]           = end_Unimplemented;
}
