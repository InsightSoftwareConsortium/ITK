#ifndef _sourceParser_h
#define _sourceParser_h

#include "xmlParseException.h"
#include "xmlAttributes.h"
#include "sourceRep.h"

#include <stack>

namespace source
{

typedef ::xml::ParseException              ParseException;
typedef ::xml::UnknownElementTagException  UnknownElementTagException;
typedef ::xml::Attributes                  Attributes;

/**
 * Class to parse the XML description of a C++ source.
 */
class Parser: public Object
{
public:
  typedef Parser Self;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;
  
  static Pointer New();
  
  void Parse(std::istream&);

public:
  // Call-backs from the XML parser.
  void BeginElement(const char *name, const char **atts);
  void EndElement(const char *name);
  
protected:
  Parser();
  Parser(const Self&) {}
  void operator=(const Self&) {}
  virtual ~Parser();
  
private:
  /**
   * The actual XML_Parser.
   */
  XML_Parser m_XML_Parser;
  
  /**
   * A stack of XML elements used during parsing of the XML source.
   */
  std::stack<InternalObject::Pointer>  m_ElementStack;  
  
  /**
   * Store the global namespace.  It will also be at the bottom of the
   * element stack if the XML source is correct.
   */
  Namespace::Pointer  m_GlobalNamespace;
  
  // Access functions for element stack.
  InternalObject::Pointer CurrentElement();
  Context::Pointer        CurrentContext();
  Namespace::Pointer      CurrentNamespace();
  Class::Pointer          CurrentClass();
  Type::Pointer           CurrentType();
  Function::Pointer       CurrentFunction();
  Argument::Pointer       CurrentArgument();
  PointerType::Pointer    CurrentPointerType();
  ReferenceType::Pointer  CurrentReferenceType();
  FunctionType::Pointer   CurrentFunctionType();
  MethodType::Pointer     CurrentMethodType();
  OffsetType::Pointer     CurrentOffsetType();
  ArrayType::Pointer      CurrentArrayType();
  
  // Element stack utilities.
  void PushElement(InternalObject* element);
  void PopElement();

  // The element begin handlers.
  void begin_GlobalNamespace(const Attributes&);
  void begin_Namespace(const Attributes&);
  void begin_NamespaceAlias(const Attributes&);
  void begin_Typedef(const Attributes&);
  void begin_Class(const Attributes&);
  void begin_Struct(const Attributes&);
  void begin_Union(const Attributes&);
  void begin_Constructor(const Attributes&);
  void begin_Destructor(const Attributes&);
  void begin_Converter(const Attributes&);
  void begin_OperatorFunction(const Attributes&);
  void begin_OperatorMethod(const Attributes&);
  void begin_Method(const Attributes&);
  void begin_Function(const Attributes&);
  void begin_Argument(const Attributes&);
  void begin_Returns(const Attributes&);
  void begin_DefaultArgument(const Attributes&);
  void begin_Ellipsis(const Attributes&);
  void begin_Variable(const Attributes&);
  void begin_Initializer(const Attributes&);
  void begin_Field(const Attributes&);
  void begin_Enum(const Attributes&);
  void begin_NamedType(const Attributes&);
  void begin_PointerType(const Attributes&);
  void begin_ReferenceType(const Attributes&);
  void begin_FunctionType(const Attributes&);
  void begin_MethodType(const Attributes&);
  void begin_OffsetType(const Attributes&);
  void begin_ArrayType(const Attributes&);
  void begin_EnumType(const Attributes&);
  void begin_QualifiedName(const Attributes&);
  void begin_NameQualifier(const Attributes&);
  void begin_BaseClass(const Attributes&);
  void begin_BaseType(const Attributes&);
  void begin_Instantiation(const Attributes&);
  void begin_TemplateArgument(const Attributes&);
  void begin_External(const Attributes&);
  void begin_IncompleteType(const Attributes&);
  void begin_Location(const Attributes&);
  void begin_CvQualifiers(const Attributes&);
  void begin_Unimplemented(const Attributes&);

  // The element end handlers.
  void end_GlobalNamespace();
  void end_Namespace();
  void end_NamespaceAlias();
  void end_Typedef();
  void end_Class();
  void end_Struct();
  void end_Union();
  void end_Constructor();
  void end_Destructor();
  void end_Converter();
  void end_OperatorFunction();
  void end_OperatorMethod();
  void end_Method();
  void end_Function();
  void end_Argument();
  void end_Returns();
  void end_DefaultArgument();
  void end_Ellipsis();
  void end_Variable();
  void end_Initializer();
  void end_Field();
  void end_Enum();
  void end_NamedType();
  void end_PointerType();
  void end_ReferenceType();
  void end_FunctionType();
  void end_MethodType();
  void end_OffsetType();
  void end_ArrayType();
  void end_EnumType();
  void end_QualifiedName();
  void end_NameQualifier();
  void end_BaseClass();
  void end_BaseType();
  void end_Instantiation();
  void end_TemplateArgument();
  void end_External();
  void end_IncompleteType();
  void end_Location();
  void end_CvQualifiers();
  void end_Unimplemented();
  
  // Element map utilities.
  static void InitializeHandlers();  

  /**
   * Map from element name to its beginning handler.
   */
  typedef std::map<String, void (Self::*)(const Attributes&)>  BeginHandlers;
  
  /**
   * Map from element name to its ending handler.
   */
  typedef std::map<String, void (Self::*)()>  EndHandlers;  
  
  static BeginHandlers beginHandlers;
  static EndHandlers   endHandlers;  

  // Proxy functions for call-backs from XML Parser.
  static void BeginElement_proxy(void*, const char *, const char **);
  static void EndElement_proxy(void*, const char *);
};

} // namespace source
  
#endif
