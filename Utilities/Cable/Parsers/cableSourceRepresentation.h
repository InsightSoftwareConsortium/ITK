/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cableSourceRepresentation.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _cableSourceRepresentation_h
#define _cableSourceRepresentation_h

#include "cableReferenceCount.h"
#include "cxxTypeSystem.h"

/**
 * We don't have to worry about name conflicts here, so alias the
 * "_cxx_" namespace with "cxx".
 */
namespace cxx = _cxx_;

#include <stdio.h>
#include <string>
#include <vector>
#include <set>
#include <strstream>

#define NoPointedToTypeException  ("PointerType has no pointed-to type.")
#define NoReferencedTypeException  ("ReferenceType has no referenced type.")
#define NotIndirectionException ("Indirection operation on non-indirection type.")
#define IndirectionOnReferenceException ("Attempt to form type with indirection to reference.")
#define NameIsNotClassException ("Name looked up as class, but is not Class, Struct, or Union.")

namespace source
{

typedef std::string String;

String GetValid_C_Identifier(const String& in_name);

class Location;
class CvQualifiers;
class Type;
class Argument;
class Function;
class Context;
class NamedType;
class PointerType;
class ReferenceType;
class FunctionType;
class MethodType;
class OffsetType;
class ArrayType;
class Namespace;
class Method;
class Constructor;
class Destructor;
class Converter;
class OperatorMethod;
class OperatorFunction;
class Class;
class Struct;
class Union;
class Instantiation;
class TemplateArgument;
class QualifiedName;
class NameQualifier;
class BaseClass;
class BaseType;

enum Access { Public, Protected, Private };


/**
 * Enumeration of identifiers for object types.
 */
enum TypeOfObject {
  Undefined_id=0,
  
  Location_id, CvQualifiers_id,
  
  Namespace_id, Class_id, Struct_id, Union_id,
  
  Function_id, Method_id, Constructor_id, Destructor_id,
  Converter_id, OperatorFunction_id, OperatorMethod_id,
  
  NamedType_id, PointerType_id, ReferenceType_id, FunctionType_id,
  MethodType_id, OffsetType_id, ArrayType_id,
  
  Argument_id, Returns_id,
  
  QualifiedName_id, NameQualifier_id,
  
  UnimplementedTypeHolder_id,
  UnimplementedNameHolder_id
};


/**
 * Base class for all internal representation objects.  Implements
 * reference counting and object type identification.
 */
class InternalObject: public Object
{
public:
  typedef InternalObject            Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /**
   * Get type identifier of object.  Default to Undefined_id.
   */
  virtual TypeOfObject GetTypeOfObject() const
    {
      return Undefined_id;
    }
  
  /**
   * Get the name of the real object's class.
   */
  virtual const char* GetClassName() const { return "InternalObject"; }
  
  /**
   * Many of the objects have an internal type that must be set.  Examples
   * include Argument, Returns, and certain Type objects.
   */
  virtual void SetInternalType(Type*)
    {
      std::strstream e;
      e << "Object not expecting internal type setting: "
        << int(this->GetTypeOfObject()) << std::ends;
      throw String(e.str());
    }
  
  /**
   * Many of the object have an internal qualified name that must be set.
   * Examples include NamespaceAlias, NamedType, BaseType, and BaseClass.
   */
  virtual void SetInternalQualifiedName(QualifiedName*)
    {
      std::strstream e;
      e << "Object not expecting internal name setting: "
        << int(this->GetTypeOfObject()) << std::ends;
      throw String(e.str());
    }
  
  /**
   * Many of the objects have a name with a location that must be set.
   */
  virtual void SetLocation(Location* location)
    {
      throw String("Object not expecting location setting.");
    }

  /**
   * Checks if the object has what it needs to be complete.
   * If an object type needs certain conditions to consider itself complete,
   * it should re-implement this to do the test before returning true.
   */
  virtual bool CheckComplete() const 
    {
      return true;
    }
  
protected:
  /**
   * Called to ensure that the object pointed to by "this" considers
   * itself complete accodring to CheckComplete().
   */
  void AssertComplete(const char* source_file, unsigned int source_line) const
    {
      if(this->CheckComplete()) return;
      std::strstream e;
      e << "Incomplete object encountered at:" << std::endl
        << source_file << ": " << source_line << std::ends;
      throw String(e.str());
    }
  
protected:
  InternalObject() {}
  InternalObject(const Self&) {}
  void operator=(const Self&) {}
  virtual ~InternalObject() {}
};


/**
 * Store the source location information for a declaration.
 */
class Location: public InternalObject
{
public:
  typedef Location                  Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  virtual TypeOfObject GetTypeOfObject() const { return Location_id; }
  virtual const char* GetClassName() const { return "Location"; }
  
  static Pointer New(const String& file, unsigned int line);

  String GetFile() const { return m_File; }
  unsigned int GetLine() const { return m_Line; }
  
  void Print(FILE*, unsigned int) const;
  
protected:
  Location(const String& file, unsigned int line):
    m_File(file), m_Line(line) {}
  Location(const Self&) {}
  void operator=(const Self&) {}
  virtual ~Location() {}
private:
  String m_File;
  unsigned int m_Line;
};


/** \class Named
 * Base class for any class in the internal representation which represents
 * a named entity of the source program.  Such named entities also have
 * a location, which is stored as well.
 */
class Named: public InternalObject
{
public:
  typedef Named                     Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  const String& GetName() const { return m_Name; }

  virtual void SetLocation(Location* location) { m_Location = location; }
  Location::Pointer GetLocation() { return m_Location; }
  Location::ConstPointer GetLocation() const { return m_Location.RealPointer(); }
  
protected:
  Named(const String& name): m_Name(name) {}
  Named(const Named&) {}
  void operator=(const Named&) {}
  virtual ~Named() {}
  
private:
  /**
   * The name of this entity.
   */
  String m_Name;
  Location::Pointer m_Location;
};


/** \struct NamedCompare
 * Comparison function object for two "Named" pointers.  Used to sort
 * objects derived from Named.
 */
struct NamedCompare
{
  bool operator () (const Named* l, const Named* r) const
    { return l->GetName() < r->GetName(); }
};

typedef SmartPointer<Context>        ContextPointer;
typedef SmartPointer<const Context>  ContextConstPointer;
typedef SmartPointer<QualifiedName>        QualifiedNamePointer;
typedef SmartPointer<const QualifiedName>  QualifiedNameConstPointer;
typedef SmartPointer<BaseType>        BaseTypePointer;
typedef SmartPointer<const BaseType>  BaseTypeConstPointer;

typedef SmartPointer<Argument>   ArgumentPointer;
typedef SmartPointer<Class>      ClassPointer;
typedef SmartPointer<Namespace>  NamespacePointer;
typedef SmartPointer<Function>   FunctionPointer;
typedef SmartPointer<Method>     MethodPointer;
typedef SmartPointer<BaseClass>  BaseClassPointer;

typedef std::vector<ArgumentPointer>                  ArgumentContainer;
typedef std::set<ClassPointer, NamedCompare>          ClassContainer;
typedef std::set<NamespacePointer, NamedCompare>      NamespaceContainer;
typedef std::multiset<FunctionPointer, NamedCompare>  FunctionContainer;
typedef std::multiset<MethodPointer, NamedCompare>    MethodContainer;
typedef std::vector<BaseClassPointer>                 BaseClassContainer;

typedef ArgumentContainer::const_iterator   ArgumentsIterator;
typedef ClassContainer::const_iterator      ClassesIterator;
typedef FunctionContainer::const_iterator   FunctionsIterator;
typedef NamespaceContainer::const_iterator  NamespacesIterator;
typedef MethodContainer::const_iterator     MethodsIterator;
typedef BaseClassContainer::const_iterator  BaseClassesIterator;

/**
 * A singe instance of the TypeSystem will be used to register all
 * cxx type representations.
 */
extern cxx::TypeSystem typeSystem;

/**
 * Store cv-qualifiers for a type.
 */
class CvQualifiers: public InternalObject
{
public:
  typedef CvQualifiers             Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  virtual TypeOfObject GetTypeOfObject() const { return CvQualifiers_id; }
  virtual const char* GetClassName() const { return "CvQualifiers"; }
  
  static Pointer New(bool is_const, bool is_volatile, bool is_restrict);
  
  String GetString() const;
  
  bool IsConst() const    { return m_Const; }
  bool IsVolatile() const { return m_Volatile; }
  bool IsRestrict() const { return m_Restrict; }

  void Print(FILE*, unsigned int) const;
  
protected:
  CvQualifiers(bool is_const, bool is_volatile, bool is_restrict):
    m_Const(is_const), m_Volatile(is_volatile), m_Restrict(is_restrict) {}
  CvQualifiers(const Self&) {}
  void operator=(const Self&) {}
  virtual ~CvQualifiers() {}
  
private:
  bool m_Const;
  bool m_Volatile;
  bool m_Restrict;
};


/**
 * The interface to any type.
 */
class Type: public InternalObject
{
public:
  typedef Type                      Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  bool IsNamedType() const     { return (this->GetTypeOfObject() == NamedType_id); }
  bool IsPointerType() const   { return (this->GetTypeOfObject() == PointerType_id); }
  bool IsReferenceType() const { return (this->GetTypeOfObject() == ReferenceType_id); }
  bool IsFunctionType() const  { return (this->GetTypeOfObject() == FunctionType_id); }
  bool IsMethodType() const    { return (this->GetTypeOfObject() == MethodType_id); }
  bool IsOffsetType() const    { return (this->GetTypeOfObject() == OffsetType_id); }
  bool IsArrayType() const     { return (this->GetTypeOfObject() == ArrayType_id); }
  bool IsFunctionPointer() const
    {
      // if(!this->IsPointerType()) return false;      
      // ... Type::Pointer GetPointedToType() { return m_PointedToType; }
      return ((this->GetTypeOfObject() == PointerType_id )
	      && (this->GetName().find("(*)") != String::npos));
    }
  
  String GetName() const { return this->GetNameWithCV(); }  
  String GetCV() const { return m_CvQualifiers? m_CvQualifiers->GetString() : ""; }
  
  /**
   * Get the cv-qualified name of this type.
   * If the pointer is not NULL, use the Type's GetIndirectionWithCV() method
   * and return the indirection form of the name (pointer or reference).
   */
  virtual String GetNameWithCV(const Type* =NULL) const =0;
  
  /**
   * Get the cv-unqualified name of this type.
   * If the pointer is not NULL, use the Type's GetIndirectionWithoutCV()
   * method and return the indirection form of the name (pointer or
   * reference).
   */
  virtual String GetNameWithoutCV(const Type* =NULL) const =0;

  /**
   * Get the cv-qualified indirection string for this type.  Only
   * pointer and reference types will implement this to do something useful.
   * Shouldn't be called for others, so throw an exception by default.
   */
  virtual String GetIndirectionWithCV() const
    { throw NotIndirectionException; return String(""); }
  
  /**
   * Get the cv-unqualified indirection string for this type.  Only
   * pointer and reference types will implement this to do something useful.
   * Shouldn't be called for others, so throw an exception by default.
   */
  virtual String GetIndirectionWithoutCV() const
    { throw NotIndirectionException; return String(""); }
  
  void SetCvQualifiers(CvQualifiers* cv)
    { m_CvQualifiers = cv; }

  CvQualifiers::Pointer GetCvQualifiers()
    { return m_CvQualifiers; }

  CvQualifiers::ConstPointer GetCvQualifiers() const
    { return m_CvQualifiers.RealPointer(); }

  bool IsConst() const   { return (m_CvQualifiers && m_CvQualifiers->IsConst()); }
  bool IsVolatile() const { return (m_CvQualifiers && m_CvQualifiers->IsVolatile()); }
  bool IsRestrict() const { return (m_CvQualifiers && m_CvQualifiers->IsRestrict()); }

  virtual void Print(FILE*, unsigned int) const =0;
  void PrintCvQualifiers(FILE*, unsigned int) const;  
  void PrintName(FILE*, unsigned int) const;  
  
protected:
  Type() {}
  Type(const Self&) {}
  void operator=(const Self&) {}
  virtual ~Type() {}
  
private:
  CvQualifiers::Pointer m_CvQualifiers;
};


/**
 * Store one function argument.
 */
class Argument: public Named
{
public:
  typedef Argument                  Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  virtual TypeOfObject GetTypeOfObject() const { return Argument_id; }
  virtual const char* GetClassName() const { return "Argument"; }

  static Pointer New(const String& name);

  virtual void SetInternalType(Type* t) { this->SetType(t); }

  void SetType(Type* t)       { m_Type = t; }
  Type::Pointer GetType() { return m_Type; }
  Type::ConstPointer GetType() const { return m_Type.RealPointer(); }

  void SetDefault(const String& s) { m_Default = s; }
  String GetDefault()          { return m_Default; }

  String GetStringWithCV() const;
  String GetStringWithoutCV() const;

  void Print(FILE*, unsigned int) const;
  
  virtual bool CheckComplete() const { return (m_Type != NULL); }
  
protected:
  Argument(const String& name): Named(name) {}
  Argument(const Self&): Named("") {}
  void operator=(const Self&) {}
  virtual ~Argument() {}
private:
  Type::Pointer m_Type;
  String m_Default;
};


/**
 * Store the return type for a function.
 */
class Returns: public InternalObject
{
public:
  typedef Returns                   Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  virtual TypeOfObject GetTypeOfObject() const { return Returns_id; }
  virtual const char* GetClassName() const { return "Returns"; }

  static Pointer New();

  virtual void SetInternalType(Type* t) { this->SetType(t); }
  
  void SetType(Type* t)       { m_Type = t; }
  Type::Pointer GetType() { return m_Type; }
  Type::ConstPointer GetType() const { return m_Type.RealPointer(); }

  String GetStringWithCV() const;
  String GetStringWithoutCV() const;

  void Print(FILE*, unsigned int) const;
  
  virtual bool CheckComplete() const { return (m_Type != NULL); }
  
protected:
  Returns() {}
  Returns(const Self&) {}
  void operator=(const Self&) {}
  virtual ~Returns() {}
private:
  Type::Pointer m_Type;
};


/**
 * Store a normal function, and provide an interface to all function types.
 * Also stores a static function in a Class, Struct, or Union.
 */
class Function: public Named
{
public:
  typedef Function                  Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  static Pointer New(const String& name);
  
  virtual TypeOfObject GetTypeOfObject() const { return Function_id; }
  virtual const char* GetClassName() const { return "Function"; }

  bool IsFunction() const          { return (this->GetTypeOfObject() == Function_id); }
  bool IsMethod() const            { return (this->GetTypeOfObject() == Method_id); }
  bool IsConstructor() const       { return (this->GetTypeOfObject() == Constructor_id); }
  bool IsDestructor() const        { return (this->GetTypeOfObject() == Destructor_id); }
  bool IsConverter() const         { return (this->GetTypeOfObject() == Converter_id); }
  bool IsOperatorMethod() const    { return (this->GetTypeOfObject() == OperatorMethod_id); }
  bool IsOperatorFunction() const  { return (this->GetTypeOfObject() == OperatorFunction_id); }

  void SetReturns(Returns* t)       { m_Returns = t; }
  Returns::Pointer GetReturns() { return m_Returns; }
  Returns::ConstPointer GetReturns() const { return m_Returns.RealPointer(); }
  
  void AddArgument(Argument* a) { m_Arguments.insert(m_Arguments.end(), a); }
  const ArgumentContainer& GetArguments() const { return m_Arguments; }
  Argument::Pointer GetArgument(unsigned int a) { return m_Arguments[a]; }
  Argument::ConstPointer GetArgument(unsigned int a) const { return m_Arguments[a].RealPointer(); }
  unsigned int GetArgumentCount() const { return m_Arguments.size(); }  
  
  void SetContext(Context* context) { m_Context = context; }
  ContextPointer GetContext() { return m_Context.RealPointer(); }
  ContextConstPointer GetContext() const { return m_Context.RealPointer(); }

  void SetEllipsis(bool e) { m_Ellipsis = e; }
  bool GetEllipsis() const { return m_Ellipsis; }

  virtual String GetCallName() const { return this->GetName(); }
  
  virtual void Print(FILE*, unsigned int) const;
  void PrintFunctionPrototypeInfo(FILE*, unsigned int) const;
  
protected:
  Function(const String& name): Named(name) {}
  Function(const Self&): Named("") {}
  void operator=(const Self&) {}
  virtual ~Function() {}
  
private:
  Returns::Pointer   m_Returns;
  ArgumentContainer  m_Arguments;
  ContextPointer     m_Context;
  bool               m_Ellipsis;
};


/** \class Context
 * Abstract interface for scoping.  Any namespace or class scope has
 * a containing context (except the global namespace), a name, and
 * child classes.
 */
class Context: public Named
{
public:
  typedef Context                   Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  bool IsNamespace() const { return (this->GetTypeOfObject() == Namespace_id); }
  bool IsClass() const     { return (this->GetTypeOfObject() == Class_id); }
  bool IsStruct() const    { return (this->GetTypeOfObject() == Struct_id); }
  bool IsUnion() const     { return (this->GetTypeOfObject() == Union_id); }
  
  void AddClass(Context* inClass)
    { inClass->SetContext(this); m_Classes.insert((Class*)inClass); }
  const ClassContainer& GetClasses() const { return m_Classes; }

  void SetContext(Context* context) { m_Context = context; }
  Pointer GetContext() { return m_Context.RealPointer(); }
  ConstPointer GetContext() const { return m_Context.RealPointer(); }
  
  virtual void Print(FILE*, unsigned int) const =0;

  void PrintClasses(FILE*, unsigned int) const;
  
  String GetQualifiedName() const;
  
  typedef std::vector<String>  Qualifiers;
  typedef Qualifiers::const_iterator QualifiersConstIterator;
  typedef std::back_insert_iterator<Qualifiers>  QualifiersInserter;

  bool ParseQualifiedName(const String&, QualifiersInserter) const;
  
protected:
  Context(const String& name): Named(name) {}
  Context(const Self&): Named("") {}
  void operator=(const Self&) {}
  virtual ~Context() { }
  
protected:
  ClassContainer     m_Classes;
  Pointer            m_Context;
};


/**
 * A type with no special language-defined semantics.
 */
class NamedType: public Type
{
public:
  typedef NamedType                Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  virtual TypeOfObject GetTypeOfObject() const { return NamedType_id; }
  virtual const char* GetClassName() const { return "NamedType"; }

  static Pointer New();
                     
  virtual void SetInternalQualifiedName(QualifiedName* n) { m_QualifiedName = n; }
  
  virtual String GetNameWithCV(const Type* =NULL) const;
  virtual String GetNameWithoutCV(const Type* =NULL) const;
  
  virtual void Print(FILE*, unsigned int) const;
  
  virtual bool CheckComplete() const { return (m_QualifiedName != NULL); }
  
protected:
  NamedType() {}
  NamedType(const Self&) {}
  void operator=(const Self&) {}
  virtual ~NamedType() {}
  
private:
  QualifiedNamePointer m_QualifiedName;
};


/**
 * Store a pointer type.
 */
class PointerType: public Type
{
public:
  typedef PointerType               Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  virtual TypeOfObject GetTypeOfObject() const { return PointerType_id; }
  virtual const char* GetClassName() const { return "PointerType"; }

  static Pointer New();

  virtual String GetNameWithCV(const Type* =NULL) const;
  virtual String GetNameWithoutCV(const Type* =NULL) const;
  virtual String GetIndirectionWithCV() const;
  virtual String GetIndirectionWithoutCV() const;
  
  virtual void SetInternalType(Type* t) { this->SetPointedToType(t); }

  void SetPointedToType(Type* t)       { m_PointedToType = t; }
  Type::Pointer GetPointedToType() { return m_PointedToType; }
  Type::ConstPointer GetPointedToType() const
    { return m_PointedToType.RealPointer(); }
  
  virtual void Print(FILE*, unsigned int) const;

  virtual bool CheckComplete() const { return (m_PointedToType != NULL); }

protected:
  PointerType() {}
  PointerType(const Self&) {}
  void operator=(const Self&) {}
  virtual ~PointerType() {}
  
private:
  Type::Pointer m_PointedToType;
};


/**
 * Store a reference type.
 */
class ReferenceType: public Type
{
public:
  typedef ReferenceType             Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  virtual TypeOfObject GetTypeOfObject() const { return ReferenceType_id; }
  virtual const char* GetClassName() const { return "ReferenceType"; }

  static Pointer New();

  virtual String GetNameWithCV(const Type* =NULL) const;
  virtual String GetNameWithoutCV(const Type* =NULL) const;
  virtual String GetIndirectionWithCV() const;
  virtual String GetIndirectionWithoutCV() const;
  
  virtual void SetInternalType(Type* t) { this->SetReferencedType(t); }
  
  void SetReferencedType(Type* t)       { m_ReferencedType = t; }
  Type::Pointer GetReferencedType() { return m_ReferencedType; }
  Type::ConstPointer GetReferencedType() const
    { return m_ReferencedType.RealPointer(); }
  
  virtual void Print(FILE*, unsigned int) const;

  virtual bool CheckComplete() const { return (m_ReferencedType != NULL); }
  
protected:
  ReferenceType() {}
  ReferenceType(const Self&) {}
  void operator=(const Self&) {}
  virtual ~ReferenceType() {}
  
private:
  Type::Pointer m_ReferencedType;
};


/**
 * Store a function type.  This includes the funcion's return type,
 * argument types, and cv-qualifiers.
 */
class FunctionType: public Type
{
 public:
  typedef FunctionType              Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  virtual TypeOfObject GetTypeOfObject() const { return FunctionType_id; }
  virtual const char* GetClassName() const { return "FunctionType"; }

  static Pointer New();

  virtual String GetNameWithCV(const Type* =NULL) const;
  virtual String GetNameWithoutCV(const Type* =NULL) const;
  
  void SetReturns(Returns* r)       { m_Returns = r; }
  Returns::Pointer GetReturns() { return m_Returns; }
  Returns::ConstPointer GetReturns() const { return m_Returns.RealPointer(); }
  
  void AddArgument(Argument* a) { m_Arguments.insert(m_Arguments.end(), a); }
  const ArgumentContainer& GetArguments() const { return m_Arguments; }
  Argument::Pointer GetArgument(unsigned int a) { return m_Arguments[a]; }
  Argument::ConstPointer GetArgument(unsigned int a) const { return m_Arguments[a].RealPointer(); }
  unsigned int GetArgumentCount() const { return m_Arguments.size(); }

  void SetEllipsis(bool e) { m_Ellipsis = e; }
  bool GetEllipsis() const { return m_Ellipsis; }
  
  virtual void Print(FILE*, unsigned int) const;

protected:
  FunctionType() {}
  FunctionType(const Self&) {}
  void operator=(const Self&) {}
  virtual ~FunctionType() {}
  
private:
  Returns::Pointer   m_Returns;
  ArgumentContainer  m_Arguments;
  bool m_Ellipsis;
};


/**
 * Store a method type.  This includes the methods's function type, and
 * the class owning the method.
 */
class MethodType: public FunctionType
{
 public:
  typedef MethodType                Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  virtual TypeOfObject GetTypeOfObject() const { return MethodType_id; }
  virtual const char* GetClassName() const { return "MethodType"; }

  static Pointer New();

  virtual String GetNameWithCV(const Type* =NULL) const;
  virtual String GetNameWithoutCV(const Type* =NULL) const;
  
  void SetBaseType(BaseType* t) { m_BaseType = t; }
  BaseTypePointer GetBaseType() { return m_BaseType; }
  BaseTypeConstPointer GetBaseType() const { return m_BaseType.RealPointer(); }
  
  virtual void Print(FILE*, unsigned int) const;

  virtual bool CheckComplete() const { return (m_BaseType != NULL); }
  
protected:
  MethodType() {}
  MethodType(const Self&) {}
  void operator=(const Self&) {}
  virtual ~MethodType() {}
  
private:
  BaseTypePointer m_BaseType;
};



/**
 * Store an offset-type, which is a pointer to member.  This includes
 * the member's type, and the class owning the member.
 */
class OffsetType: public Type
{
 public:
  typedef OffsetType                Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  virtual TypeOfObject GetTypeOfObject() const { return OffsetType_id; }
  virtual const char* GetClassName() const { return "OffsetType"; }

  static Pointer New();

  virtual String GetNameWithCV(const Type* =NULL) const;
  virtual String GetNameWithoutCV(const Type* =NULL) const;
  
  void SetBaseType(BaseType* t) { m_BaseType = t; }
  BaseTypePointer GetBaseType() { return m_BaseType; }
  BaseTypeConstPointer GetBaseType() const { return m_BaseType.RealPointer(); }
  
  virtual void SetInternalType(Type* t) { this->SetMemberType(t); }
  
  void SetMemberType(Type* t)       { m_MemberType = t; }
  Type::Pointer GetMemberType() { return m_MemberType; }

  virtual void Print(FILE*, unsigned int) const;

  virtual bool CheckComplete() const
    { return ((m_BaseType != NULL) && (m_MemberType != NULL)); }
  
protected:
  OffsetType() {}
  OffsetType(const Self&) {}
  void operator=(const Self&) {}
  virtual ~OffsetType() {}
  
private:
  BaseTypePointer m_BaseType;
  Type::Pointer m_MemberType;
};


/**
 * Store an array type.  This includes the type of the array's elements,
 * and the size.
 */
class ArrayType: public Type
{
public:
  typedef ArrayType                Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  virtual TypeOfObject GetTypeOfObject() const { return ArrayType_id; }
  virtual const char* GetClassName() const { return "ArrayType"; }

  static Pointer New(int min, int max);
                     
  virtual String GetNameWithCV(const Type* =NULL) const;
  virtual String GetNameWithoutCV(const Type* =NULL) const;

  virtual void SetInternalType(Type* t) { this->SetElementType(t); }

  void SetElementType(Type* t)       { m_ElementType = t; }
  Type::Pointer GetElementType() { return m_ElementType; }
  
  virtual void Print(FILE*, unsigned int) const;

  virtual bool CheckComplete() const { return (m_ElementType != NULL); }
  
protected:
  ArrayType(int min, int max): m_Size(max-min+1) {}
  ArrayType(const Self&) {}
  void operator=(const Self&) {}
  virtual ~ArrayType() {}
  
private:
  int m_Size;
  Type::Pointer m_ElementType;
};


/** \class Namespace
 * Stores information for a namespace Context.  A namespace can contain other
 * namespaces as well as Class es.  A namespace can also have Function s.
 */
class Namespace: public Context
{
public:
  typedef Namespace                 Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  static Pointer New(const String& name);
  
  virtual TypeOfObject GetTypeOfObject() const { return Namespace_id; }
  virtual const char* GetClassName() const { return "Namespace"; }

  void AddNamespace(Namespace* in_namespace)
    { in_namespace->SetContext(this); m_Namespaces.insert(in_namespace); }
  const NamespaceContainer& GetNamespaces() const { return m_Namespaces; }

  void AddFunction(Function* function)
    { function->SetContext(this); m_Functions.insert(function); }
  const FunctionContainer& GetFunctions() const { return m_Functions; }
  
  virtual void Print(FILE*, unsigned int) const;
  
  typedef Context::Qualifiers  Qualifiers;
  typedef Context::QualifiersConstIterator QualifiersConstIterator;
  typedef Context::QualifiersInserter QualifiersInserter;  
  
  Class* LookupClass(const String&) const;
  Context* LookupName(QualifiersConstIterator,QualifiersConstIterator) const;
protected:
  Namespace(const String& name): Context(name) {}
  Namespace(const Self&): Context("") {}
  void operator=(const Self&) {}
  virtual ~Namespace() {}
  
private:
  NamespaceContainer m_Namespaces;
  FunctionContainer  m_Functions;
};


/**
 * Stores a function that is a member of a Class, Struct, or Union.
 */
class Method: public Function
{
public:
  typedef Method                    Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  static Pointer New(const String& name, Access access,
                     bool is_static, bool is_const);

  virtual TypeOfObject GetTypeOfObject() const { return Method_id; }
  virtual const char* GetClassName() const { return "Method"; }
  
  bool IsStatic() const { return m_Static; }
  bool IsConst() const { return m_Const; }
  Access GetAccess() const  { return m_Access; }

  virtual void Print(FILE*, unsigned int) const;
  
protected:
  Method(const String& in_name, Access access, bool is_static, bool is_const):
    Function(in_name), m_Access(access), m_Static(is_static), m_Const(is_const) {}
  Method(const Self&): Function("") {}
  void operator=(const Self&) {}
  virtual ~Method() {}
  
private:
  Access m_Access;
  bool m_Static;
  bool m_Const;
};


/**
 * Stores a function that is a constructor of a Class, Struct, or Union.
 */
class Constructor: public Method
{
public:
  typedef Constructor               Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  static Pointer New(Access access);

  virtual TypeOfObject GetTypeOfObject() const { return Constructor_id; }
  virtual const char* GetClassName() const { return "Constructor"; }
  
  virtual void Print(FILE*, unsigned int) const;
  
protected:
  Constructor(Access access):
    Method("", access, false, false) {}
  Constructor(const Self&): Method("", Public, false, false) {}
  void operator=(const Self&) {}
  virtual ~Constructor() {}
};


/**
 * Stores a function that is a destructor of a Class, Struct, or Union.
 */
class Destructor: public Method
{
public:
  typedef Destructor                Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  static Pointer New(Access access);

  virtual TypeOfObject GetTypeOfObject() const { return Destructor_id; }
  virtual const char* GetClassName() const { return "Destructor"; }
  
  virtual void Print(FILE*, unsigned int) const;
  
protected:
  Destructor(Access access):
    Method("", access, false, false) {}
  Destructor(const Self&): Method("", Public, false, false) {}
  void operator=(const Self&) {}
  virtual ~Destructor() {}
};


/**
 * Stores a function that is a type conversion operator
 * for a Class, Struct, or Union.
 */
class Converter: public Method
{
public:
  typedef Converter                 Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  static Pointer New(Access access, bool is_const);

  virtual TypeOfObject GetTypeOfObject() const { return Converter_id; }
  virtual const char* GetClassName() const { return "Converter"; }
  
  virtual void Print(FILE*, unsigned int) const;
  
protected:
  Converter(Access access, bool is_const):
    Method("", access, false, is_const) {}
  Converter(const Self&): Method("", Public, false, false) {}
  void operator=(const Self&) {}
  virtual ~Converter() {}
};


/**
 * Stores a function that is an operator for a Class, Struct, or Union.
 */
class OperatorMethod: public Method
{
public:
  typedef OperatorMethod            Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  static Pointer New(const String& name, Access access, bool is_const);

  virtual TypeOfObject GetTypeOfObject() const { return OperatorMethod_id; }
  virtual const char* GetClassName() const { return "OperatorMethod"; }
  
  virtual String GetCallName() const { return "operator"+this->GetName(); }
  
  virtual void Print(FILE*, unsigned int) const;
  
protected:
  OperatorMethod(const String& name, Access access, bool is_const):
    Method(name, access, false, is_const) {}
  OperatorMethod(const Self&): Method("", Public, false, false) {}
  void operator=(const Self&) {}
  virtual ~OperatorMethod() {}
};


/**
 * Stores a function that is an operator in a namespace.
 */
class OperatorFunction: public Function
{
public:
  typedef OperatorFunction          Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  static Pointer New(const String& name);

  virtual TypeOfObject GetTypeOfObject() const { return OperatorFunction_id; }
  virtual const char* GetClassName() const { return "OperatorFunction"; }
  
  virtual String GetCallName() const { return "operator"+this->GetName(); }
  
  virtual void Print(FILE*, unsigned int) const;
  
protected:
  OperatorFunction(const String& name): Function(name) {}
  OperatorFunction(const Self&): Function("") {}
  void operator=(const Self&) {}
  virtual ~OperatorFunction() {}
};



/** \class Class
 * Stores information for a class Context.  A class can have members, but
 * cannot contain other namespaces.
 */
class Class: public Context
{
public:
  typedef Class                     Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  static Pointer New(const String& name, Access access);

  virtual TypeOfObject GetTypeOfObject() const { return Class_id; }
  virtual const char* GetClassName() const { return "Class"; }

  void AddMethod(Method* x)
    { x->SetContext(this); m_Methods.insert(x); }
  const MethodContainer& GetMethods() const { return m_Methods; }

  void AddBaseClass(BaseClass* b) { m_BaseClasses.push_back(b); }
  const BaseClassContainer& GetBaseClasses() const { return m_BaseClasses; }

  bool IsPublic() const    { return (m_Access == Public);    }
  bool IsProtected() const { return (m_Access == Protected); }
  bool IsPrivate() const   { return (m_Access == Private);   }
  
  virtual void Print(FILE*, unsigned int) const;
  void PrintMethods(FILE*, unsigned int) const;
  void PrintBaseClasses(FILE*, unsigned int) const;
  
  typedef Context::Qualifiers  Qualifiers;
  typedef Context::QualifiersConstIterator QualifiersConstIterator;
  typedef Context::QualifiersInserter QualifiersInserter;  
  
  Context* LookupName(QualifiersConstIterator,QualifiersConstIterator) const;
protected:  
  Class(const String& name, Access access): Context(name), m_Access(access) {}
  Class(const Self&): Context("") {}
  void operator=(const Self&) {}
  virtual ~Class() {}
  
private:
  MethodContainer     m_Methods;
  BaseClassContainer  m_BaseClasses;
  Access              m_Access;
};


/** \class Struct
 * Equivalent to Class, except that original source declared it as
 * a struct instead of a class.
 */
class Struct: public Class
{
public:
  typedef Struct                    Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  static Pointer New(const String& name, Access access);
  
  virtual TypeOfObject GetTypeOfObject() const { return Struct_id; }
  virtual const char* GetClassName() const { return "Struct"; }
  
  virtual void Print(FILE*, unsigned int) const;
  
protected:
  Struct(const String& name, Access access): Class(name, access) {}
  Struct(const Self&): Class("", Public) {}
  void operator=(const Self&) {}
  virtual ~Struct() {}
};


/** \class Union
 * Equivalent to Class, except that original source declared it as
 * a union instead of a class.
 */
class Union: public Class
{
public:
  typedef Union                     Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  static Pointer New(const String& name, Access access);
  
  virtual TypeOfObject GetTypeOfObject() const { return Union_id; }
  virtual const char* GetClassName() const { return "Union"; }
  
  virtual void Print(FILE*, unsigned int) const;
  
protected:
  Union(const String& name, Access access): Class(name, access) {}
  Union(const Self&): Class("", Public) {}
  void operator=(const Self&) {}
  virtual ~Union() {}
};


/**
 * Provide the interface to a qualified name.
 * Also stores the innermost qualified name.
 */
class QualifiedName: public Named
{
public:
  typedef QualifiedName             Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  virtual TypeOfObject GetTypeOfObject() const { return QualifiedName_id; }
  virtual const char* GetClassName() const { return "QualifiedName"; }

  static Pointer New(const String& name);
  
  virtual String Get() const { return this->GetName(); }
  
protected:
  QualifiedName(const String& name): Named(name) {}
  QualifiedName(const Self&): Named("") {}
  void operator=(const Self&) {}
  virtual ~QualifiedName() {}
};


/**
 * Stores a non-innermost name qualifier, and the inner QualifiedName.
 */
class NameQualifier: public QualifiedName
{
public:
  typedef NameQualifier             Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  virtual TypeOfObject GetTypeOfObject() const { return NameQualifier_id; }
  virtual const char* GetClassName() const { return "NameQualifier"; }

  static Pointer New(const String& name);
  
  virtual void SetInternalQualifiedName(QualifiedName* n) { m_QualifiedName = n; }

  virtual String Get() const
    { return this->GetName()+"::"+m_QualifiedName->Get(); }
  
  virtual bool CheckComplete() const { return (m_QualifiedName != NULL); }
  
protected:
  NameQualifier(const String& name): QualifiedName(name) {}
  NameQualifier(const Self&): QualifiedName("") {}
  void operator=(const Self&) {}
  virtual ~NameQualifier() {}
  
private:
  QualifiedName::Pointer m_QualifiedName;
};


/**
 * Store the name of a base class, and support looking up the actual
 * Class, Struct, or Union.
 */
class BaseClass: public InternalObject
{
public:
  typedef BaseClass                 Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  static Pointer New(Access access);

  virtual void SetInternalQualifiedName(QualifiedName* n) { m_QualifiedName = n; }

  String GetQualifiedName() const { return m_QualifiedName->Get(); }
  
  bool HaveClass() const { return (m_Class != NULL); }
  
  Class::Pointer GetClass() { return m_Class; }
  Class::ConstPointer GetClass() const { return m_Class.RealPointer(); }

  Access GetAccess() const { return m_Access; }
  
  void Print(FILE*, unsigned int) const;
  
  virtual bool CheckComplete() const { return (m_QualifiedName != NULL); }
  
protected:
  BaseClass(Access access): m_Access(access) {}
  BaseClass(const Self&) {}
  void operator=(const Self&) {}
  virtual ~BaseClass() {}
  
private:
  QualifiedName::Pointer m_QualifiedName;
  Class::Pointer         m_Class;
  Access                 m_Access;
};


/**
 * Store the name of a base type for an MethodType or OffsetType.
 */
class BaseType: public InternalObject
{
public:
  typedef BaseType                  Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  static Pointer New();

  virtual void SetInternalQualifiedName(QualifiedName* n) { m_QualifiedName = n; }
  String GetQualifiedName() const { return m_QualifiedName->Get(); }
  
  bool HaveClass() const { return (m_Class != NULL); }
  
  Class::Pointer GetClass() { return m_Class; }
  Class::ConstPointer GetClass() const { return m_Class.RealPointer(); }
  
  virtual bool CheckComplete() const { return (m_QualifiedName != NULL); }
  
protected:
  BaseType() {}
  BaseType(const Self&) {}
  void operator=(const Self&) {}
  virtual ~BaseType() {}
  
private:
  QualifiedName::Pointer m_QualifiedName;
  Class::Pointer         m_Class;
};


/**
 * For unimplemented parts of a parser, this will allow it to absorb
 * any definition which has one internal type to be set.
 */
class UnimplementedTypeHolder: public Named
{
public:
  typedef UnimplementedTypeHolder   Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  virtual TypeOfObject GetTypeOfObject() const { return UnimplementedTypeHolder_id; }
  virtual const char* GetClassName() const { return "UnimplementedTypeHolder"; }
  
  static Pointer New();
  
  virtual void SetInternalType(Type* t) { this->SetType(t); }
  
  void SetType(Type* t) { m_Type = t; }
  
protected:
  UnimplementedTypeHolder(): Named("") {}
  UnimplementedTypeHolder(const Self&): Named("") {}
  void operator=(const Self&) {}
  virtual ~UnimplementedTypeHolder() {}
private:
  Type::Pointer m_Type;
};


/**
 * For unimplemented parts of a parser, this will allow it to absorb
 * any definition which has one internal name to be set.
 */
class UnimplementedNameHolder: public Named
{
public:
  typedef UnimplementedNameHolder   Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  virtual TypeOfObject GetTypeOfObject() const { return UnimplementedNameHolder_id; }
  virtual const char* GetClassName() const { return "UnimplementedNameHolder"; }
  
  static Pointer New();
  
  virtual void SetInternalQualifiedName(QualifiedName* n)
    { this->SetQualifiedName(n); }
  
  void SetQualifiedName(QualifiedName* n) { m_QualifiedName = n; }
  
protected:
  UnimplementedNameHolder(): Named("") {}
  UnimplementedNameHolder(const Self&): Named("") {}
  void operator=(const Self&) {}
  virtual ~UnimplementedNameHolder() {}
private:
  QualifiedName::Pointer m_QualifiedName;
};

} // namespace source

#endif
