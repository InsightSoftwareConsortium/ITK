#ifndef _configRep_h
#define _configRep_h

#include "referenceCount.h"

#include <string>
#include <set>
#include <map>
#include <vector>
#include <list>

namespace configuration
{

typedef std::string String;


/**
 * Enumeration of identifiers for object types.
 */
enum TypeOfObject {
  Undefined_id=0,

  Dependencies_id, CodeBlock_id, Element_id, Set_id, WrapperSet_id,
  Namespace_id, Headers_id, Package_id
};


/**
 * Top-level base class for all configuration objects.
 */
class ConfigureObject: public Object
{
public:
  typedef ConfigureObject           Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  virtual const char* GetClassName() const { return "ConfigureObject"; }
  virtual TypeOfObject GetTypeOfObject() const;
  
  bool IsPackage() const      { return (this->GetTypeOfObject() == Package_id); }
  bool IsDependencies() const { return (this->GetTypeOfObject() == Dependencies_id); }
  bool IsHeaders() const      { return (this->GetTypeOfObject() == Headers_id); }
  bool IsNamespace() const    { return (this->GetTypeOfObject() == Namespace_id); }
  bool IsCodeBlock() const    { return (this->GetTypeOfObject() == CodeBlock_id); }
  bool IsSet() const          { return (this->GetTypeOfObject() == Set_id); }
  bool IsWrapperSet() const   { return (this->GetTypeOfObject() == WrapperSet_id); }
  bool IsElement() const      { return (this->GetTypeOfObject() == Element_id); }

  virtual void AddCharacterData(const char*, unsigned long, bool);
  
protected:
  ConfigureObject() {}
  ConfigureObject(const Self&) {}
  void operator=(const Self&) {}
  virtual ~ConfigureObject() {}
};


/**
 * Store a set of package names on which a package depends.
 */
class Dependencies: public ConfigureObject
{
public:
  typedef Dependencies              Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  virtual const char* GetClassName() const { return "Dependencies"; }
  virtual TypeOfObject GetTypeOfObject() const { return Dependencies_id; }

  static Pointer New();
  
  void Add(const String&);
  
protected:
  Dependencies() {}
  Dependencies(const Self&) {}
  void operator=(const Self&) {}
  virtual ~Dependencies() {}
  
private:
  std::list<String>  m_PackageNames;
};


/**
 * Store a set of directories and header files.
 */
class Headers: public ConfigureObject
{
public:
  typedef Headers              Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  virtual const char* GetClassName() const { return "Headers"; }
  virtual TypeOfObject GetTypeOfObject() const { return Headers_id; }

  static Pointer New();
  
  void AddFile(const String&);
  void AddDirectory(const String&);
  
protected:
  Headers() {}
  Headers(const Self&) {}
  void operator=(const Self&) {}
  virtual ~Headers() {}
  
private:
  std::set<String>  m_Files;
  std::set<String>  m_Directories;
};


/**
 * Store a Set entry on the element stack during parsing.
 */
class Element: public ConfigureObject
{
public:
  typedef Element           Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  virtual const char* GetClassName() const { return "Element"; }
  virtual TypeOfObject GetTypeOfObject() const { return Element_id; }
  
  static Pointer New(const String&);
  
  void AddCharacterData(const char*, unsigned long, bool);
  const String& GetTag() const;
  const String& GetCode() const;
  
protected:
  Element(const String&);
  Element(const Self&) {}
  void operator=(const Self&) {}
  virtual ~Element() {}
  
private:
  /**
   * Hold the tag that will be concatenated to the name using this element.
   */
  String m_Tag;

  /**
   * Hold the code associated with the element.
   */
  String m_Code;
};


/**
 * Interface to any object that can be referenced with a name in the source.
 */
class Named: public ConfigureObject
{
public:
  typedef Named Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  const String& GetName() const { return m_Name; }
  
protected:
  Named(const String& in_name): m_Name(in_name) {}
  Named(const Self&) {}
  void operator=(const Self&) {}
  virtual ~Named() {}
private:
  /**
   * The unqualified name by which this is referenced.
   */
  String m_Name;
};


/**
 * Store source code text for custom functions (like create and delete).
 */
class CodeBlock: public Named
{
public:
  typedef CodeBlock           Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  virtual const char* GetClassName() const { return "CodeBlock"; }
  virtual TypeOfObject GetTypeOfObject() const { return CodeBlock_id; }
  
  static Pointer New(const String&);
  
  void AddCharacterData(const char*, unsigned long, bool);
  const String& GetCode() const;
  
protected:
  CodeBlock(const String& in_name): Named(in_name) {}
  CodeBlock(const Self&): Named("") {}
  void operator=(const Self&) {}
  virtual ~CodeBlock() {}
  
private:
  /**
   * Hold the code belonging to this CodeBlock.
   */
  String m_Code;
};


/**
 * Store a set of elements.  These will be used as template arguments and/or
 * wrapped types.
 */
class Set: public Named
{
public:
  typedef Set           Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  virtual const char* GetClassName() const { return "Set"; }
  virtual TypeOfObject GetTypeOfObject() const { return Set_id; }
  
  static Pointer New(const String&);
  
  void Add(const String&, const String&);
  void Add(const Set*);
  
  typedef std::map<String, String> ElementContainer;
  typedef ElementContainer::iterator  Iterator;
  typedef ElementContainer::const_iterator ConstIterator;
  
  Iterator Begin()            { return m_Elements.begin(); }
  Iterator End()              { return m_Elements.end(); }
  ConstIterator Begin() const { return m_Elements.begin(); }
  ConstIterator End() const   { return m_Elements.end(); }
  unsigned long Size() const  { return m_Elements.size(); }
  
protected:
  Set(const String& in_name): Named(in_name) {}
  Set(const Self&): Named("") {}
  void operator=(const Self&) {}
  virtual ~Set() {}
  
private:
  /**
   * Store the elements as a mapping from the tag to the code.
   * This is not a multi-map because we don't want two different arguments
   * to have the same tag.
   */
  ElementContainer  m_Elements;
};



/**
 * Store a set of elements to be wrapped.
 */
class WrapperSet: public Set
{
public:
  typedef WrapperSet           Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  virtual const char* GetClassName() const { return "WrapperSet"; }
  virtual TypeOfObject GetTypeOfObject() const { return WrapperSet_id; }
  
  static Pointer New();
  
  typedef Set::ElementContainer  ElementContainer;
  typedef Set::Iterator          Iterator;
  typedef Set::ConstIterator     ConstIterator;
  
protected:
  WrapperSet(): Set("") {}
  WrapperSet(const Self&): Set("") {}
  void operator=(const Self&) {}
  virtual ~WrapperSet() {}
};


/**
 * Represent a namespace in the configuration file.  This will correspond
 * to a namespace in the generated wrappers, as well as for Set and CodeBlock names
 * in the configuration itself.
 */
class Namespace: public Named
{
public:
  typedef Namespace                 Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  virtual const char* GetClassName() const { return "Namespace"; }
  virtual TypeOfObject GetTypeOfObject() const { return Namespace_id; }
  
  static Pointer New(const String&, const String&, Namespace*);

  typedef std::list<Named::Pointer> WrapperList;
  typedef WrapperList::const_iterator WrapperIterator;

  void AddWrapperSet(WrapperSet*);
  
  bool AddField(Named*);
  
  bool AddCode(CodeBlock*);
  bool AddSet(Set*);
  bool AddNamespace(Namespace*);
  
  Named* LookupName(const String&) const;
  
  Set* LookupSet(const String&) const;
  CodeBlock* LookupCode(const String&) const;
  Namespace* LookupNamespace(const String&) const;
  
  WrapperIterator BeginWrapperList() const { return m_WrapperList.begin(); }
  WrapperIterator EndWrapperList() const { return m_WrapperList.end(); }

  bool IsGlobalNamespace() const { return (m_EnclosingNamespace == NULL); }
  
  String GetQualifierString(const String&) const;
  
protected:
  Namespace(const String&, const String&, Namespace*);
  Namespace(const Self&): Named("") {}
  void operator=(const Self&) {}
  virtual ~Namespace() {}
  
private:
  typedef std::list<String>  QualifierList;
  typedef QualifierList::const_iterator QualifierListConstIterator;
  typedef std::back_insert_iterator<QualifierList>  QualifierListInserter;

  Named* LookupName(QualifierListConstIterator,
                    QualifierListConstIterator,
                    bool walkUpEnclosingScopes) const;
  
  bool ParseQualifiedName(const String&, QualifierListInserter) const;
  
  QualifierListConstIterator Next(QualifierListConstIterator) const;
private:
  /**
   * The name of this namespace.
   */
  String m_Name;
  
  /**
   * The prefix-separator of this namespace.  This is used when an output
   * language does not support namespaces to separate the namespace name from
   * the rest of an identifier when they are concatenated together.
   */
  String m_PrefixSeparator;
  
  /**
   * The enclosing namespace.  NULL only for the global namespace.
   * This is a real pointer to avoid circular references.
   */
  Namespace* m_EnclosingNamespace;

  typedef std::map<String, Named::Pointer>  Fields;
  /**
   * The set of fields that have been defined in this Namespace.
   */
  Fields m_Fields;
  
  /**
   * List of WrapperSets and Namespaces to that have been defined
   * in this Namespace, in order.
   */
  WrapperList m_WrapperList;
};


/**
 * A collection of all configuration information for this package of wrappers.
 */
class Package: public ConfigureObject
{
public:
  typedef Package      Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  virtual const char* GetClassName() const { return "Package"; }
  virtual TypeOfObject GetTypeOfObject() const { return Package_id; }

  static Pointer New(const String&);

  const String& GetName() const;
  
  void SetDependencies(Dependencies* dependencies)
    { m_Dependencies = dependencies; }
  Dependencies::Pointer GetDependencies() const
    { return m_Dependencies; }

  void SetHeaders(Headers* headers)
    { m_Headers = headers; }
  Headers::Pointer GetHeaders() const
    { return m_Headers; }
  
  Namespace::Pointer GetGlobalNamespace() const
    { return m_GlobalNamespace; }
  
protected:
  Package(const String&);
  Package(const Self&) {}
  void operator=(const Self&) {}
  virtual ~Package() {}
  
private:
  /**
   * The name of the package.
   */
  String m_Name;
  
  /**
   * The set of package dependencies for this package.
   */
  Dependencies::Pointer m_Dependencies;
  
  /**
   * The set of header files and directories needed for this package.
   */
  Headers::Pointer m_Headers;
  
  /**
   * The global namespace defined in the package.  All other namespaces
   * are contained within it.
   */
  Namespace::Pointer m_GlobalNamespace;
};


} // namespace configuration

#endif
