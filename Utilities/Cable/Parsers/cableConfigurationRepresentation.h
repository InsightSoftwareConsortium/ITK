#ifndef _configRep_h
#define _configRep_h

#include <string>
#include <set>
#include <map>
#include <vector>
#include <list>

#include "referenceCount.h"

namespace configuration
{

typedef std::string String;


/**
 * Enumeration of identifiers for object types.
 */
enum TypeOfObject {
  Undefined_id=0,

  Dependencies_id, CodeBlock_id, Element_id, Set_id, WrapperSet_id,
  Package_id, Headers_id
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
  bool IsCodeBlock() const    { return (this->GetTypeOfObject() == CodeBlock_id); }
  bool IsSet() const          { return (this->GetTypeOfObject() == Set_id); }
  bool IsWrapperSet() const   { return (this->GetTypeOfObject() == WrapperSet_id); }
  bool IsElement() const      { return (this->GetTypeOfObject() == Element_id); }
  bool IsHeaders() const      { return (this->GetTypeOfObject() == Headers_id); }

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
 * Store source code text for custom functions (like create and delete).
 */
class CodeBlock: public ConfigureObject
{
public:
  typedef CodeBlock           Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  virtual const char* GetClassName() const { return "CodeBlock"; }
  virtual TypeOfObject GetTypeOfObject() const { return CodeBlock_id; }
  
  static Pointer New();
  
  void AddCharacterData(const char*, unsigned long, bool);
  void PrintCode(std::ostream&) const;
  const String& GetCode() const;
  
protected:
  CodeBlock();
  CodeBlock(const Self&) {}
  void operator=(const Self&) {}
  virtual ~CodeBlock() {}
  
private:
  /**
   * Hold the code belonging to this CodeBlock.
   */
  String m_Code;
};


/**
 * Store a Set entry on the element stack during parsing.
 */
class Element: public CodeBlock
{
public:
  typedef Element           Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  virtual const char* GetClassName() const { return "Element"; }
  virtual TypeOfObject GetTypeOfObject() const { return Element_id; }
  
  static Pointer New(const String&);
  
  const String& GetTag() const;
  
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
};


/**
 * Store a set of elements.  These will be used as template arguments and/or
 * wrapped types.
 */
class Set: public ConfigureObject
{
public:
  typedef Set           Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  virtual const char* GetClassName() const { return "Set"; }
  virtual TypeOfObject GetTypeOfObject() const { return Set_id; }
  
  static Pointer New();
  
  void Add(const String&, const String&);
  void Add(const Set*);
  void Print(std::ostream&) const;
  
  typedef std::map<String, String> ElementContainer;
  typedef ElementContainer::iterator  Iterator;
  typedef ElementContainer::const_iterator ConstIterator;
  
  Iterator Begin()            { return m_Elements.begin(); }
  Iterator End()              { return m_Elements.end(); }
  ConstIterator Begin() const { return m_Elements.begin(); }
  ConstIterator End() const   { return m_Elements.end(); }
  unsigned long Size() const  { return m_Elements.size(); }
  
protected:
  Set() {}
  Set(const Self&) {}
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
  
protected:
  WrapperSet() {}
  WrapperSet(const Self&) {}
  void operator=(const Self&) {}
  virtual ~WrapperSet() {}
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
  Dependencies::Pointer  m_Dependencies;
  
  /**
   * The set of header files and directories needed for this package.
   */
  Headers::Pointer m_Headers;
};


} // namespace configuration

#endif
