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

  Dependencies_id, CodeBlock_id, Argument_id, ArgumentSet_id, Package_id,
  Headers_id
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
  bool IsArgumentSet() const  { return (this->GetTypeOfObject() == ArgumentSet_id); }
  bool IsArgument() const     { return (this->GetTypeOfObject() == Argument_id); }
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


typedef std::vector<String> CodeContainer;
typedef CodeContainer::iterator CodeIterator;
typedef CodeContainer::const_iterator CodeConstIterator;

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
  
protected:
  CodeBlock() {}
  CodeBlock(const Self&) {}
  void operator=(const Self&) {}
  virtual ~CodeBlock() {}
  
private:
  /**
   * Hold the code belonging to this CodeBlock.
   */
  CodeContainer  m_Code;
};


/**
 * Store a single argument for passing to a template parameter.
 * This is actually added to an ArgumentSet, and then deleted.  It is only
 * needed to hold a place on the element stack.
 */
class Argument: public CodeBlock
{
public:
  typedef Argument           Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  virtual const char* GetClassName() const { return "Argument"; }
  virtual TypeOfObject GetTypeOfObject() const { return Argument_id; }
  
  static Pointer New(const String&);
  
protected:
  Argument(const String&);
  Argument(const Self&) {}
  void operator=(const Self&) {}
  virtual ~Argument() {}
  
private:
  /**
   * Hold the tag that will be concatenated to the name using this argument.
   */
  String m_Tag;
};


typedef std::list<Argument::Pointer> ArgumentContainer;
typedef ArgumentContainer::iterator  ArgumentIterator;
typedef ArgumentContainer::const_iterator ArgumentConstIterator;


/**
 * Store a set of arguments for passing to a template parameter.
 */
class ArgumentSet: public ConfigureObject
{
public:
  typedef ArgumentSet           Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  virtual const char* GetClassName() const { return "ArgumentSet"; }
  virtual TypeOfObject GetTypeOfObject() const { return ArgumentSet_id; }
  
  static Pointer New();
  
  void Add(Argument*);
  void Add(ArgumentSet*);
  
  ArgumentIterator Begin()            { return m_Arguments.begin(); }
  ArgumentIterator End()              { return m_Arguments.end(); }
  ArgumentConstIterator Begin() const { return m_Arguments.begin(); }
  ArgumentConstIterator End() const   { return m_Arguments.end(); }
  
protected:
  ArgumentSet() {}
  ArgumentSet(const Self&) {}
  void operator=(const Self&) {}
  virtual ~ArgumentSet() {}
  
private:
  ArgumentContainer  m_Arguments;
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
