#ifndef _configRep_h
#define _configRep_h

#include <string>
#include <map>
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

  Dependencies_id, Package_id
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

protected:
  Package(const String&);
  Package(const Self&) {}
  void operator=(const Self&) {}
  virtual ~Package() {}
  
private:
  String m_Name;
  Dependencies::Pointer  m_Dependencies;
};


} // namespace configuration

#endif
