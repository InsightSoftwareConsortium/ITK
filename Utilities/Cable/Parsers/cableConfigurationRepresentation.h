#ifndef _configRep_h
#define _configRep_h

#include <string>
#include "referenceCount.h"

typedef std::string String;

/**
 * Top-level base class for all configuration objects.
 */
class ConfigureObject: public Object
{
public:
  typedef ConfigureObject           Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

protected:
  ConfigureObject() {}
  ConfigureObject(const Self&) {}
  void operator=(const Self&) {}
  virtual ~ConfigureObject() {}
};


/**
 * A collection of all configuration information for the wrappers.
 */
class Configuration: public ConfigureObject
{
public:
  typedef Configuration             Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  static Pointer New(void);
  
protected:
  Configuration() {}
  Configuration(const Self&) {}
  void operator=(const Self&) {}
  virtual ~Configuration() {}
};

#endif
