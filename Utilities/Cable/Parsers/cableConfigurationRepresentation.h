#ifndef _configRep_h
#define _configRep_h

#include "referenceCount.h"

typedef std::string String;

class ConfigureObject: public Object
{
public:
  typedef ConfigureObject           Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
};


class Configuration: public ConfigureObject
{
public:
  typedef Configuration             Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
};

#endif
