#ifndef _configRep_h
#define _configRep_h

#include <string>
#include <vector>
#include <cstdio>
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

typedef std::vector<String> LinesContainer;
typedef LinesContainer::iterator LinesIterator;
typedef LinesContainer::const_iterator LinesConstIterator;

/**
 * Store source code text for custom functions (like create and delete).
 */
class CodeBlock: public ConfigureObject
{
public:
  typedef CodeBlock           Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  void AddLine(const char* line, unsigned long len)
    { m_Lines.push_back(String(line, len)); }

protected:
  void PrintLines(FILE*) const;
  
protected:
  CodeBlock() {}
  CodeBlock(const Self&) {}
  void operator=(const Self&) {}
  virtual ~CodeBlock() {}
  
private:
 LinesContainer  m_Lines;
};


/**
 * Store code for an object creation function.
 */
class Create: public CodeBlock
{
public:
  typedef Create                    Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  static Pointer New(void);
  
  void PrintFunction(FILE*, const String&) const;
  
protected:
  Create() {}
  Create(const Self&) {}
  void operator=(const Self&) {}
  virtual ~Create() {}
};


/**
 * Store code for an object deletion function.
 */
class Delete: public CodeBlock
{
public:
  typedef Delete                    Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  static Pointer New(void);
  
  void PrintFunction(FILE*, const String&) const;
  
protected:
  Delete() {}
  Delete(const Self&) {}
  void operator=(const Self&) {}
  virtual ~Delete() {}
};


/**
 * Information to wrap a single type.
 */
class WrapType: public ConfigureObject
{
public:
  typedef WrapType                  Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  static Pointer New(const String& name);

  const String& GetName(void) const { return m_Name; }

  void SetCreate(Create* c) { m_Create = c; }
  Create::Pointer GetCreate(void) { return m_Create; }
  Create::ConstPointer GetCreate(void) const { return m_Create.RealPointer(); }
  
  void SetDelete(Delete* d) { m_Delete = d; }
  Delete::Pointer GetDelete(void) { return m_Delete; }
  Delete::ConstPointer GetDelete(void) const { return m_Delete.RealPointer(); }
  
  void Print(FILE*) const;
  
protected:
  WrapType(const String& name): m_Name(name) {}
  WrapType(const Self&) {}
  void operator=(const Self&) {}
  virtual ~WrapType() {}
  
private:
  String m_Name;
  Create::Pointer m_Create;
  Delete::Pointer m_Delete;
};

typedef std::vector<WrapType::Pointer>  WrapTypesContainer;
typedef WrapTypesContainer::iterator    WrapTypesIterator;
typedef WrapTypesContainer::const_iterator    WrapTypesConstIterator;

/**
 * A collection of all configuration information for the wrappers.
 */
class WrapperConfiguration: public ConfigureObject
{
public:
  typedef WrapperConfiguration      Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  static Pointer New(const String& fileName);

  FILE* GetSourceXML(void) const;
  
  void SetDefaultCreate(Create* c) { m_DefaultCreate = c; }
  Create::Pointer GetDefaultCreate(void) { return m_DefaultCreate; }
  Create::ConstPointer GetDefaultCreate(void) const
    { return m_DefaultCreate.RealPointer(); }
  
  void SetDefaultDelete(Delete* d) { m_DefaultDelete = d; }
  Delete::Pointer GetDefaultDelete(void) { return m_DefaultDelete; }
  Delete::ConstPointer GetDefaultDelete(void) const
    { return m_DefaultDelete.RealPointer(); }

  void AddWrapType(WrapType* w) { m_WrapTypes.push_back(w); }

  void Print(FILE*) const;
  
protected:
  WrapperConfiguration(const String& fileName): m_FileName(fileName) {}
  WrapperConfiguration(const Self&) {}
  void operator=(const Self&) {}
  virtual ~WrapperConfiguration() {}
  
private:
  String m_FileName;
  Create::Pointer m_DefaultCreate;
  Delete::Pointer m_DefaultDelete;
  WrapTypesContainer  m_WrapTypes;
};

#endif
