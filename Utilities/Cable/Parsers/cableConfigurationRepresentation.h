#ifndef _configRep_h
#define _configRep_h

#include <map>
#include "internalRep.h"

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
  
  void PrintFunction(FILE*, const String& typeName) const;
  
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
  
  void PrintFunction(FILE*, const String& typeName) const;
  
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

  bool HaveClass(void) const { return (m_Class != NULL); }
  void SetClass(Class* c) { m_Class = c; }
  Class::Pointer GetClass(void) { return m_Class; }
  Class::ConstPointer GetClass(void) const { return m_Class.RealPointer(); }
  
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
  Class::Pointer  m_Class;
};

typedef std::map<String, WrapType::Pointer>  WrapTypesContainer;
typedef WrapTypesContainer::const_iterator   WrapTypesIterator;

/**
 * A collection of all configuration information for the wrappers.
 */
class WrapperConfiguration: public ConfigureObject
{
public:
  typedef WrapperConfiguration      Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  static Pointer New(const String& source, const String& dest);

  FILE* GetSourceXML(void) const;
  
  void SetOutputName(const String& dest) { m_Dest = dest; }
  FILE* GetOutputFile(const String& outputDirectory) const;
  String GetOutputName(void) const { return m_Dest; }
  
  void SetDefaultCreate(Create* c) { m_DefaultCreate = c; }
  Create::Pointer GetDefaultCreate(void) { return m_DefaultCreate; }
  Create::ConstPointer GetDefaultCreate(void) const
    { return m_DefaultCreate.RealPointer(); }
  
  void SetDefaultDelete(Delete* d) { m_DefaultDelete = d; }
  Delete::Pointer GetDefaultDelete(void) { return m_DefaultDelete; }
  Delete::ConstPointer GetDefaultDelete(void) const
    { return m_DefaultDelete.RealPointer(); }

  void AddWrapType(WrapType* w) { m_WrapTypes[w->GetName()] = w; }
  const WrapTypesContainer& GetWrapTypes(void) const { return m_WrapTypes; }
  
  void Print(FILE*) const;
  void PrintMissingTypes(FILE*) const;
  
  bool FindTypes(Namespace*);
  
protected:
  WrapperConfiguration(const String& source,
                       const String& dest):
    m_Source(source), m_Dest(dest) {}
  WrapperConfiguration(const Self&) {}
  void operator=(const Self&) {}
  virtual ~WrapperConfiguration() {}

private:
  void FindTypesInNamespace(Namespace*);
  void FindTypesInClass(Class*);

private:
  String m_Source;
  String m_Dest;
  Create::Pointer m_DefaultCreate;
  Delete::Pointer m_DefaultDelete;
  WrapTypesContainer  m_WrapTypes;
};

#endif
