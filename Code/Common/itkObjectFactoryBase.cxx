/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkObjectFactoryBase.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkObjectFactoryBase.h"
#include "itkDynamicLoader.h"
#include "itkDirectory.h"
#include "itkVersion.h"
#include <stdlib.h>
#include <ctype.h>
#include <algorithm>
#include <map>

namespace
{
  
class CleanUpObjectFactory
{
public:
  inline void Use() 
  {
  }
  ~CleanUpObjectFactory()
  {
    itk::ObjectFactoryBase::UnRegisterAllFactories();
  }  
};
static CleanUpObjectFactory CleanUpObjectFactoryGlobal;
}




namespace itk
{

/**
 * Add this for the SGI compiler which does not seem
 * to provide a default implementation as it should.
 */
bool operator==(const ObjectFactoryBase::OverrideInformation& rhs, 
                const ObjectFactoryBase::OverrideInformation& lhs)
{
  return (rhs.m_Description == lhs.m_Description
          && rhs.m_OverrideWithName == lhs.m_OverrideWithName);
}

/**
 * Add this for the SGI compiler which does not seem
 * to provide a default implementation as it should.
 */
bool operator<(const ObjectFactoryBase::OverrideInformation& rhs, 
               const ObjectFactoryBase::OverrideInformation& lhs)
{
  return (rhs.m_Description < lhs.m_Description
          && rhs.m_OverrideWithName < lhs.m_OverrideWithName);
}


/** \class StringOverMap
 * \brief Internal implementation class for ObjectFactorBase.
 *
 * Create a sub class to shrink the size of the symbols
 * Also, so a forward reference can be put in ObjectFactoryBase.h
 * and a pointer member can be used.  This avoids other
 * classes including <map> and getting long symbol warnings.
 */
typedef std::multimap<std::string, ObjectFactoryBase::OverrideInformation> 
StringOverMapType;

/** \class OverRideMap
 * \brief Internal implementation class for ObjectFactorBase.
 */
class OverRideMap : public StringOverMapType
{
public:
};

/**
 * Initialize static list of factories.
 */
std::list<ObjectFactoryBase*>* 
ObjectFactoryBase::m_RegisteredFactories = 0;


/**
 * Create an instance of a named itk object using the loaded
 * factories
 */
LightObject::Pointer 
ObjectFactoryBase
::CreateInstance(const char* itkclassname)
{
  if ( !ObjectFactoryBase::m_RegisteredFactories )
    {
    ObjectFactoryBase::Initialize();
    }
  
  for ( std::list<ObjectFactoryBase*>::iterator 
          i = m_RegisteredFactories->begin();
        i != m_RegisteredFactories->end(); ++i )
    {
    LightObject::Pointer newobject = (*i)->CreateObject(itkclassname);
    if(newobject)
      {
      return newobject;
      }
    }
  return 0;
}


std::list<LightObject::Pointer>
ObjectFactoryBase
::CreateAllInstance(const char* itkclassname)
{
  if ( !ObjectFactoryBase::m_RegisteredFactories )
    {
    ObjectFactoryBase::Initialize();
    }
  std::list<LightObject::Pointer> created;
  for ( std::list<ObjectFactoryBase*>::iterator 
          i = m_RegisteredFactories->begin();
        i != m_RegisteredFactories->end(); ++i )
    {
    LightObject::Pointer newobject = (*i)->CreateObject(itkclassname);
    if(newobject)
      {
      created.push_back(newobject);
      }
    }
  return created;
}


/**
 * A one time initialization method.
 */
void 
ObjectFactoryBase
::Initialize()
{
  CleanUpObjectFactoryGlobal.Use();
  /**
   * Don't do anything if we are already initialized
   */
  if ( ObjectFactoryBase::m_RegisteredFactories )
    {
    return;
    }
  
  ObjectFactoryBase::m_RegisteredFactories =
    new std::list<ObjectFactoryBase*>;
  ObjectFactoryBase::RegisterDefaults();
  ObjectFactoryBase::LoadDynamicFactories();
}


/**
 * Register any factories that are always present in ITK like
 * the OpenGL factory, currently this is not done.
 */
void 
ObjectFactoryBase
::RegisterDefaults()
{
}

/**
 * Load all libraries in ITK_AUTOLOAD_PATH
 */
void 
ObjectFactoryBase
::LoadDynamicFactories()
{
  /**
   * follow PATH conventions
   */
#ifdef _WIN32
  char PathSeparator = ';';
#else
  char PathSeparator = ':';
#endif
  
  std::string LoadPath;
  if (getenv("ITK_AUTOLOAD_PATH"))
    {
    LoadPath = getenv("ITK_AUTOLOAD_PATH");
    }
  else
    {
    return;
    }

  if(LoadPath.size() == 0)
    {
    return;
    }
  std::string::size_type EndSeparatorPosition = 0;
  std::string::size_type StartSeparatorPosition = 0;
  while ( StartSeparatorPosition != std::string::npos )
    {
    StartSeparatorPosition = EndSeparatorPosition;
    /**
     * find PathSeparator in LoadPath
     */
    EndSeparatorPosition = LoadPath.find(PathSeparator, 
                                         StartSeparatorPosition);
    if(EndSeparatorPosition == std::string::npos)
      {
      EndSeparatorPosition = LoadPath.size();
      }
    std::string CurrentPath = 
      LoadPath.substr(StartSeparatorPosition, EndSeparatorPosition);
    ObjectFactoryBase::LoadLibrariesInPath(CurrentPath.c_str());
    /**
     * move past separator
     */
    if(EndSeparatorPosition == LoadPath.size())
      {
      StartSeparatorPosition = std::string::npos;
      }
    else
      {
      EndSeparatorPosition++;
      }
    }
}


/**
 * A file scope helper function to concat path and file into
 * a full path
 */
static std::string 
CreateFullPath(const char* path, const char* file)
{
  std::string ret;
#ifdef _WIN32
  const char sep = '\\';
#else
  const char sep = '/';
#endif
  /**
   * make sure the end of path is a separator
   */
  ret = path;
  if ( ret[ret.size()-1] != sep )
    {
    ret += sep;
    }
  ret += file;
  return ret;
}


/**
 * A file scope typedef to make the cast code to the load
 * function cleaner to read.
 */
typedef ObjectFactoryBase* (* ITK_LOAD_FUNCTION)();


/**
 * A file scoped function to determine if a file has
 * the shared library extension in its name, this converts name to lower
 * case before the compare, DynamicLoader always uses
 * lower case for LibExtension values.
 */
inline bool 
NameIsSharedLibrary(const char* name)
{
  std::string sname = name;
  if ( sname.find(DynamicLoader::LibExtension()) != std::string::npos )
    {
    return true;
    }
  return false;
}


/**
 *
 */
void 
ObjectFactoryBase
::LoadLibrariesInPath(const char* path)
{
  Directory::Pointer dir = Directory::New();
  if ( !dir->Load(path) )
    {
    return;
    }
  
  /**
   * Attempt to load each file in the directory as a shared library
   */
  for ( unsigned int i = 0; i < dir->GetNumberOfFiles(); i++ )
    {
    const char* file = dir->GetFile(i);
    /**
     * try to make sure the file has at least the extension
     * for a shared library in it.
     */
    if ( NameIsSharedLibrary(file) )
      {
      std::string fullpath = CreateFullPath(path, file);
      LibHandle lib = DynamicLoader::OpenLibrary(fullpath.c_str());
      if ( lib )
        {
        /**
   * Look for the symbol itkLoad in the library
   */
        ITK_LOAD_FUNCTION loadfunction
          = (ITK_LOAD_FUNCTION)DynamicLoader::GetSymbolAddress(lib, "itkLoad");
        /**
   * if the symbol is found call it to create the factory
         * from the library
   */
        if ( loadfunction )
          {
          ObjectFactoryBase* newfactory = (*loadfunction)();
          /**
     * initialize class members if load worked
     */
          newfactory->m_LibraryHandle = (void*)lib;
          newfactory->m_LibraryPath = fullpath;
          newfactory->m_LibraryDate = 0; // unused for now...
          ObjectFactoryBase::RegisterFactory(newfactory);
          }
        }
      }
    }
}


/**
 * Recheck the ITK_AUTOLOAD_PATH for new libraries
 */
void 
ObjectFactoryBase
::ReHash()
{
  ObjectFactoryBase::UnRegisterAllFactories();
  ObjectFactoryBase::Initialize();
}


/**
 * initialize class members
 */
ObjectFactoryBase::ObjectFactoryBase()
{
  m_LibraryHandle = 0;
  m_LibraryDate = 0;
  m_OverrideMap = new OverRideMap;
}


/**
 * Unload the library and free the path string
 */
ObjectFactoryBase
::~ObjectFactoryBase()
{
  if(m_LibraryHandle)
    {
    DynamicLoader::CloseLibrary((LibHandle)m_LibraryHandle);
    }
  m_OverrideMap->erase(m_OverrideMap->begin(), m_OverrideMap->end());
  delete m_OverrideMap;
}


/**
 * Add a factory to the registered list
 */
void 
ObjectFactoryBase
::RegisterFactory(ObjectFactoryBase* factory)
{
  if ( factory->m_LibraryHandle == 0 )
    {
    const char* nonDynamicName = "Non-Dynamicly loaded factory";
    factory->m_LibraryPath = nonDynamicName;
    }
  if ( strcmp(factory->GetITKSourceVersion(), 
              Version::GetITKSourceVersion()) != 0 )
    {
    itkGenericOutputMacro(<< "Possible incompatible factory load:" 
                          << "\nRunning itk version :\n" << Version::GetITKSourceVersion() 
                          << "\nLoaded Factory version:\n" << factory->GetITKSourceVersion()
                          << "\nLoading factory:\n" << factory->m_LibraryPath << "\n");
    }
  ObjectFactoryBase::Initialize();
  ObjectFactoryBase::m_RegisteredFactories->push_back(factory);
  factory->Register();
}


/**
 *
 */
void 
ObjectFactoryBase
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Factory DLL path: " << m_LibraryPath.c_str() << "\n";
  os << indent << "Factory description: " << this->GetDescription() << std::endl;

  int num = static_cast<int>( m_OverrideMap->size() );
  os << indent << "Factory overides " << num << " classes:" << std::endl;

  indent = indent.GetNextIndent();
  for(OverRideMap::iterator i = m_OverrideMap->begin();
      i != m_OverrideMap->end(); ++i)
    {
    os << indent << "Class : " <<  (*i).first.c_str() << "\n";
    os << indent << "Overriden with: " <<  (*i).second.m_OverrideWithName.c_str()
       << std::endl;
    os << indent << "Enable flag: " << (*i).second.m_EnabledFlag
       << std::endl;
    os << std::endl;
    }
}


/**
 *
 */
void 
ObjectFactoryBase
::UnRegisterFactory(ObjectFactoryBase* factory)
{ 
  for ( std::list<ObjectFactoryBase*>::iterator i = 
          m_RegisteredFactories->begin();
        i != m_RegisteredFactories->end(); ++i )
    {
    if ( factory == *i )
      {
      m_RegisteredFactories->remove(factory);
      factory->UnRegister();
      return;
      }
    }
}
  

/**
 * unregister all factories and delete the RegisteredFactories list
 */
void 
ObjectFactoryBase
::UnRegisterAllFactories()
{
  
  if ( ObjectFactoryBase::m_RegisteredFactories )
    {
    for ( std::list<ObjectFactoryBase*>::iterator i 
            = m_RegisteredFactories->begin();
          i != m_RegisteredFactories->end(); ++i )
      {
      (*i)->UnRegister();
      }
    delete ObjectFactoryBase::m_RegisteredFactories;
    ObjectFactoryBase::m_RegisteredFactories = 0;
    }
}


/**
 *
 */
void 
ObjectFactoryBase
::RegisterOverride(const char* classOverride,
                   const char* subclass,
                   const char* description,
                   bool enableFlag,
                   CreateObjectFunctionBase*
                   createFunction)
{
  ObjectFactoryBase::OverrideInformation info;
  info.m_Description = description;
  info.m_OverrideWithName = subclass;
  info.m_EnabledFlag = enableFlag;
  info.m_CreateObject = createFunction;
  m_OverrideMap->insert(OverRideMap::value_type(classOverride, info));
}


LightObject::Pointer 
ObjectFactoryBase
::CreateObject(const char* itkclassname)
{
  m_OverrideMap->find(itkclassname);
  OverRideMap::iterator pos = m_OverrideMap->find(itkclassname);
  if ( pos != m_OverrideMap->end() )
    {
    return (*pos).second.m_CreateObject->CreateObject();
    }
  return 0;
}


/**
 *
 */
void 
ObjectFactoryBase
::SetEnableFlag(bool flag,
                const char* className,
                const char* subclassName)
{
  OverRideMap::iterator start = m_OverrideMap->lower_bound(className);
  OverRideMap::iterator end = m_OverrideMap->upper_bound(className);
  for ( OverRideMap::iterator i = start; i != end; ++i )
    {
    if ( (*i).second.m_OverrideWithName == subclassName )
      {
      (*i).second.m_EnabledFlag = flag;
      }
    }
}


/**
 *
 */
bool 
ObjectFactoryBase
::GetEnableFlag(const char* className, const char* subclassName)
{
  OverRideMap::iterator start = m_OverrideMap->lower_bound(className);
  OverRideMap::iterator end = m_OverrideMap->upper_bound(className);
  for ( OverRideMap::iterator i = start; i != end; ++i )
    {
    if ( (*i).second.m_OverrideWithName == subclassName )
      {
      return (*i).second.m_EnabledFlag;
      }
    }
  return 0;
}


/**
 *
 */
void 
ObjectFactoryBase
::Disable(const char* className)
{
  OverRideMap::iterator start = m_OverrideMap->lower_bound(className);
  OverRideMap::iterator end = m_OverrideMap->upper_bound(className);
  for ( OverRideMap::iterator i = start; i != end; ++i )
    {
    (*i).second.m_EnabledFlag = 0;
    }
}


/**
 *
 */
std::list<ObjectFactoryBase*> 
ObjectFactoryBase
::GetRegisteredFactories()
{
  return *ObjectFactoryBase::m_RegisteredFactories;
}


/**
 * Return a list of classes that this factory overrides.
 */
std::list<std::string> 
ObjectFactoryBase
::GetClassOverrideNames()
{
  std::list<std::string> ret;
  for ( OverRideMap::iterator i = m_OverrideMap->begin();
        i != m_OverrideMap->end(); ++i )
    {
    ret.push_back((*i).first);
    }
  return ret;
}


/**
 * Return a list of the names of classes that override classes.
 */
std::list<std::string> 
ObjectFactoryBase
::GetClassOverrideWithNames()
{
  std::list<std::string> ret;
  for ( OverRideMap::iterator i = m_OverrideMap->begin();
        i != m_OverrideMap->end(); ++i )
    {
    ret.push_back((*i).second.m_OverrideWithName);
    }
  return ret;
}


/**
 * Retrun a list of descriptions for class overrides
 */
std::list<std::string> 
ObjectFactoryBase
::GetClassOverrideDescriptions()
{ 
  std::list<std::string> ret;
  for ( OverRideMap::iterator i = m_OverrideMap->begin();
        i != m_OverrideMap->end(); ++i )
    {
    ret.push_back((*i).second.m_Description);
    }
  return ret;
}


/**
 * Return a list of enable flags
 */
std::list<bool> 
ObjectFactoryBase
::GetEnableFlags()
{
  std::list<bool> ret;
  for( OverRideMap::iterator i = m_OverrideMap->begin();
       i != m_OverrideMap->end(); ++i)
    {
    ret.push_back((*i).second.m_EnabledFlag);
    }
  return ret;
}

} // end namespace itk
