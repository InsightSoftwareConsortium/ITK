/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#if defined( _MSC_VER )
#pragma warning ( disable : 4786 )
#endif

#include "itkObjectFactoryBase.h"
#include "itkDynamicLoader.h"
#include "itkDirectory.h"
#include "itkVersion.h"
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <algorithm>
#include <map>

namespace
{
class CleanUpObjectFactory
{
public:
  inline void Use()
  {}

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
bool operator==(const ObjectFactoryBase::OverrideInformation & rhs,
                const ObjectFactoryBase::OverrideInformation & lhs)
{
  return ( rhs.m_Description == lhs.m_Description
           && rhs.m_OverrideWithName == lhs.m_OverrideWithName );
}

/**
 * Add this for the SGI compiler which does not seem
 * to provide a default implementation as it should.
 */
bool operator<(const ObjectFactoryBase::OverrideInformation & rhs,
               const ObjectFactoryBase::OverrideInformation & lhs)
{
  return ( rhs.m_Description < lhs.m_Description
           && rhs.m_OverrideWithName < lhs.m_OverrideWithName );
}

/** \class StringOverMap
 * \brief Internal implementation class for ObjectFactorBase.
 *
 * Create a sub class to shrink the size of the symbols
 * Also, so a forward reference can be put in ObjectFactoryBase.h
 * and a pointer member can be used.  This avoids other
 * classes including <map> and getting long symbol warnings.
 */
typedef std::multimap< std::string, ObjectFactoryBase::OverrideInformation >
StringOverMapType;

/** \class OverRideMap
 * \brief Internal implementation class for ObjectFactorBase.
 */
class OverRideMap:public StringOverMapType
{
public:
};

/**
 * static list of factories.
 *
 * IMPORTANT: The initialization to zero has been purposely removed here.
 * We now rely on the initialization of the translation unit calling Initialize().
 * Therefore, here we do not do any further initialization, since it may
 * override the one that is already done in Initialize().
 *
 * By default the compiler will initialize this variable to Zero at the proper
 * time.
 *
 */
typedef std::list< ObjectFactoryBase * > FactoryListType;
namespace ObjectFactoryBasePrivate
{
FactoryListType * RegisteredFactories;
}

/**
 * Make possible for application developers to demand an exact match
 * between the application's ITK version and the dynamic libraries'
 * ITK version.
 */
bool ObjectFactoryBase::m_StrictVersionChecking = false;

void
ObjectFactoryBase::SetStrictVersionChecking( bool value )
{
  ObjectFactoryBase::m_StrictVersionChecking = value;
}

void
ObjectFactoryBase::StrictVersionCheckingOn()
{
  ObjectFactoryBase::m_StrictVersionChecking = true;
}

void
ObjectFactoryBase::StrictVersionCheckingOff()
{
  ObjectFactoryBase::m_StrictVersionChecking = false;
}

bool
ObjectFactoryBase::GetStrictVersionChecking()
{
  return ObjectFactoryBase::m_StrictVersionChecking;
}


/**
 * Create an instance of a named itk object using the loaded
 * factories
 */
LightObject::Pointer
ObjectFactoryBase
::CreateInstance(const char *itkclassname)
{
  if ( !ObjectFactoryBasePrivate::RegisteredFactories )
    {
    ObjectFactoryBase::Initialize();
    }

  for ( FactoryListType::iterator
        i = ObjectFactoryBasePrivate::RegisteredFactories->begin();
        i != ObjectFactoryBasePrivate::RegisteredFactories->end(); ++i )
    {
    LightObject::Pointer newobject = ( *i )->CreateObject(itkclassname);
    if ( newobject )
      {
      newobject->Register();
      return newobject;
      }
    }
  return 0;
}

std::list< LightObject::Pointer >
ObjectFactoryBase
::CreateAllInstance(const char *itkclassname)
{
  if ( !ObjectFactoryBasePrivate::RegisteredFactories )
    {
    ObjectFactoryBase::Initialize();
    }
  std::list< LightObject::Pointer > created;
  for ( FactoryListType::iterator
        i = ObjectFactoryBasePrivate::RegisteredFactories->begin();
        i != ObjectFactoryBasePrivate::RegisteredFactories->end(); ++i )
    {
    std::list< LightObject::Pointer > moreObjects = ( *i )->CreateAllObject(itkclassname);
    created.splice(created.end(), moreObjects);
    }
  return created;
}

/**
 * A one time initialization method.
 */
void
ObjectFactoryBase
::InitializeFactoryList()
{
  CleanUpObjectFactoryGlobal.Use();
  /**
   * Don't do anything if we are already initialized
   */
  if ( ObjectFactoryBasePrivate::RegisteredFactories )
    {
    return;
    }

  ObjectFactoryBasePrivate::RegisteredFactories = new FactoryListType;
}

/**
 * A one time initialization method.
 */
void
ObjectFactoryBase
::Initialize()
{
  ObjectFactoryBase::InitializeFactoryList();
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
{}

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
  if ( getenv("ITK_AUTOLOAD_PATH") )
    {
    LoadPath = getenv("ITK_AUTOLOAD_PATH");
    }
  else
    {
    return;
    }

  if ( LoadPath.size() == 0 )
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
    if ( EndSeparatorPosition == std::string::npos )
      {
      EndSeparatorPosition = LoadPath.size() + 1; // Add 1 to simulate
                                                  // having a separator
      }
    std::string CurrentPath =
      LoadPath.substr(StartSeparatorPosition,
                      EndSeparatorPosition - StartSeparatorPosition);

    ObjectFactoryBase::LoadLibrariesInPath( CurrentPath.c_str() );

    /**
     * Move past separator
     */
    if ( EndSeparatorPosition > LoadPath.size() )
      {
      StartSeparatorPosition = std::string::npos;
      }
    else
      {
      EndSeparatorPosition++; // Skip the separator
      }
    }
}

/**
 * A file scope helper function to concat path and file into
 * a full path
 */
static std::string
CreateFullPath(const char *path, const char *file)
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
  if ( !ret.empty() && ret[ret.size() - 1] != sep )
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
typedef ObjectFactoryBase * ( *ITK_LOAD_FUNCTION )();

/**
 * A file scoped function to determine if a file has
 * the shared library extension in its name, this converts name to lower
 * case before the compare, DynamicLoader always uses
 * lower case for LibExtension values.
 */
inline bool
NameIsSharedLibrary(const char *name)
{
  std::string extension = itksys::DynamicLoader::LibExtension();

  std::string sname = name;
  if ( sname.rfind(extension) == sname.size() - extension.size() )
    {
    return true;
    }
#ifdef __APPLE__
  // Need to also check libraries with a .dylib extension.
  extension = ".dylib";
#endif
  if ( sname.rfind(extension) == sname.size() - extension.size() )
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
::LoadLibrariesInPath(const char *path)
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
    const char *file = dir->GetFile(i);
    /**
     * try to make sure the file has at least the extension
     * for a shared library in it.
     */
    if ( NameIsSharedLibrary(file) )
      {
      std::string fullpath = CreateFullPath(path, file);
      LibHandle   lib = DynamicLoader::OpenLibrary( fullpath.c_str() );
      if ( lib )
        {
        /**
         * Look for the symbol itkLoad in the library
         */
        ITK_LOAD_FUNCTION loadfunction =
          ( ITK_LOAD_FUNCTION ) DynamicLoader::GetSymbolAddress(lib, "itkLoad");
        /**
         * if the symbol is found call it to create the factory
         * from the library
         */
        if ( loadfunction )
          {
          ObjectFactoryBase *newfactory = ( *loadfunction )( );

          /**
           * initialize class members if load worked
           */
          newfactory->m_LibraryHandle = (void *)lib;
          newfactory->m_LibraryPath = fullpath;
          newfactory->m_LibraryDate = 0; // unused for now...
          ObjectFactoryBase::RegisterFactoryInternal(newfactory);
          }
        else
          {
          // In the past, some platforms crashed on the call
          // DynamicLoader::CloseLibrary(lib) if the lib has symbols
          // that the current executable is using.
          DynamicLoader::CloseLibrary(lib);
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
  m_OverrideMap->erase( m_OverrideMap->begin(), m_OverrideMap->end() );
  delete m_OverrideMap;
}

/**
 * Add a factory to the registered list.
 * Intended to be used only by default built-in factories,
 * not to be used by loadable factories.
 */
void
ObjectFactoryBase
::RegisterFactoryInternal(ObjectFactoryBase *factory)
{
  ObjectFactoryBase::InitializeFactoryList();
  ObjectFactoryBasePrivate::RegisteredFactories->push_back(factory);
  factory->Register();
}

/**
 * Add a factory to the registered list
 */
void
ObjectFactoryBase
::RegisterFactory(ObjectFactoryBase *factory, InsertionPositionType where, size_t position)
{
  if ( factory->m_LibraryHandle == 0 )
    {
    const char nonDynamicName[] = "Non-Dynamicaly loaded factory";
    factory->m_LibraryPath = nonDynamicName;
    }
  if ( strcmp( factory->GetITKSourceVersion(),
               Version::GetITKSourceVersion() ) != 0 )
    {
    if ( ObjectFactoryBase::m_StrictVersionChecking )
      {
      itkGenericExceptionMacro(<< "Incompatible factory version load attempt:"
                            << "\nRunning itk version :\n" << Version::GetITKSourceVersion()
                            << "\nAttempted loading factory version:\n" << factory->GetITKSourceVersion()
                            << "\nAttempted factory:\n" << factory->m_LibraryPath << "\n");
      }
    else
      {
      itkGenericOutputMacro(<< "Possible incompatible factory load:"
                            << "\nRunning itk version :\n" << Version::GetITKSourceVersion()
                            << "\nLoaded factory version:\n" << factory->GetITKSourceVersion()
                            << "\nLoading factory:\n" << factory->m_LibraryPath << "\n");
      }
    }
  ObjectFactoryBase::Initialize();

  //
  //  Register the factory in the internal list at the requested location.
  //
  switch( where )
    {
    case INSERT_AT_BACK:
      {
      if( position )
        {
        itkGenericExceptionMacro(<< "position argument must not be used with INSERT_AT_BACK option");
        }
      ObjectFactoryBasePrivate::RegisteredFactories->push_back(factory);
      break;
      }
    case INSERT_AT_FRONT:
      {
      if( position )
        {
        itkGenericExceptionMacro(<< "position argument must not be used with INSERT_AT_FRONT option");
        }
      ObjectFactoryBasePrivate::RegisteredFactories->push_front(factory);
      break;
      }
    case INSERT_AT_POSITION:
      {
      const size_t numberOfFactories = ObjectFactoryBasePrivate::RegisteredFactories->size();
      if( position < numberOfFactories )
        {
        typedef FactoryListType::iterator    FactoryIterator;
        FactoryIterator fitr = ObjectFactoryBasePrivate::RegisteredFactories->begin();

        while( position-- )
          {
          ++fitr;
          }

        ObjectFactoryBasePrivate::RegisteredFactories->insert(fitr,factory);
        break;
        }
      else
        {
        itkGenericExceptionMacro("Position" << position << " is outside range. \
          Only " << numberOfFactories << " factories are registered");
        }
      }
    }
  factory->Register();
}

/**
 *
 */
void
ObjectFactoryBase
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Factory DLL path: " << m_LibraryPath.c_str() << "\n";
  os << indent << "Factory description: " << this->GetDescription() << std::endl;

  int num = static_cast< int >( m_OverrideMap->size() );
  os << indent << "Factory overides " << num << " classes:" << std::endl;

  indent = indent.GetNextIndent();
  for ( OverRideMap::iterator i = m_OverrideMap->begin();
        i != m_OverrideMap->end(); ++i )
    {
    os << indent << "Class : " <<  ( *i ).first.c_str() << "\n";
    os << indent << "Overriden with: " <<  ( *i ).second.m_OverrideWithName.c_str()
       << std::endl;
    os << indent << "Enable flag: " << ( *i ).second.m_EnabledFlag
       << std::endl;
    os << indent << "Create object: " << ( *i ).second.m_CreateObject
       << std::endl;
    os << std::endl;
    }
}

/**
 *
 */
void
ObjectFactoryBase
::UnRegisterFactory(ObjectFactoryBase *factory)
{
  for ( std::list< ObjectFactoryBase * >::iterator i =
          ObjectFactoryBasePrivate::RegisteredFactories->begin();
        i != ObjectFactoryBasePrivate::RegisteredFactories->end(); ++i )
    {
    if ( factory == *i )
      {
      factory->UnRegister();
      ObjectFactoryBasePrivate::RegisteredFactories->remove(factory);
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
  if ( ObjectFactoryBasePrivate::RegisteredFactories )
    {
    // Collect up all the library handles so they can be closed
    // AFTER the factory has been deleted.
    std::list< void * > libs;
    for ( std::list< ObjectFactoryBase * >::iterator i =
            ObjectFactoryBasePrivate::RegisteredFactories->begin();
          i != ObjectFactoryBasePrivate::RegisteredFactories->end(); ++i )
      {
      libs.push_back( static_cast< void * >( ( *i )->m_LibraryHandle ) );
      }
    // Unregister each factory
    for ( std::list< ObjectFactoryBase * >::iterator f =
            ObjectFactoryBasePrivate::RegisteredFactories->begin();
          f != ObjectFactoryBasePrivate::RegisteredFactories->end(); ++f )
      {
      ( *f )->UnRegister();
      }
    // And delete the library handles all at once
    for ( std::list< void * >::iterator lib = libs.begin();
          lib != libs.end();
          ++lib )
      {
      if ( ( *lib ) )
        {
        DynamicLoader::CloseLibrary( static_cast< LibHandle >( *lib ) );
        }
      }
    delete ObjectFactoryBasePrivate::RegisteredFactories;
    ObjectFactoryBasePrivate::RegisteredFactories = 0;
    }
}

/**
 *
 */
void
ObjectFactoryBase
::RegisterOverride(const char *classOverride,
                   const char *subclass,
                   const char *description,
                   bool enableFlag,
                   CreateObjectFunctionBase *
                   createFunction)
{
  ObjectFactoryBase::OverrideInformation info;

  info.m_Description = description;
  info.m_OverrideWithName = subclass;
  info.m_EnabledFlag = enableFlag;
  info.m_CreateObject = createFunction;

  m_OverrideMap->insert( OverRideMap::value_type(classOverride, info) );
}

LightObject::Pointer
ObjectFactoryBase
::CreateObject(const char *itkclassname)
{
  OverRideMap::iterator start = m_OverrideMap->lower_bound(itkclassname);
  OverRideMap::iterator end = m_OverrideMap->upper_bound(itkclassname);

  for ( OverRideMap::iterator i = start; i != end; ++i )
    {
    if ( i != m_OverrideMap->end() && ( *i ).second.m_EnabledFlag )
      {
      return ( *i ).second.m_CreateObject->CreateObject();
      }
    }
  return 0;
}

std::list< LightObject::Pointer >
ObjectFactoryBase
::CreateAllObject(const char *itkclassname)
{
  OverRideMap::iterator start = m_OverrideMap->lower_bound(itkclassname);
  OverRideMap::iterator end = m_OverrideMap->upper_bound(itkclassname);

  std::list< LightObject::Pointer > created;

  for ( OverRideMap::iterator i = start; i != end; ++i )
    {
    if ( i != m_OverrideMap->end() && ( *i ).second.m_EnabledFlag )
      {
      created.push_back( ( *i ).second.m_CreateObject->CreateObject() );
      }
    }
  return created;
}

/**
 *
 */
void
ObjectFactoryBase
::SetEnableFlag(bool flag,
                const char *className,
                const char *subclassName)
{
  OverRideMap::iterator start = m_OverrideMap->lower_bound(className);
  OverRideMap::iterator end = m_OverrideMap->upper_bound(className);

  for ( OverRideMap::iterator i = start; i != end; ++i )
    {
    if ( ( *i ).second.m_OverrideWithName == subclassName )
      {
      ( *i ).second.m_EnabledFlag = flag;
      }
    }
}

/**
 *
 */
bool
ObjectFactoryBase
::GetEnableFlag(const char *className, const char *subclassName)
{
  OverRideMap::iterator start = m_OverrideMap->lower_bound(className);
  OverRideMap::iterator end = m_OverrideMap->upper_bound(className);

  for ( OverRideMap::iterator i = start; i != end; ++i )
    {
    if ( ( *i ).second.m_OverrideWithName == subclassName )
      {
      return ( *i ).second.m_EnabledFlag;
      }
    }
  return 0;
}

/**
 *
 */
void
ObjectFactoryBase
::Disable(const char *className)
{
  OverRideMap::iterator start = m_OverrideMap->lower_bound(className);
  OverRideMap::iterator end = m_OverrideMap->upper_bound(className);

  for ( OverRideMap::iterator i = start; i != end; ++i )
    {
    ( *i ).second.m_EnabledFlag = 0;
    }
}

/**
 *
 */
std::list< ObjectFactoryBase * >
ObjectFactoryBase
::GetRegisteredFactories()
{
  return *ObjectFactoryBasePrivate::RegisteredFactories;
}

/**
 * Return a list of classes that this factory overrides.
 */
std::list< std::string >
ObjectFactoryBase
::GetClassOverrideNames()
{
  std::list< std::string > ret;
  for ( OverRideMap::iterator i = m_OverrideMap->begin();
        i != m_OverrideMap->end(); ++i )
    {
    ret.push_back( ( *i ).first );
    }
  return ret;
}

/**
 * Return a list of the names of classes that override classes.
 */
std::list< std::string >
ObjectFactoryBase
::GetClassOverrideWithNames()
{
  std::list< std::string > ret;
  for ( OverRideMap::iterator i = m_OverrideMap->begin();
        i != m_OverrideMap->end(); ++i )
    {
    ret.push_back( ( *i ).second.m_OverrideWithName );
    }
  return ret;
}

/**
 * Retrun a list of descriptions for class overrides
 */
std::list< std::string >
ObjectFactoryBase
::GetClassOverrideDescriptions()
{
  std::list< std::string > ret;
  for ( OverRideMap::iterator i = m_OverrideMap->begin();
        i != m_OverrideMap->end(); ++i )
    {
    ret.push_back( ( *i ).second.m_Description );
    }
  return ret;
}

/**
 * Return a list of enable flags
 */
std::list< bool >
ObjectFactoryBase
::GetEnableFlags()
{
  std::list< bool > ret;
  for ( OverRideMap::iterator i = m_OverrideMap->begin();
        i != m_OverrideMap->end(); ++i )
    {
    ret.push_back( ( *i ).second.m_EnabledFlag );
    }
  return ret;
}

/**
 * Return the path to a dynamically loaded factory. */
const char *
ObjectFactoryBase
::GetLibraryPath()
{
  return m_LibraryPath.c_str();
}
} // end namespace itk
