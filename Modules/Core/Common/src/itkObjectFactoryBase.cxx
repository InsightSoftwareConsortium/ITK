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

#include "itkDynamicLoader.h"
#include "itkDirectory.h"
#include "itkVersion.h"
#include <string.h>
#include <algorithm>


namespace itk
{

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
FactoryListType * m_RegisteredFactories;
FactoryListType * m_InternalFactories;
bool              m_Initialized;
}

namespace
{
class CleanUpObjectFactory
{
public:

  ~CleanUpObjectFactory()
  {
    itk::ObjectFactoryBase::UnRegisterAllFactories();

    if ( ObjectFactoryBasePrivate::m_InternalFactories )
      {
      for ( std::list< ObjectFactoryBase * >::iterator i =
              ObjectFactoryBasePrivate::m_InternalFactories->begin();
            i != ObjectFactoryBasePrivate::m_InternalFactories->end(); ++i )
        {
        (*i)->UnRegister();
        }
      delete ObjectFactoryBasePrivate::m_InternalFactories;
      }
  }
};
//NOTE:  KWStyle insists on m_ for m_CleanUpObjectFactoryGlobal
static CleanUpObjectFactory m_CleanUpObjectFactoryGlobal;
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
 * Create an instance of a named ITK object using the loaded
 * factories
 */
LightObject::Pointer
ObjectFactoryBase
::CreateInstance(const char *itkclassname)
{
  ObjectFactoryBase::Initialize();

  for ( FactoryListType::iterator
        i = ObjectFactoryBasePrivate::m_RegisteredFactories->begin();
        i != ObjectFactoryBasePrivate::m_RegisteredFactories->end(); ++i )
    {
    LightObject::Pointer newobject = ( *i )->CreateObject(itkclassname);
    if ( newobject )
      {
      newobject->Register();
      return newobject;
      }
    }
  return ITK_NULLPTR;
}

std::list< LightObject::Pointer >
ObjectFactoryBase
::CreateAllInstance(const char *itkclassname)
{

  ObjectFactoryBase::Initialize();

  std::list< LightObject::Pointer > created;
  for ( FactoryListType::iterator
        i = ObjectFactoryBasePrivate::m_RegisteredFactories->begin();
        i != ObjectFactoryBasePrivate::m_RegisteredFactories->end(); ++i )
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
  static_cast<void>(m_CleanUpObjectFactoryGlobal);
  /**
   * Don't do anything if we are already initialized
   */
  if ( !ObjectFactoryBasePrivate::m_RegisteredFactories )
    {
    ObjectFactoryBasePrivate::m_RegisteredFactories = new FactoryListType;
    }

  if ( !ObjectFactoryBasePrivate::m_InternalFactories )
    {
    ObjectFactoryBasePrivate::m_InternalFactories = new FactoryListType;
    }
}

/**
 * A one time initialization method.
 */
void
ObjectFactoryBase
::Initialize()
{
  if (!ObjectFactoryBasePrivate::m_Initialized ||
    !ObjectFactoryBasePrivate::m_RegisteredFactories )
    {
    ObjectFactoryBasePrivate::m_Initialized = true;
    ObjectFactoryBase::InitializeFactoryList();
    ObjectFactoryBase::RegisterInternal();
    ObjectFactoryBase::LoadDynamicFactories();
    }
}

/**
 * Register any factories that are always present in ITK like
 * the OpenGL factory, currently this is not done.
 */
void
ObjectFactoryBase
::RegisterInternal()
{
  // Guarantee that no internal factories have already been registered.
  itkAssertInDebugAndIgnoreInReleaseMacro( ObjectFactoryBasePrivate::m_RegisteredFactories->empty() );
  ObjectFactoryBasePrivate::m_RegisteredFactories->clear();

  // Register all factories registered by the
  // "RegisterFactoryInternal" method
  for ( std::list< ObjectFactoryBase * >::iterator i =
          ObjectFactoryBasePrivate::m_InternalFactories->begin();
        i != ObjectFactoryBasePrivate::m_InternalFactories->end(); ++i )
    {
    ObjectFactoryBasePrivate::m_RegisteredFactories->push_back( *i );
    }
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
  const size_t extensionpos = sname.rfind(extension);
  if ( ( extensionpos != std::string::npos )
    && ( extensionpos == ( sname.size() - extension.size() ) )
    )
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

          // ObjectFactoryBase::RegisterFactory may raise an exception if
          // a user enables StrictVersionChecking and a library in "path"
          // is a conflicting version; this exception should be propagated
          // to the user and not caught by ITK
          //
          // Do not edit the next comment line! It is intended to be parsed
          // by the Coverity analyzer.
          // coverity[fun_call_w_exception]
          if (!ObjectFactoryBase::RegisterFactory(newfactory))
            {
            DynamicLoader::CloseLibrary(lib);
            }
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
  m_LibraryHandle = ITK_NULLPTR;
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
  if ( factory->m_LibraryHandle != ITK_NULLPTR )
    {
    itkGenericExceptionMacro( "A dynamic factory tried to be loaded internally!" );
    }

  // Do not call general ::Initialize() method as that may envoke additional
  // libraries to be loaded and this method is called during static
  // initialization.
  ObjectFactoryBase::InitializeFactoryList();

  ObjectFactoryBasePrivate::m_InternalFactories->push_back(factory);
  factory->Register();

  // if the internal factories have already been register add this one too
  if ( ObjectFactoryBasePrivate::m_Initialized )
    {
    ObjectFactoryBasePrivate::m_RegisteredFactories->push_back(factory);
    }
}

/**
 * Add a factory to the registered list
 */
bool
ObjectFactoryBase
::RegisterFactory(ObjectFactoryBase *factory, InsertionPositionType where, size_t position)
{
  if ( factory->m_LibraryHandle == ITK_NULLPTR )
    {
    const char nonDynamicName[] = "Non-Dynamicaly loaded factory";
    factory->m_LibraryPath = nonDynamicName;
    }
  else
    {
    // Factories must only be loaded once
    for ( std::list< ObjectFactoryBase * >::iterator i =
            ObjectFactoryBasePrivate::m_RegisteredFactories->begin();
          i != ObjectFactoryBasePrivate::m_RegisteredFactories->end(); ++i )
      {
      if ((*i)->m_LibraryPath == factory->m_LibraryPath)
        {
        itkGenericOutputMacro(<< factory->m_LibraryPath << " is already loaded");
        return false;
        }
      }
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
      ObjectFactoryBasePrivate::m_RegisteredFactories->push_back(factory);
      break;
      }
    case INSERT_AT_FRONT:
      {
      if( position )
        {
        itkGenericExceptionMacro(<< "position argument must not be used with INSERT_AT_FRONT option");
        }
      ObjectFactoryBasePrivate::m_RegisteredFactories->push_front(factory);
      break;
      }
    case INSERT_AT_POSITION:
      {
      const size_t numberOfFactories = ObjectFactoryBasePrivate::m_RegisteredFactories->size();
      if( position < numberOfFactories )
        {
        typedef FactoryListType::iterator    FactoryIterator;
        FactoryIterator fitr = ObjectFactoryBasePrivate::m_RegisteredFactories->begin();

        while( position-- )
          {
          ++fitr;
          }

        ObjectFactoryBasePrivate::m_RegisteredFactories->insert(fitr,factory);
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
  return true;
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
::DeleteNonInternalFactory(  ObjectFactoryBase *factory )
{
  // if factory is not internal then delete
  if ( std::find( ObjectFactoryBasePrivate::m_InternalFactories->begin(),
                  ObjectFactoryBasePrivate::m_InternalFactories->end(),
                  factory )
       ==  ObjectFactoryBasePrivate::m_InternalFactories->end() )
    {
    factory->UnRegister();
    }
}

/**
 *
 */
void
ObjectFactoryBase
::UnRegisterFactory(ObjectFactoryBase *factory)
{
  if ( ObjectFactoryBasePrivate::m_RegisteredFactories )
    {
    for ( std::list< ObjectFactoryBase * >::iterator i =
            ObjectFactoryBasePrivate::m_RegisteredFactories->begin();
          i != ObjectFactoryBasePrivate::m_RegisteredFactories->end(); ++i )
      {
      if ( factory == *i )
        {
        DeleteNonInternalFactory(factory);
        ObjectFactoryBasePrivate::m_RegisteredFactories->remove(factory);
        return;
        }
      }
    }
}

/**
 * unregister all factories and delete the m_RegisteredFactories list
 */
void
ObjectFactoryBase
::UnRegisterAllFactories()
{
  if ( ObjectFactoryBasePrivate::m_RegisteredFactories )
    {
    // Collect up all the library handles so they can be closed
    // AFTER the factory has been deleted.
    std::list< void * > libs;
    for ( std::list< ObjectFactoryBase * >::iterator i =
            ObjectFactoryBasePrivate::m_RegisteredFactories->begin();
          i != ObjectFactoryBasePrivate::m_RegisteredFactories->end(); ++i )
      {
      libs.push_back( static_cast< void * >( ( *i )->m_LibraryHandle ) );
      }
    // Unregister each factory
    for ( std::list< ObjectFactoryBase * >::iterator f =
            ObjectFactoryBasePrivate::m_RegisteredFactories->begin();
          f != ObjectFactoryBasePrivate::m_RegisteredFactories->end(); ++f )
      {
      DeleteNonInternalFactory(*f);
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
    delete ObjectFactoryBasePrivate::m_RegisteredFactories;
    ObjectFactoryBasePrivate::m_RegisteredFactories = ITK_NULLPTR;
    ObjectFactoryBasePrivate::m_Initialized = false;
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
  return ITK_NULLPTR;
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

  ObjectFactoryBase::Initialize();

  return *ObjectFactoryBasePrivate::m_RegisteredFactories;
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
