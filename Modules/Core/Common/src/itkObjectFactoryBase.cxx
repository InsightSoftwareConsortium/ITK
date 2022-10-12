/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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

#include "itkConfigure.h"
#ifdef ITK_DYNAMIC_LOADING
#  include "itkDynamicLoader.h"
#endif
#include "itkDirectory.h"
#include "itkLightObject.h"
#include "itkSingleton.h"
#include "itkVersion.h"
#include "itksys/SystemTools.hxx"
#include <cstring>
#include <algorithm>

namespace
{

using FactoryListType = std::list<itk::ObjectFactoryBase *>;

// Convenience function to synchronize lists and register the new factory,
// either with `RegisterFactoryInternal()` or with `RegisterFactory()`. Avoid
// duplicating factories that have already been registered and only add
// factories that were not already in the list.
void
SynchronizeList(FactoryListType & output, FactoryListType & input, bool internal)
{
  for (auto & factory : input)
  {
    int pos = -1;
    int curr = 0;
    for (auto & i : output)
    {
      if (typeid(*i) == typeid(*factory))
      {
        pos = curr;
        break; // factory already in internal factories.
      }
      ++curr;
    }
    if (pos == -1)
    {
      if (internal == true)
      {
        itk::ObjectFactoryBase::RegisterFactoryInternal(factory);
      }
      else
      {
        itk::ObjectFactoryBase::RegisterFactory(factory);
      }
    }
  }
}
} // end of anonymous namespace

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

class ObjectFactoryBase::ObjectFactoryBasePrivate : public LightObject
{
public:
  ~ObjectFactoryBasePrivate() override
  {
    itk::ObjectFactoryBase::UnRegisterAllFactories();
    for (auto & internalFactory : m_InternalFactories)
    {
      internalFactory->UnRegister();
    }
  }

  ObjectFactoryBasePrivate() = default;

  FactoryListType m_RegisteredFactories{};
  FactoryListType m_InternalFactories{};
  bool            m_Initialized{ false };
  bool            m_StrictVersionChecking{ false };
};

auto
ObjectFactoryBase::GetPimplGlobalsPointer() -> ObjectFactoryBasePrivate *
{
  const auto                 deleteLambda = []() { m_PimplGlobals->UnRegister(); };
  ObjectFactoryBasePrivate * globalInstance =
    Singleton<ObjectFactoryBasePrivate>("ObjectFactoryBase", SynchronizeObjectFactoryBase, deleteLambda);
  if (globalInstance != m_PimplGlobals)
  {
    SynchronizeObjectFactoryBase(globalInstance);
  }
  return m_PimplGlobals;
}

/** \class OverrideMap
 * \brief Internal implementation class for ObjectFactorBase.
 *
 * Create a sub class to shrink the size of the symbols
 * Also, so a forward reference can be put in ObjectFactoryBase.h
 * and a pointer member can be used.  This avoids other
 * classes including <map> and getting long symbol warnings.
 */
class ObjectFactoryBase::OverrideMap : public std::multimap<std::string, OverrideInformation>
{};

/**
 * Make possible for application developers to demand an exact match
 * between the application's ITK version and the dynamic libraries'
 * ITK version.
 */

void
ObjectFactoryBase::SetStrictVersionChecking(bool value)
{
  itkInitGlobalsMacro(PimplGlobals);
  m_PimplGlobals->m_StrictVersionChecking = value;
}

void
ObjectFactoryBase::StrictVersionCheckingOn()
{
  itkInitGlobalsMacro(PimplGlobals);
  m_PimplGlobals->m_StrictVersionChecking = true;
}

void
ObjectFactoryBase::StrictVersionCheckingOff()
{
  itkInitGlobalsMacro(PimplGlobals);
  m_PimplGlobals->m_StrictVersionChecking = false;
}

bool
ObjectFactoryBase::GetStrictVersionChecking()
{
  itkInitGlobalsMacro(PimplGlobals);
  return m_PimplGlobals->m_StrictVersionChecking;
}


/**
 * Create an instance of a named ITK object using the loaded
 * factories
 */
LightObject::Pointer
ObjectFactoryBase::CreateInstance(const char * itkclassname)
{
  ObjectFactoryBase::Initialize();

  for (auto & registeredFactory : m_PimplGlobals->m_RegisteredFactories)
  {
    LightObject::Pointer newobject = registeredFactory->CreateObject(itkclassname);
    if (newobject)
    {
      newobject->Register();
      return newobject;
    }
  }
  return nullptr;
}

std::list<LightObject::Pointer>
ObjectFactoryBase::CreateAllInstance(const char * itkclassname)
{
  ObjectFactoryBase::Initialize();

  std::list<LightObject::Pointer> created;
  for (auto & registeredFactory : m_PimplGlobals->m_RegisteredFactories)
  {
    std::list<LightObject::Pointer> moreObjects = registeredFactory->CreateAllObject(itkclassname);
    created.splice(created.end(), moreObjects);
  }
  return created;
}

/**
 * A one time initialization method.
 */
void
ObjectFactoryBase::InitializeFactoryList()
{
  itkInitGlobalsMacro(PimplGlobals);
}

/**
 * A one time initialization method.
 */
void
ObjectFactoryBase::Initialize()
{
  itkInitGlobalsMacro(PimplGlobals);

  if (!m_PimplGlobals->m_Initialized)
  {
    m_PimplGlobals->m_Initialized = true;
    ObjectFactoryBase::InitializeFactoryList();
    ObjectFactoryBase::RegisterInternal();
#if defined(ITK_DYNAMIC_LOADING) && !defined(ITK_WRAPPING)
    ObjectFactoryBase::LoadDynamicFactories();
#endif
  }
}

/**
 * Register any factories that are always present in ITK like
 * the OpenGL factory, currently this is not done.
 */
void
ObjectFactoryBase::RegisterInternal()
{
  itkInitGlobalsMacro(PimplGlobals);

  // Guarantee that no internal factories have already been registered.
  itkAssertInDebugAndIgnoreInReleaseMacro(m_PimplGlobals->m_RegisteredFactories.empty());
  m_PimplGlobals->m_RegisteredFactories.clear();

  // Register all factories registered by the
  // "RegisterFactoryInternal" method
  for (auto & internalFactory : m_PimplGlobals->m_InternalFactories)
  {
    m_PimplGlobals->m_RegisteredFactories.push_back(internalFactory);
  }
}

/**
 * Load all libraries in ITK_AUTOLOAD_PATH
 */
void
ObjectFactoryBase::LoadDynamicFactories()
{
#ifdef ITK_DYNAMIC_LOADING
  /**
   * follow PATH conventions
   */
#  ifdef _WIN32
  char PathSeparator = ';';
#  else
  char       PathSeparator = ':';
#  endif

  const std::string itk_autoload_env{ "ITK_AUTOLOAD_PATH" };
  std::string       LoadPath;
  if (!itksys::SystemTools::GetEnv(itk_autoload_env, LoadPath))
  {
    return;
  }
  if (LoadPath.empty())
  {
    return;
  }

  std::string::size_type EndSeparatorPosition = 0;
  std::string::size_type StartSeparatorPosition = 0;

  while (StartSeparatorPosition != std::string::npos)
  {
    StartSeparatorPosition = EndSeparatorPosition;

    /**
     * find PathSeparator in LoadPath
     */
    EndSeparatorPosition = LoadPath.find(PathSeparator, StartSeparatorPosition);
    if (EndSeparatorPosition == std::string::npos)
    {
      EndSeparatorPosition = LoadPath.size() + 1; // Add 1 to simulate
                                                  // having a separator
    }
    std::string CurrentPath = LoadPath.substr(StartSeparatorPosition, EndSeparatorPosition - StartSeparatorPosition);

    ObjectFactoryBase::LoadLibrariesInPath(CurrentPath.c_str());

    /**
     * Move past separator
     */
    if (EndSeparatorPosition > LoadPath.size())
    {
      StartSeparatorPosition = std::string::npos;
    }
    else
    {
      ++EndSeparatorPosition; // Skip the separator
    }
  }
#else // ITK_DYNAMIC_LOADING
  itkGenericExceptionMacro("ITK was not built with support for dynamic loading.");
#endif
}

#ifdef ITK_DYNAMIC_LOADING
/**
 * A file scope helper function to concat path and file into
 * a full path
 */
static std::string
CreateFullPath(const char * path, const char * file)
{
  std::string ret;

#  ifdef _WIN32
  const char sep = '\\';
#  else
  const char sep = '/';
#  endif
  /**
   * make sure the end of path is a separator
   */
  ret = path;
  if (!ret.empty() && ret.back() != sep)
  {
    ret += sep;
  }
  ret += file;
  return ret;
}
#endif // ITK_DYNAMIC_LOADING

/**
 * A file scope type alias to make the cast code to the load
 * function cleaner to read.
 */
using ITK_LOAD_FUNCTION = ObjectFactoryBase * (*)();

/**
 * A file scoped function to determine if a file has
 * the shared library extension in its name, this converts name to lower
 * case before the compare, DynamicLoader always uses
 * lower case for LibExtension values.
 */
inline bool
NameIsSharedLibrary(const char * name)
{
#ifdef ITK_DYNAMIC_LOADING
  std::string extension = itksys::DynamicLoader::LibExtension();

  std::string sname = name;
  if (sname.rfind(extension) == sname.size() - extension.size())
  {
    return true;
  }
#  ifdef __APPLE__
  // Need to also check libraries with a .dylib extension.
  extension = ".dylib";
#  endif
  const size_t extensionpos = sname.rfind(extension);
  if ((extensionpos != std::string::npos) && (extensionpos == (sname.size() - extension.size())))
  {
    return true;
  }
#else // ITK_DYNAMIC_LOADING
  (void)name;
  itkGenericExceptionMacro("ITK was not built with support for dynamic loading.");
#endif
  return false;
}

/**
 *
 */
void
ObjectFactoryBase::LoadLibrariesInPath(const char * path)
{
  auto dir = Directory::New();

  if (!dir->Load(path))
  {
    return;
  }
#ifdef ITK_DYNAMIC_LOADING

  /**
   * Attempt to load each file in the directory as a shared library
   */
  for (unsigned int i = 0; i < dir->GetNumberOfFiles(); ++i)
  {
    const char * file = dir->GetFile(i);
    /**
     * try to make sure the file has at least the extension
     * for a shared library in it.
     */
    if (NameIsSharedLibrary(file))
    {
      std::string fullpath = CreateFullPath(path, file);
      LibHandle   lib = DynamicLoader::OpenLibrary(fullpath.c_str());
      if (lib)
      {
        /**
         * Look for the symbol itkLoad in the library
         */
        auto loadfunction = (ITK_LOAD_FUNCTION)DynamicLoader::GetSymbolAddress(lib, "itkLoad");
        /**
         * if the symbol is found call it to create the factory
         * from the library
         */
        if (loadfunction)
        {
          ObjectFactoryBase * newfactory = (*loadfunction)();

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
#else // ITK_DYNAMIC_LOADING
  itkGenericExceptionMacro("ITK was not built with support for dynamic loading.");
#endif
}

/**
 * Recheck the ITK_AUTOLOAD_PATH for new libraries
 */
void
ObjectFactoryBase::ReHash()
{
  ObjectFactoryBase::UnRegisterAllFactories();
  ObjectFactoryBase::Initialize();
}

/**
 * initialize class members
 */
ObjectFactoryBase::ObjectFactoryBase()
  : m_OverrideMap{ std::make_unique<OverrideMap>() }
{
  m_LibraryHandle = nullptr;
  m_LibraryDate = 0;
}

/**
 * Unload the library and free the path string
 */
ObjectFactoryBase::~ObjectFactoryBase() = default;

/**
 * Add a factory to the registered list.
 * Intended to be used only by default built-in factories,
 * not to be used by loadable factories.
 */
void
ObjectFactoryBase::RegisterFactoryInternal(ObjectFactoryBase * factory)
{
  itkInitGlobalsMacro(PimplGlobals);

  if (factory->m_LibraryHandle != nullptr)
  {
    itkGenericExceptionMacro("A dynamic factory tried to be loaded internally!");
  }

  // Do not call general ::Initialize() method as that may invoke additional
  // libraries to be loaded and this method is called during static
  // initialization.
  ObjectFactoryBase::InitializeFactoryList();
  m_PimplGlobals->m_InternalFactories.push_back(factory);
  factory->Register();
  // if the internal factories have already been register add this one too
  if (m_PimplGlobals->m_Initialized)
  {
    m_PimplGlobals->m_RegisteredFactories.push_back(factory);
  }
}

/**
 * Add a factory to the registered list
 */
bool
ObjectFactoryBase::RegisterFactory(ObjectFactoryBase * factory, InsertionPositionEnum where, size_t position)
{
  itkInitGlobalsMacro(PimplGlobals);

  if (factory->m_LibraryHandle == nullptr)
  {
    const char nonDynamicName[] = "Non-Dynamicaly loaded factory";
    factory->m_LibraryPath = nonDynamicName;
  }
  else
  {
    // Factories must only be loaded once
    for (auto & registeredFactory : m_PimplGlobals->m_RegisteredFactories)
    {
      if (registeredFactory->m_LibraryPath == factory->m_LibraryPath)
      {
        itkGenericOutputMacro(<< factory->m_LibraryPath << " is already loaded");
        return false;
      }
    }
  }
  if (strcmp(factory->GetITKSourceVersion(), Version::GetITKSourceVersion()) != 0)
  {
    if (m_PimplGlobals->m_StrictVersionChecking)
    {
      itkGenericExceptionMacro(<< "Incompatible factory version load attempt:"
                               << "\nRunning itk version :\n"
                               << Version::GetITKSourceVersion() << "\nAttempted loading factory version:\n"
                               << factory->GetITKSourceVersion() << "\nAttempted factory:\n"
                               << factory->m_LibraryPath << "\n");
    }
    else
    {
      itkGenericOutputMacro(<< "Possible incompatible factory load:"
                            << "\nRunning itk version :\n"
                            << Version::GetITKSourceVersion() << "\nLoaded factory version:\n"
                            << factory->GetITKSourceVersion() << "\nLoading factory:\n"
                            << factory->m_LibraryPath << "\n");
    }
  }
  ObjectFactoryBase::Initialize();

  //
  //  Register the factory in the internal list at the requested location.
  //
  switch (where)
  {
    case InsertionPositionEnum::INSERT_AT_BACK:
    {
      if (position)
      {
        itkGenericExceptionMacro(
          << "position argument must not be used with InsertionPositionEnum::INSERT_AT_BACK option");
      }
      m_PimplGlobals->m_RegisteredFactories.push_back(factory);
      break;
    }
    case InsertionPositionEnum::INSERT_AT_FRONT:
    {
      if (position)
      {
        itkGenericExceptionMacro(
          << "position argument must not be used with InsertionPositionEnum::INSERT_AT_FRONT option");
      }
      m_PimplGlobals->m_RegisteredFactories.push_front(factory);
      break;
    }
    case InsertionPositionEnum::INSERT_AT_POSITION:
    {
      const size_t numberOfFactories = m_PimplGlobals->m_RegisteredFactories.size();
      if (position < numberOfFactories)
      {
        auto fitr = m_PimplGlobals->m_RegisteredFactories.begin();
        while (position--)
        {
          ++fitr;
        }

        m_PimplGlobals->m_RegisteredFactories.insert(fitr, factory);
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
ObjectFactoryBase::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Factory DLL path: " << m_LibraryPath.c_str() << "\n";
  os << indent << "Factory description: " << this->GetDescription() << std::endl;

  auto num = static_cast<int>(m_OverrideMap->size());
  os << indent << "Factory overrides " << num << " classes:" << std::endl;

  indent = indent.GetNextIndent();
  for (auto & i : *m_OverrideMap)
  {
    os << indent << "Class : " << i.first.c_str() << "\n";
    os << indent << "Overridden with: " << i.second.m_OverrideWithName.c_str() << std::endl;
    os << indent << "Enable flag: " << i.second.m_EnabledFlag << std::endl;
    os << indent << "Create object: " << i.second.m_CreateObject << std::endl;
    os << std::endl;
  }
}

/**
 *
 */
void
ObjectFactoryBase::DeleteNonInternalFactory(ObjectFactoryBase * factory)
{
  itkInitGlobalsMacro(PimplGlobals);

  // if factory is not internal then delete
  if (std::find(m_PimplGlobals->m_InternalFactories.begin(), m_PimplGlobals->m_InternalFactories.end(), factory) ==
      m_PimplGlobals->m_InternalFactories.end())
  {
    factory->UnRegister();
  }
}

/**
 *
 */
void
ObjectFactoryBase::UnRegisterFactory(ObjectFactoryBase * factory)
{
  itkInitGlobalsMacro(PimplGlobals);

  for (auto i = m_PimplGlobals->m_RegisteredFactories.begin(); i != m_PimplGlobals->m_RegisteredFactories.end(); ++i)
  {
    if (factory == *i)
    {
      DeleteNonInternalFactory(factory);
      m_PimplGlobals->m_RegisteredFactories.remove(factory);
      return;
    }
  }
}

/**
 * unregister all factories and delete the m_RegisteredFactories list
 */
void
ObjectFactoryBase::UnRegisterAllFactories()
{
  itkInitGlobalsMacro(PimplGlobals);

  // Collect up all the library handles so they can be closed
  // AFTER the factory has been deleted.
  std::list<void *> libs;
  for (auto & registeredFactory : m_PimplGlobals->m_RegisteredFactories)
  {
    libs.push_back(static_cast<void *>(registeredFactory->m_LibraryHandle));
  }
  // Unregister each factory
  for (auto & registeredFactory : m_PimplGlobals->m_RegisteredFactories)
  {
    DeleteNonInternalFactory(registeredFactory);
  }
#ifdef ITK_DYNAMIC_LOADING
  // And delete the library handles all at once
  for (auto & lib : libs)
  {
    if (lib)
    {
      DynamicLoader::CloseLibrary(static_cast<LibHandle>(lib));
    }
  }
#endif
  m_PimplGlobals->m_RegisteredFactories.clear();
  m_PimplGlobals->m_Initialized = false;
}

/**
 *
 */
void
ObjectFactoryBase::RegisterOverride(const char *               classOverride,
                                    const char *               subclass,
                                    const char *               description,
                                    bool                       enableFlag,
                                    CreateObjectFunctionBase * createFunction)
{
  ObjectFactoryBase::OverrideInformation info;

  info.m_Description = description;
  info.m_OverrideWithName = subclass;
  info.m_EnabledFlag = enableFlag;
  info.m_CreateObject = createFunction;

  m_OverrideMap->insert(OverrideMap::value_type(classOverride, info));
}

LightObject::Pointer
ObjectFactoryBase::CreateObject(const char * itkclassname)
{
  auto start = m_OverrideMap->lower_bound(itkclassname);
  auto end = m_OverrideMap->upper_bound(itkclassname);

  for (auto i = start; i != end; ++i)
  {
    if (i != m_OverrideMap->end() && i->second.m_EnabledFlag)
    {
      return i->second.m_CreateObject->CreateObject();
    }
  }
  return nullptr;
}

std::list<LightObject::Pointer>
ObjectFactoryBase::CreateAllObject(const char * itkclassname)
{
  auto start = m_OverrideMap->lower_bound(itkclassname);
  auto end = m_OverrideMap->upper_bound(itkclassname);

  std::list<LightObject::Pointer> created;

  for (auto i = start; i != end; ++i)
  {
    if (i != m_OverrideMap->end() && i->second.m_EnabledFlag)
    {
      created.push_back(i->second.m_CreateObject->CreateObject());
    }
  }
  return created;
}

/**
 *
 */
void
ObjectFactoryBase::SetEnableFlag(bool flag, const char * className, const char * subclassName)
{
  auto start = m_OverrideMap->lower_bound(className);
  auto end = m_OverrideMap->upper_bound(className);

  for (auto i = start; i != end; ++i)
  {
    if (i->second.m_OverrideWithName == subclassName)
    {
      i->second.m_EnabledFlag = flag;
    }
  }
}

/**
 *
 */
bool
ObjectFactoryBase::GetEnableFlag(const char * className, const char * subclassName)
{
  auto start = m_OverrideMap->lower_bound(className);
  auto end = m_OverrideMap->upper_bound(className);

  for (auto i = start; i != end; ++i)
  {
    if (i->second.m_OverrideWithName == subclassName)
    {
      return i->second.m_EnabledFlag;
    }
  }
  return false;
}

/**
 *
 */
void
ObjectFactoryBase::Disable(const char * className)
{
  auto start = m_OverrideMap->lower_bound(className);
  auto end = m_OverrideMap->upper_bound(className);

  for (auto i = start; i != end; ++i)
  {
    i->second.m_EnabledFlag = false;
  }
}

/**
 *
 */
void
ObjectFactoryBase::SynchronizeObjectFactoryBase(void * objectFactoryBasePrivate)
{
  // We need to register the previously registered factories with the new pointer.
  // We keep track of the previously registered factory in `previousObjectFactoryBasePrivate`
  // but assign the new pointer to `m_PimplGlobals` so factories can be
  // registered directly with the new pointer.
  ObjectFactoryBasePrivate * previousObjectFactoryBasePrivate = m_PimplGlobals;
  m_PimplGlobals = static_cast<ObjectFactoryBasePrivate *>(objectFactoryBasePrivate);

  if (m_PimplGlobals && previousObjectFactoryBasePrivate)
  {
    SynchronizeList(m_PimplGlobals->m_InternalFactories, previousObjectFactoryBasePrivate->m_InternalFactories, true);
    SynchronizeList(
      m_PimplGlobals->m_RegisteredFactories, previousObjectFactoryBasePrivate->m_RegisteredFactories, false);
  }

  if (m_PimplGlobals && previousObjectFactoryBasePrivate && previousObjectFactoryBasePrivate != m_PimplGlobals)
  {
    m_PimplGlobals->Register();
    previousObjectFactoryBasePrivate->UnRegister();
  }
}

/**
 *
 */
std::list<ObjectFactoryBase *>
ObjectFactoryBase::GetRegisteredFactories()
{
  //  static SingletonIndex * singletonIndex = SingletonIndex::GetInstance();
  //  Unused(singletonIndex);
  ObjectFactoryBase::Initialize();
  return m_PimplGlobals->m_RegisteredFactories;
}

/**
 * Return a list of classes that this factory overrides.
 */
std::list<std::string>
ObjectFactoryBase::GetClassOverrideNames()
{
  std::list<std::string> ret;
  for (auto & i : *m_OverrideMap)
  {
    ret.push_back(i.first);
  }
  return ret;
}

/**
 * Return a list of the names of classes that override classes.
 */
std::list<std::string>
ObjectFactoryBase::GetClassOverrideWithNames()
{
  std::list<std::string> ret;
  for (auto & i : *m_OverrideMap)
  {
    ret.push_back(i.second.m_OverrideWithName);
  }
  return ret;
}

/**
 * Return a list of descriptions for class overrides
 */
std::list<std::string>
ObjectFactoryBase::GetClassOverrideDescriptions()
{
  std::list<std::string> ret;
  for (auto & i : *m_OverrideMap)
  {
    ret.push_back(i.second.m_Description);
  }
  return ret;
}

/**
 * Return a list of enable flags
 */
std::list<bool>
ObjectFactoryBase::GetEnableFlags()
{
  std::list<bool> ret;
  for (auto & i : *m_OverrideMap)
  {
    ret.push_back(i.second.m_EnabledFlag);
  }
  return ret;
}

/**
 * Return the path to a dynamically loaded factory. */
const char *
ObjectFactoryBase::GetLibraryPath()
{
  return m_LibraryPath.c_str();
}

ObjectFactoryBase::ObjectFactoryBasePrivate * ObjectFactoryBase::m_PimplGlobals;
} // end namespace itk
