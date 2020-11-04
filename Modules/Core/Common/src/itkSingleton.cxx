/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#include "itkSingleton.h"


namespace
{
// This ensures that m_GlobalSingletonIndex has been initialized once the library
// has been loaded. In some cases, this call will perform the initialization.
// In other cases, static initializers like the IO factory initialization code
// will have done the initialization.
::itk::SingletonIndex * initializedGlobalSingletonIndex = ::itk::SingletonIndex::GetInstance();

/** \class GlobalSingletonIndexInitializer
 *
 * \brief Initialize a GlobalSingletonIndex and delete it on program
 * completion.
 * */
class GlobalSingletonIndexInitializer
{
public:
  using Self = GlobalSingletonIndexInitializer;
  using SingletonIndex = ::itk::SingletonIndex;

  GlobalSingletonIndexInitializer() = default;

  /** Delete the time stamp if it was created. */
  ~GlobalSingletonIndexInitializer()
  {
    delete m_GlobalSingletonIndex;
    m_GlobalSingletonIndex = nullptr;
  }

  /** Create the GlobalSingletonIndex if needed and return it. */
  static SingletonIndex *
  GetGlobalSingletonIndex()
  {
    if (m_GlobalSingletonIndex == nullptr)
    {
      m_GlobalSingletonIndex = new SingletonIndex;
      // To avoid being optimized out. The compiler does not like this
      // statement at a higher scope.
      Unused(initializedGlobalSingletonIndex);
    }
    return m_GlobalSingletonIndex;
  }

private:
  static SingletonIndex * m_GlobalSingletonIndex;
};

// Takes care of cleaning up the GlobalSingletonIndex
static GlobalSingletonIndexInitializer GlobalSingletonIndexInitializerInstance;
// Initialized by the compiler to zero
GlobalSingletonIndexInitializer::SingletonIndex * GlobalSingletonIndexInitializer::m_GlobalSingletonIndex;

} // end anonymous namespace


// may return NULL if string is not registered already
//
// access something like a std::map<std::string, void *> or
// registered globals, it may be possible to restrict the held
// classes to be derived from itk::LightObject, so dynamic cast can
// work, and could use some type of Holder<T> class for intrinsic types
namespace itk
{
void *
SingletonIndex::GetGlobalInstancePrivate(const char * globalName)
{
  SingletonData::iterator it;
  it = m_GlobalObjects.find(globalName);
  if (it == m_GlobalObjects.end())
  {
    return nullptr;
  }
  return std::get<0>(it->second);
}

// If globalName is already registered remove it from map,
// otherwise global is added to the singleton index under globalName
bool
SingletonIndex::SetGlobalInstancePrivate(const char *                globalName,
                                         void *                      global,
                                         std::function<void(void *)> func,
                                         std::function<void(void)>   deleteFunc)
{
  m_GlobalObjects.erase(globalName);
  m_GlobalObjects.insert(std::make_pair(globalName, std::make_tuple(global, func, deleteFunc)));
  return true;
}

SingletonIndex *
SingletonIndex::GetInstance()
{
  if (m_Instance == nullptr)
  {
    m_Instance = GlobalSingletonIndexInitializer::GetGlobalSingletonIndex();
  }
  return m_Instance;
}

void
SingletonIndex::SetInstance(Self * instance)
{
  m_Instance = instance;
}


SingletonIndex::~SingletonIndex()
{
  for (auto & pair : m_GlobalObjects)
  {
    std::get<2>(pair.second)();
  }
}

SingletonIndex::Self * SingletonIndex::m_Instance;


} // end namespace itk
