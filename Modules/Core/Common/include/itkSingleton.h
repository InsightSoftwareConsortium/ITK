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
#ifndef itkSingleton_h
#define itkSingleton_h

#include "itkMacro.h"
#include "itkSingletonMacro.h"
#include <map>
#include <functional>

/** \brief A function which does nothing
 *
 * This function is to be used to mark parameters as unused to suppress
 * compiler warning. It can be used when the parameter needs to be named
 * (i.e. itkNotUsed cannot be used) but is not always used. It ensures
 * that the parameter is not optimized out.
 */
template <typename T>
inline void
Unused(const T &){};

namespace itk
{
/** \class SingletonIndex
 * \brief Implementation detail.
 *
 * \ingroup ITKCommon
 */

class ITKCommon_EXPORT SingletonIndex
{
public:
  /** Standard class types. */
  using Self = SingletonIndex;
  using SingletonData =
    std::map<std::string, std::tuple<void *, std::function<void(void *)>, std::function<void(void)>>>;

  // obtain a global registered in the singleton index under the
  // globalName, if unknown then nullptr will be returned.
  template <typename T>
  T *
  GetGlobalInstance(const char * globalName)
  {
    return static_cast<T *>(this->GetGlobalInstancePrivate(globalName));
  }


  // returns true if the globalName has not been registered yet.
  //
  // It is assumed that the global will remain valid until the start
  // of globals being destroyed.
  template <typename T>
  bool
  SetGlobalInstance(const char *                globalName,
                    T *                         global,
                    std::function<void(void *)> func,
                    std::function<void(void)>   deleteFunc)
  {
    return this->SetGlobalInstancePrivate(globalName, global, func, deleteFunc);
  }

  /** Set/Get the pointer to GlobalSingleton.
   * Note that SetGlobalSingleton is not concurrent thread safe. */
  static Self *
  GetInstance();
  static void
  SetInstance(Self * SingletonIndex);
  ~SingletonIndex();

private:
  // may return nullptr if string is not registered already
  //
  // access something like a std::map<std::string, void *> or
  // registered globals, it may be possible to restrict the held
  // classes to be derived from itk::LightObject, so dynamic cast can
  // work, and could use some type of Holder<T> class for intrinsic types
  void *
  GetGlobalInstancePrivate(const char * globalName);
  // If globalName is already registered than false is return,
  // otherwise global is added to the singleton index under globalName
  bool
  SetGlobalInstancePrivate(const char *                globalName,
                           void *                      global,
                           std::function<void(void *)> func,
                           std::function<void(void)>   deleteFunc);

  /** The static GlobalSingleton. This is initialized to nullptr as the first
   * stage of static initialization. It is then populated on the first call to
   * itk::Singleton::Modified() but it can be overridden with SetGlobalSingleton().
   * */
  SingletonData m_GlobalObjects;
  static Self * m_Instance;
  //  static SingletonIndexPrivate * m_GlobalSingleton;
};


// A wrapper for a global variable registered in the singleton index.
template <typename T>
T *
Singleton(const char * globalName, std::function<void(void *)> func, std::function<void(void)> deleteFunc)
{
  static SingletonIndex * singletonIndex = SingletonIndex::GetInstance();
  Unused(singletonIndex);
  T * instance = SingletonIndex::GetInstance()->GetGlobalInstance<T>(globalName);
  if (instance == nullptr)
  {
    instance = new T;
    if (!SingletonIndex::GetInstance()->SetGlobalInstance<T>(globalName, instance, func, deleteFunc))
    {
      delete instance;
      instance = nullptr;
    }
  }
  return instance;
}
} // end namespace itk

#endif
