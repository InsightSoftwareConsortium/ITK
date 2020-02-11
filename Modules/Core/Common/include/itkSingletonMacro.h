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
/**
 * itkSingletonMacro.h defines macros that are used to declare and define
 * global variables across ITK with a global map that is used to synchronize
 * this variables across multiple instantiations of ITK if necessary.
 * Note: this is rarely necessary.
 */

#ifndef itkSingletonMacro_h
#define itkSingletonMacro_h

#define itkInitGlobalsMacro(VarName)                                                                                   \
  {                                                                                                                    \
    static auto * staticGlobals = Get##VarName##Pointer();                                                             \
    (void)staticGlobals;                                                                                               \
  }

#define itkGetGlobalDeclarationMacro(Type, VarName) static Type * Get##VarName##Pointer();

#define itkGetGlobalSimpleMacro(Class, Type, Name) itkGetGlobalInitializeMacro(Class, Type, Name, Class, (void)0)

#define itkGetGlobalValueMacro(Class, Type, Name, Value)                                                               \
  itkGetGlobalInitializeMacro(Class, Type, Name, Name, *m_##Name = Value)

#define itkGetGlobalInitializeMacro(Class, Type, VarName, SingletonName, Init)                                         \
  Type * Class::Get##VarName##Pointer()                                                                                \
  {                                                                                                                    \
    if (m_##VarName == nullptr)                                                                                        \
    {                                                                                                                  \
      static auto setLambda = [](void * a) {                                                                           \
        delete m_##VarName;                                                                                            \
        m_##VarName = static_cast<Type *>(a);                                                                          \
      };                                                                                                               \
      static auto deleteLambda = []() {                                                                                \
        delete m_##VarName;                                                                                            \
        m_##VarName = nullptr;                                                                                         \
      };                                                                                                               \
      auto * old_instance = SingletonIndex::GetInstance()->GetGlobalInstance<Type>(#SingletonName);                    \
      m_##VarName = Singleton<Type>(#SingletonName, setLambda, deleteLambda);                                          \
      if (old_instance == nullptr)                                                                                     \
      {                                                                                                                \
        Init;                                                                                                          \
      }                                                                                                                \
    }                                                                                                                  \
    return m_##VarName;                                                                                                \
  }

#endif
