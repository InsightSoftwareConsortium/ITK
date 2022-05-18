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

#include "itkVersion.h"
#include "itkImage.h"
#include <list>

#define CHECK_FOR_VALUE(a, b)                                                            \
  {                                                                                      \
    if (a != b)                                                                          \
    {                                                                                    \
      std::cerr << "Error in " #a << " expected " << b << " but got " << a << std::endl; \
      return EXIT_FAILURE;                                                               \
    }                                                                                    \
  }                                                                                      \
  ITK_MACROEND_NOOP_STATEMENT

template <typename TPixel, unsigned int VImageDimension = 2>
class TestImage : public itk::Image<TPixel, VImageDimension>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(TestImage);

  /** Standard class type aliases.   */
  using Self = TestImage;
  using Superclass = itk::Image<TPixel, VImageDimension>;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(TestImage);

  /** Run-time type information (and related methods). */
  itkTypeMacro(TestImage, Image);

  // Methods from itkObject
  ~TestImage() override = default;
  TestImage() = default;
};

template <typename TPixel, unsigned int VImageDimension = 3>
class TestImage2 : public itk::Image<TPixel, VImageDimension>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(TestImage2);

  /** Standard class type aliases.   */
  using Self = TestImage2;
  using Superclass = itk::Image<TPixel, VImageDimension>;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(TestImage2);

  /** Run-time type information (and related methods). */
  itkTypeMacro(TestImage2, Image);

  // Methods from itkObject
  ~TestImage2() override = default;
  TestImage2() = default;
};

class TestFactory : public itk::ObjectFactoryBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(TestFactory);

  using Self = TestFactory;
  using Superclass = itk::ObjectFactoryBase;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;

  /** Class methods used to interface with the registered factories. */
  const char *
  GetITKSourceVersion() const override
  {
    return ITK_SOURCE_VERSION;
  }
  const char *
  GetDescription() const override
  {
    return "A Test Factory";
  }

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(TestFactory, itk::ObjectFactoryBase);

  /** Register one factory of this type  */
  static void
  RegisterOneFactory()
  {
    auto factory = TestFactory::New();
    itk::ObjectFactoryBase::RegisterFactory(factory);
  }

private:
  TestFactory()
  {
    this->RegisterOverride(typeid(itk::Image<short, 2>).name(),
                           typeid(TestImage<short, 2>).name(),
                           "Test image factory override",
                           true,
                           itk::CreateObjectFunction<TestImage<short, 2>>::New());
    this->RegisterOverride(typeid(itk::Image<short, 2>).name(),
                           typeid(TestImage2<short, 2>).name(),
                           "Test image factory override 2",
                           false,
                           itk::CreateObjectFunction<TestImage2<short, 2>>::New());
  }
};

using myPointer = itk::Image<short, 2>::Pointer;
bool
TestNewImage(myPointer v, const char * expectedClassName)
{
  std::cout << "v->GetNameOfClass(): " << v->GetNameOfClass();
  std::cout << ", expectedClassName: " << expectedClassName << std::endl;
  if (strcmp(v->GetNameOfClass(), expectedClassName) != 0)
  {
    std::cout << "Test Failed" << std::endl;
    return false;
  }
  return true;
}


int
itkObjectFactoryTest(int, char *[])
{
  auto factory = TestFactory::New();
  itk::ObjectFactoryBase::RegisterFactory(factory);

  // List all registered factories
  std::list<itk::ObjectFactoryBase *> factories = itk::ObjectFactoryBase::GetRegisteredFactories();

  std::cout << "----- Registered factories -----" << std::endl;
  for (auto & oneFactory : factories)
  {
    std::cout << "  Factory version: " << oneFactory->GetITKSourceVersion() << std::endl
              << "  Factory description: " << oneFactory->GetDescription() << std::endl;

    std::list<std::string>                 overrides = oneFactory->GetClassOverrideNames();
    std::list<std::string>                 names = oneFactory->GetClassOverrideWithNames();
    std::list<std::string>                 descriptions = oneFactory->GetClassOverrideDescriptions();
    std::list<bool>                        enableflags = oneFactory->GetEnableFlags();
    std::list<std::string>::const_iterator n = names.begin();
    std::list<std::string>::const_iterator d = descriptions.begin();
    std::list<bool>::const_iterator        e = enableflags.begin();
    for (std::list<std::string>::const_iterator o = overrides.begin(); o != overrides.end(); ++o, ++n, ++d, ++e)
    {
      std::cout << "    Override " << *o << " with " << *n << std::endl
                << "      described as \"" << *d << "\"" << std::endl
                << "      enabled " << *e << std::endl;
    }
  }
  std::cout << "----- -----" << std::endl;
  itk::Image<short, 2>::Pointer v = itk::Image<short, 2>::New();

  factory->Print(std::cout);

  int status = EXIT_SUCCESS;
  if (!TestNewImage(v, "TestImage"))
  {
    status = EXIT_FAILURE;
  }

  // disable all itk::Image creation with the
  factory->Disable(typeid(itk::Image<short, 2>).name());
  v = itk::Image<short, 2>::New();
  if (!TestNewImage(v, "Image"))
  {
    status = EXIT_FAILURE;
  }

  factory->SetEnableFlag(true, typeid(itk::Image<short, 2>).name(), typeid(TestImage2<short, 2>).name());

  std::cout << typeid(itk::Image<short, 2>).name() << " overridden by " << typeid(TestImage2<short, 2>).name()
            << std::endl
            << "   EnableFlag is "
            << factory->GetEnableFlag(typeid(itk::Image<short, 2>).name(), typeid(TestImage2<short, 2>).name())
            << std::endl;

  v = itk::Image<short, 2>::New();
  if (!TestNewImage(v, "TestImage2"))
  {
    status = EXIT_FAILURE;
  }

  factory->SetEnableFlag(false, typeid(itk::Image<short, 2>).name(), typeid(TestImage2<short, 2>).name());
  factory->SetEnableFlag(true, typeid(itk::Image<short, 2>).name(), typeid(TestImage<short, 2>).name());

  v = itk::Image<short, 2>::New();
  if (!TestNewImage(v, "TestImage"))
  {
    status = EXIT_FAILURE;
  }

  itk::ObjectFactoryBase::UnRegisterFactory(factory);

  v = itk::Image<short, 2>::New();
  if (!TestNewImage(v, "Image"))
  {
    status = EXIT_FAILURE;
  }

  TestFactory::RegisterOneFactory();
  v = itk::Image<short, 2>::New();
  if (!TestNewImage(v, "TestImage"))
  {
    status = EXIT_FAILURE;
  }

  itk::ObjectFactoryBase::SetStrictVersionChecking(false);
  CHECK_FOR_VALUE(itk::ObjectFactoryBase::GetStrictVersionChecking(), false);

  itk::ObjectFactoryBase::SetStrictVersionChecking(true);
  CHECK_FOR_VALUE(itk::ObjectFactoryBase::GetStrictVersionChecking(), true);

  itk::ObjectFactoryBase::StrictVersionCheckingOff();
  CHECK_FOR_VALUE(itk::ObjectFactoryBase::GetStrictVersionChecking(), false);

  itk::ObjectFactoryBase::StrictVersionCheckingOn();
  CHECK_FOR_VALUE(itk::ObjectFactoryBase::GetStrictVersionChecking(), true);

  return status;
}
