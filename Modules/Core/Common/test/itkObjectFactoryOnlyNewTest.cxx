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

// Test for interface class constructor override with
// implementation class constructor through object factory methods

#include "itkVersion.h"
#include "itkImage.h"
#include "itkTestingMacros.h"
#include <list>

// Define an interface class with no backend
template <typename TPixel, unsigned int VImageDimension = 2>
class TestImageInterfaceClass : public itk::Image<TPixel, VImageDimension>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(TestImageInterfaceClass);

  /** Standard class type aliases.   */
  using Self = TestImageInterfaceClass;
  using Superclass = itk::Image<TPixel, VImageDimension>;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;

  /** Method for creation through the object factory.*/
  itkFactoryOnlyNewMacro(TestImageInterfaceClass);

  /** Run-time type information (and related methods). */
  itkTypeMacro(TestImageInterfaceClass, Image);

  // Methods from itkObject
  ~TestImageInterfaceClass() override = default;
  TestImageInterfaceClass() = default;
};

// Define an implementation subclass
template <typename TPixel, unsigned int VImageDimension = 2>
class TestImageImplementationClass : public TestImageInterfaceClass<TPixel, VImageDimension>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(TestImageImplementationClass);

  /** Standard class type aliases.   */
  using Self = TestImageImplementationClass;
  using Superclass = TestImageImplementationClass<TPixel, VImageDimension>;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkFactorylessNewMacro(TestImageImplementationClass);

  /** Run-time type information (and related methods). */
  itkTypeMacro(TestImageImplementationClass, TestImage);

  // Methods from itkObject
  ~TestImageImplementationClass() override = default;
  TestImageImplementationClass() = default;
};

// Define factory to register implementation override for interface
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
    this->RegisterOverride(typeid(TestImageInterfaceClass<short, 2>).name(),
                           typeid(TestImageImplementationClass<short, 2>).name(),
                           "Test image factory override",
                           true,
                           itk::CreateObjectFunction<TestImageImplementationClass<short, 2>>::New());
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
itkObjectFactoryOnlyNewTest(int, char *[])
{
  // Verify that interface class cannot be instantiated
  // if no implementation factory is registered
  using PixelType = short;
  const size_t Dimension = 2;
  using TestImageInterfaceType = TestImageInterfaceClass<PixelType, Dimension>;
  ITK_TRY_EXPECT_EXCEPTION(TestImageInterfaceType::New());

  // Register implementation override
  auto factory = TestFactory::New();
  itk::ObjectFactoryBase::RegisterFactory(factory);

  // Log all registered factories
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

  TestImageInterfaceType::Pointer v;
  ITK_TRY_EXPECT_NO_EXCEPTION(v = TestImageInterfaceType::New());
  factory->Print(std::cout);
  if (!TestNewImage(v, "TestImageImplementationClass"))
  {
    return EXIT_FAILURE;
  }

  // disable interface class creation through the factory
  factory->Disable(typeid(TestImageInterfaceType).name());
  ITK_TRY_EXPECT_EXCEPTION(v = TestImageInterfaceType::New());

  // re-enable interface class creation through the factory
  using TestImageImplementationType = TestImageImplementationClass<PixelType, Dimension>;
  factory->SetEnableFlag(true, typeid(TestImageInterfaceType).name(), typeid(TestImageImplementationType).name());
  ITK_TRY_EXPECT_NO_EXCEPTION(v = TestImageInterfaceType::New());
  if (!TestNewImage(v, "TestImageImplementationClass"))
  {
    return EXIT_FAILURE;
  }

  // remove factory and verify interface class cannot be created
  itk::ObjectFactoryBase::UnRegisterFactory(factory);
  ITK_TRY_EXPECT_EXCEPTION(v = TestImageInterfaceType::New());

  return EXIT_SUCCESS;
}
