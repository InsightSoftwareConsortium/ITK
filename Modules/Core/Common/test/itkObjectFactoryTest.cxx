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

#include "itkVersion.h"
#include "itkImage.h"
#include <list>

#define CHECK_FOR_VALUE(a,b) \
  { \
  if( a != b ) \
    { \
    std::cerr << "Error in "#a << " expected " << b << " but got " << a << std::endl; \
    return EXIT_FAILURE; \
    } \
  }

template<typename TPixel, unsigned int VImageDimension=2>
class TestImage : public itk::Image< TPixel, VImageDimension >
{
public:
  /** Standard class typedefs.   */
  typedef TestImage                             Self;
  typedef itk::Image< TPixel, VImageDimension > Superclass;
  typedef itk::SmartPointer<Self>               Pointer;
  typedef itk::SmartPointer<const Self>         ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(TestImage);

  /** Run-time type information (and related methods). */
  itkTypeMacro(TestImage, Image);

  // Methods from itkObject
  virtual ~TestImage() ITK_OVERRIDE {}
  TestImage()
    {
    }

private:
  TestImage(const TestImage&);
  void operator=(const TestImage&);
};

template<typename TPixel, unsigned int VImageDimension=3>
class TestImage2 : public itk::Image< TPixel, VImageDimension >
{
public:
  /** Standard class typedefs.   */
  typedef TestImage2                            Self;
  typedef itk::Image< TPixel, VImageDimension > Superclass;
  typedef itk::SmartPointer<Self>               Pointer;
  typedef itk::SmartPointer<const Self>         ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(TestImage2);

  /** Run-time type information (and related methods). */
  itkTypeMacro(TestImage2, Image);

  // Methods from itkObject
  virtual ~TestImage2() ITK_OVERRIDE {}
  TestImage2()
    {
    }

private:
  TestImage2(const TestImage2&);
  void operator=(const TestImage2&);
};

class TestFactory : public itk::ObjectFactoryBase
{
public:
  typedef TestFactory                   Self;
  typedef itk::ObjectFactoryBase        Superclass;
  typedef itk::SmartPointer<Self>       Pointer;
  typedef itk::SmartPointer<const Self> ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char* GetITKSourceVersion() const ITK_OVERRIDE { return ITK_SOURCE_VERSION; }
  virtual const char* GetDescription() const ITK_OVERRIDE { return "A Test Factory"; }

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(TestFactory, itk::ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
  {
    TestFactory::Pointer factory = TestFactory::New();
    itk::ObjectFactoryBase::RegisterFactory(factory);
  }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(TestFactory);

  TestFactory()
    {
    this->RegisterOverride(typeid(itk::Image<short,2>).name(),
                           typeid(TestImage<short,2>).name(),
                           "Test image factory override",
                           true,
                           itk::CreateObjectFunction<TestImage<short,2> >::New());
    this->RegisterOverride(typeid(itk::Image<short,2>).name(),
                           typeid(TestImage2<short,2>).name(),
                           "Test image factory override 2",
                           false,
                           itk::CreateObjectFunction<TestImage2<short,2> >::New());
    }
};

typedef itk::Image<short,2>::Pointer myPointer;
bool TestNewImage(myPointer v, const char* expectedClassName)
{
  std::cout << "v->GetNameOfClass(): " << v->GetNameOfClass();
  std::cout << ", expectedClassName: " << expectedClassName << std::endl;
  if(strcmp(v->GetNameOfClass(), expectedClassName) != 0)
    {
    std::cout << "Test Failed" << std::endl;
    return false;
    }
  return true;
}


int itkObjectFactoryTest(int, char *[])
{
  TestFactory::Pointer factory = TestFactory::New();
  itk::ObjectFactoryBase::RegisterFactory(factory);

  // List all registered factories
  std::list<itk::ObjectFactoryBase *> factories =
    itk::ObjectFactoryBase::GetRegisteredFactories();

  std::cout << "----- Registered factories -----" << std::endl;
  for ( std::list<itk::ObjectFactoryBase*>::iterator
          f = factories.begin();
        f != factories.end(); ++f )
    {
    std::cout << "  Factory version: "
              << (*f)->GetITKSourceVersion() << std::endl
              << "  Factory description: "
              << (*f)->GetDescription() << std::endl;

    std::list<std::string> overrides = (*f)->GetClassOverrideNames();
    std::list<std::string> names = (*f)->GetClassOverrideWithNames();
    std::list<std::string> descriptions = (*f)->GetClassOverrideDescriptions();
    std::list<bool> enableflags = (*f)->GetEnableFlags();
    std::list<std::string>::const_iterator n = names.begin();
    std::list<std::string>::const_iterator d = descriptions.begin();
    std::list<bool>::const_iterator e = enableflags.begin();
    for ( std::list<std::string>::const_iterator o = overrides.begin();
          o != overrides.end(); ++o, ++n, ++d, e++ )
      {
      std::cout << "    Override " << *o
                << " with " << *n << std::endl
                << "      described as \"" << *d << "\"" << std::endl
                << "      enabled " << *e << std::endl;
      }
    }
  std::cout << "----- -----" << std::endl;
  itk::Image<short,2>::Pointer v = itk::Image<short,2>::New();

  factory->Print(std::cout);

  int status = EXIT_SUCCESS;
  if (!TestNewImage(v, "TestImage"))
    {
    status = EXIT_FAILURE;
    }

  // disable all itk::Image creation with the
  factory->Disable(typeid(itk::Image<short,2>).name());
  v = itk::Image<short,2>::New();
  if (!TestNewImage(v, "Image"))
    {
    status = EXIT_FAILURE;
    }

  factory->SetEnableFlag(true,
                         typeid(itk::Image<short,2>).name(),
                         typeid(TestImage2<short,2>).name());

  std::cout << typeid(itk::Image<short,2>).name()
            << " overridden by "
            << typeid(TestImage2<short,2>).name() << std::endl
            << "   EnableFlag is "
            <<   factory->GetEnableFlag(
              typeid(itk::Image<short,2>).name(),
              typeid(TestImage2<short,2>).name()) << std::endl;

  v = itk::Image<short,2>::New();
  if (!TestNewImage(v, "TestImage2"))
    {
    status = EXIT_FAILURE;
    }

  factory->SetEnableFlag(false,
                         typeid(itk::Image<short,2>).name(),
                         typeid(TestImage2<short,2>).name());
  factory->SetEnableFlag(true,
                         typeid(itk::Image<short,2>).name(),
                         typeid(TestImage<short,2>).name());

  v = itk::Image<short,2>::New();
  if (!TestNewImage(v, "TestImage"))
    {
    status = EXIT_FAILURE;
    }

  itk::ObjectFactoryBase::UnRegisterFactory(factory);

  v = itk::Image<short,2>::New();
  if (!TestNewImage(v, "Image"))
    {
    status = EXIT_FAILURE;
    }

  TestFactory::RegisterOneFactory();
  v = itk::Image<short,2>::New();
  if (!TestNewImage(v, "TestImage"))
    {
    status = EXIT_FAILURE;
    }

  itk::ObjectFactoryBase::SetStrictVersionChecking( false );
  CHECK_FOR_VALUE( itk::ObjectFactoryBase::GetStrictVersionChecking() , false );

  itk::ObjectFactoryBase::SetStrictVersionChecking( true );
  CHECK_FOR_VALUE( itk::ObjectFactoryBase::GetStrictVersionChecking() , true );

  itk::ObjectFactoryBase::StrictVersionCheckingOff();
  CHECK_FOR_VALUE( itk::ObjectFactoryBase::GetStrictVersionChecking() , false );

  itk::ObjectFactoryBase::StrictVersionCheckingOn();
  CHECK_FOR_VALUE( itk::ObjectFactoryBase::GetStrictVersionChecking() , true );

  return status;
}
