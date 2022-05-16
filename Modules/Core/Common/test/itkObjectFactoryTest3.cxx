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

class FakeObject3 : public itk::Object
{
public:
  using Self = FakeObject3;
  using Superclass = itk::Object;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;

protected:
  FakeObject3() = default;
  ~FakeObject3() override = default;
};

class TestFactory3 : public itk::ObjectFactoryBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(TestFactory3);

  using Self = TestFactory3;
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
    return m_Description.c_str();
  }

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(TestFactory3, itk::ObjectFactoryBase);

  /** Register one factory of this type  */
  static void
  RegisterOneFactory()
  {
    auto factory = TestFactory3::New();
    itk::ObjectFactoryBase::RegisterFactory(factory);
  }

  /** Enable users to set a custom description for this instance. */
  itkSetStringMacro(Description);

private:
  TestFactory3()
  {
    this->RegisterOverride(typeid(FakeObject3).name(),
                           typeid(FakeObject3).name(),
                           "FakeObject3 factory override",
                           true,
                           itk::CreateObjectFunction<FakeObject3>::New());
  }
  std::string m_Description;
};

using DescriptionListType = std::vector<std::string>;

int
ListRegisteredFactories(const std::string & TestName, const DescriptionListType & expectedList)
{
  using FactoryListType = std::list<itk::ObjectFactoryBase *>;

  FactoryListType factories = itk::ObjectFactoryBase::GetRegisteredFactories();

  std::cout << "----- Registered factories -----" << std::endl;

  auto registeredFactoryItr = factories.begin();

  if (expectedList.size() != factories.size())
  {
    std::cerr << "In " << TestName << std::endl;
    std::cerr << "Size of expected factory list does not match size of registered factories list." << std::endl;
    return EXIT_FAILURE;
  }

  auto expectedItr = expectedList.begin();

  while (registeredFactoryItr != factories.end())
  {
    std::string description = (*registeredFactoryItr)->GetDescription();
    std::cout << "  Description: " << description << std::endl;

    if (description != *expectedItr)
    {
      std::cerr << "In " << TestName << std::endl;
      std::cerr << "Expected   " << *expectedItr << std::endl;
      std::cerr << "but Found  " << description << std::endl;
      return EXIT_FAILURE;
    }

    ++expectedItr;
    ++registeredFactoryItr;
  }

  return EXIT_SUCCESS;
}

int
itkObjectFactoryTest3(int, char *[])
{
  auto factory1 = TestFactory3::New();
  auto factory2 = TestFactory3::New();
  auto factory3 = TestFactory3::New();
  auto factory4 = TestFactory3::New();
  auto factory5 = TestFactory3::New();
  auto factory6 = TestFactory3::New();
  auto factory7 = TestFactory3::New();

  factory1->SetDescription("factory1");
  factory2->SetDescription("factory2");
  factory3->SetDescription("factory3");
  factory4->SetDescription("factory4");
  factory5->SetDescription("factory5");
  factory6->SetDescription("factory6");
  factory7->SetDescription("factory7");

  DescriptionListType descriptionList;

  itk::ObjectFactoryBase::UnRegisterAllFactories();

  itk::ObjectFactoryBase::RegisterFactory(factory1);
  itk::ObjectFactoryBase::RegisterFactory(factory2);
  itk::ObjectFactoryBase::RegisterFactory(factory3);

  descriptionList.push_back("factory1");
  descriptionList.push_back("factory2");
  descriptionList.push_back("factory3");

  int result = ListRegisteredFactories("TryA", descriptionList);

  if (result == EXIT_FAILURE)
  {
    return EXIT_FAILURE;
  }


  itk::ObjectFactoryBase::UnRegisterAllFactories();
  descriptionList.clear();

  itk::ObjectFactoryBase::RegisterFactory(factory1);
  itk::ObjectFactoryBase::RegisterFactory(factory2);
  itk::ObjectFactoryBase::RegisterFactory(factory3);
  itk::ObjectFactoryBase::RegisterFactory(factory4, itk::ObjectFactoryBase::InsertionPositionEnum::INSERT_AT_FRONT);

  descriptionList.push_back("factory4");
  descriptionList.push_back("factory1");
  descriptionList.push_back("factory2");
  descriptionList.push_back("factory3");

  result = ListRegisteredFactories("TryB", descriptionList);

  if (result == EXIT_FAILURE)
  {
    return EXIT_FAILURE;
  }


  itk::ObjectFactoryBase::RegisterFactory(factory5, itk::ObjectFactoryBase::InsertionPositionEnum::INSERT_AT_BACK);
  descriptionList.push_back("factory5");

  result = ListRegisteredFactories("TryC", descriptionList);

  if (result == EXIT_FAILURE)
  {
    return EXIT_FAILURE;
  }


  itk::ObjectFactoryBase::RegisterFactory(
    factory6, itk::ObjectFactoryBase::InsertionPositionEnum::INSERT_AT_POSITION, 3);
  descriptionList.clear();
  descriptionList.push_back("factory4"); // position 0
  descriptionList.push_back("factory1"); // position 1
  descriptionList.push_back("factory2"); // position 2
  descriptionList.push_back("factory6"); // position 3
  descriptionList.push_back("factory3"); // position 4
  descriptionList.push_back("factory5"); // position 5

  result = ListRegisteredFactories("TryD", descriptionList);

  if (result == EXIT_FAILURE)
  {
    return EXIT_FAILURE;
  }


  itk::ObjectFactoryBase::RegisterFactory(factory7, itk::ObjectFactoryBase::InsertionPositionEnum::INSERT_AT_BACK);
  descriptionList.push_back("factory7");

  result = ListRegisteredFactories("TryE", descriptionList);

  if (result == EXIT_FAILURE)
  {
    return EXIT_FAILURE;
  }


  return EXIT_SUCCESS;
}
