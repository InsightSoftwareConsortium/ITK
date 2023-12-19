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


// First include the header file to be tested:
#include "itkObjectFactoryBase.h"

#include "itkLightObject.h"
#include "itkSmartPointer.h"
#include "itkVersion.h"

#include <gtest/gtest.h>


namespace
{

// The name of the test object type, as stored by the test object factory.
constexpr const char * testObjectTypeName = "Test Object";

// A "dummy" object type, just for unit test purposes.
class TestObject : public itk::LightObject
{};


// A factory of test objects.
class TestObjectFactory : public itk::ObjectFactoryBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(TestObjectFactory);

  using Self = TestObjectFactory;
  using Superclass = ObjectFactoryBase;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;

  const char *
  GetITKSourceVersion() const override
  {
    return ITK_SOURCE_VERSION;
  }

  const char *
  GetDescription() const override
  {
    return "Test Object Factory";
  }

  itkOverrideGetNameOfClassMacro(TestObjectFactory);

  itkFactorylessNewMacro(Self);

protected:
  TestObjectFactory()
  {
    this->RegisterOverride(
      testObjectTypeName, testObjectTypeName, testObjectTypeName, true, itk::CreateObjectFunction<TestObject>::New());
  }
  ~TestObjectFactory() override = default;
};


// Ensures that a TestObjectFactory is registered and unregistered, following the C++ RAII idiom (Resource Acquisition
// Is Initialization).
class TestObjectFactoryRegistration
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(TestObjectFactoryRegistration);

  TestObjectFactoryRegistration() { itk::ObjectFactoryBase::RegisterFactory(m_Factory); }
  ~TestObjectFactoryRegistration() { itk::ObjectFactoryBase::UnRegisterFactory(m_Factory); }

private:
  TestObjectFactory::Pointer m_Factory{ TestObjectFactory::New() };
};
} // namespace


// Tests that ObjectFactoryBase::CreateInstance creates an instance that has a reference count of 2.
TEST(ObjectFactoryBase, CreatedInstanceHasReferenceCountOfTwo)
{
  const TestObjectFactoryRegistration registration{};

  const auto instance = itk::ObjectFactoryBase::CreateInstance(testObjectTypeName);

  // The type specified by testObjectTypeName is registered, so the created instance may not be null.
  ASSERT_NE(instance, nullptr);

  // The main check: expect the reference count to be 2, not 1, and certainly not zero!
  EXPECT_EQ(instance->GetReferenceCount(), 2);

  // Avoid a memory leak.
  instance->UnRegister();
}
