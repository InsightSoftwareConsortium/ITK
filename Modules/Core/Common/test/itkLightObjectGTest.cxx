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
#include "itkLightObject.h"
#include "itkObjectFactory.h"
#include <gtest/gtest.h>


// Tests that New() does initialize the data of a derived class, even when the derived class has a defaulted
// default-constructor *and* its data has no {} initializer. Uses itkSimpleNewMacro to define New().
TEST(LightObject, SimpleNewInitializesDataOfDerivedClass)
{
  class DerivedClass : public itk::LightObject
  {
  public:
    ITK_DISALLOW_COPY_AND_MOVE(DerivedClass);

    using Self = DerivedClass;
    using Pointer = itk::SmartPointer<Self>;

    itkSimpleNewMacro(DerivedClass);
    itkGetConstMacro(Data, int);

  protected:
    // Defaulted default-constructor, essential to this test.
    DerivedClass() = default;

    // Defaulted destructor.
    ~DerivedClass() override = default;

  private:
    int m_Data; // Without {} initializer, for the purpose of this test.
  };

  EXPECT_EQ(DerivedClass::New()->GetData(), 0);
}


// Tests that New() does initialize the data of a derived class, even when the derived class has a defaulted
// default-constructor *and* its data has no {} initializer. Uses itkFactorylessNewMacro to define New().
TEST(LightObject, FactorylessNewInitializesDataOfDerivedClass)
{
  class DerivedClass : public itk::LightObject
  {
  public:
    ITK_DISALLOW_COPY_AND_MOVE(DerivedClass);

    using Self = DerivedClass;
    using Pointer = itk::SmartPointer<Self>;

    itkFactorylessNewMacro(DerivedClass);
    itkGetConstMacro(Data, int);

  protected:
    // Defaulted default-constructor, essential to this test.
    DerivedClass() = default;

    // Defaulted destructor.
    ~DerivedClass() override = default;

  private:
    int m_Data; // Without {} initializer, for the purpose of this test.
  };

  EXPECT_EQ(DerivedClass::New()->GetData(), 0);
}
