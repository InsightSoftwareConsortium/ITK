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

// Do not change NULL, null, Null in this file
// This file intentionally contains usage of legacy NULL
#include "itkGTest.h"

#include "itkSmartPointer.h"
#include "itkObject.h"

namespace
{

class Derived1 : public itk::Object
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(Derived1);

  /** Standard class type aliases. */
  using Self = Derived1;
  using Superclass = Object;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;


  /** Run-time type information (and related methods). */
  itkTypeMacro(Derived1, Object);

  itkNewMacro(Derived1);

  void
  Register() const override
  {
    ++m_RegisterCount;
    Superclass::Register();
  }
  unsigned int
  GetRegisterCount() const
  {
    return m_RegisterCount;
  }

protected:
  Derived1() = default;

  ~Derived1() override = default;

  mutable unsigned int m_RegisterCount{ 0 };
};


class Derived2 : public itk::Object
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(Derived2);

  /** Standard class type aliases. */
  using Self = Derived2;
  using Superclass = Object;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;


  /** Run-time type information (and related methods). */
  itkTypeMacro(Derived2, Object);

  itkNewMacro(Derived2);

protected:
  Derived2() = default;
  ~Derived2() override = default;
};

} // namespace

TEST(SmartPointer, EmptyAndNull)
{
  using ObjectPointer = itk::SmartPointer<itk::Object>;
  using ConstObjectPointer = itk::SmartPointer<const itk::Object>;

  ObjectPointer ptr;


  EXPECT_EQ(ptr.GetPointer(), nullptr);
  EXPECT_TRUE(ptr.IsNull());
  EXPECT_FALSE(ptr.IsNotNull());
  EXPECT_FALSE(bool(ptr));

  ConstObjectPointer cptr;

  EXPECT_EQ(cptr.GetPointer(), nullptr);
  EXPECT_TRUE(cptr.IsNull());
  EXPECT_FALSE(cptr.IsNotNull());
  EXPECT_FALSE(bool(cptr));


  ptr = ObjectPointer(nullptr);
  EXPECT_TRUE(ptr.IsNull());

  cptr = ObjectPointer(nullptr);
  EXPECT_TRUE(cptr.IsNull());

  ptr = itk::Object::New();
  EXPECT_TRUE(ptr.IsNotNull());
  EXPECT_TRUE(bool(ptr));

  cptr = itk::Object::New();
  EXPECT_TRUE(cptr.IsNotNull());
  EXPECT_TRUE(bool(cptr));

  ptr = nullptr;
  EXPECT_TRUE(ptr.IsNull());

  cptr = nullptr;
  EXPECT_TRUE(cptr.IsNull());

  ptr = NULL; // Do not change NULL, null, Null in this file. This file intentionally contains usage of legacy NULL
  EXPECT_TRUE(ptr.IsNull());

  cptr = NULL; // Do not change NULL, null, Null in this file. This file intentionally contains usage of legacy NULL
  EXPECT_TRUE(cptr.IsNull());


  EXPECT_TRUE(cptr == nullptr);
  EXPECT_TRUE(nullptr == cptr);

  // This does not work with VS14 2015
  //  EXPECT_TRUE( ptr == 0 );
  //  EXPECT_TRUE( 0 == ptr );
}


TEST(SmartPointer, Converting)
{

  using BasePointer = itk::SmartPointer<itk::Object>;
  using ConstBasePointer = itk::SmartPointer<const itk::Object>;

  using Derived1Pointer = itk::SmartPointer<Derived1>;
  using ConstDerived1Pointer = itk::SmartPointer<const Derived1>;

  Derived1Pointer d1ptr = Derived1::New();
  EXPECT_TRUE(d1ptr.IsNotNull());
  EXPECT_FALSE(d1ptr.IsNull());
  EXPECT_TRUE(bool(d1ptr));
  EXPECT_FALSE(d1ptr == nullptr);
  EXPECT_TRUE(d1ptr != nullptr);

  ConstDerived1Pointer cd1ptr(d1ptr);
  EXPECT_TRUE(cd1ptr.GetPointer() == d1ptr.GetPointer());
  EXPECT_TRUE(cd1ptr == d1ptr);

  BasePointer      ptr(d1ptr);
  ConstBasePointer cptr1(d1ptr);
  ConstBasePointer cptr2(cd1ptr);

  ptr = d1ptr;


  const itk::Object * rcptr1 = ptr;
  EXPECT_TRUE(rcptr1 == ptr);

  rcptr1 = cd1ptr;
  EXPECT_TRUE(rcptr1 == ptr.GetPointer());

  const Derived1 * rcd1ptr = d1ptr;
  EXPECT_TRUE(rcd1ptr == d1ptr);

  rcd1ptr = cd1ptr;
  EXPECT_TRUE(rcd1ptr == cd1ptr);

  // is_convertible<From,To>
  static_assert(std::is_convertible<Derived1Pointer, BasePointer>::value, "conversion check");
  static_assert(std::is_convertible<Derived1Pointer, ConstBasePointer>::value, "conversion check");

  static_assert(!std::is_convertible<ConstDerived1Pointer, Derived1Pointer>::value, "conversion check");

  static_assert(!std::is_convertible<Derived1Pointer, Derived2::Pointer>::value, "conversion check");
  static_assert(!std::is_convertible<Derived1Pointer, Derived2::ConstPointer>::value, "conversion check");
}


TEST(SmartPointer, ConvertingRegisterCount)
{

  using Derived1Pointer = itk::SmartPointer<Derived1>;
  using ConstDerived1Pointer = itk::SmartPointer<const Derived1>;

  using BasePointer = itk::SmartPointer<itk::Object>;

  // Copy constructor to const
  {
    Derived1Pointer d1ptr = Derived1::New();
    EXPECT_EQ(1, d1ptr->GetRegisterCount());

    ConstDerived1Pointer cd1ptr = d1ptr;
    EXPECT_EQ(2, d1ptr->GetRegisterCount());
    EXPECT_EQ(2, cd1ptr->GetRegisterCount());
  }

  // Copy constructor to base
  {
    Derived1Pointer d1ptr = Derived1::New();
    EXPECT_EQ(1, d1ptr->GetRegisterCount());

    BasePointer bptr = d1ptr;
    EXPECT_EQ(2, d1ptr->GetRegisterCount());
    EXPECT_EQ(2, static_cast<const Derived1 *>(bptr.GetPointer())->GetRegisterCount());
  }

  // Assignment operator
  {
    Derived1Pointer d1ptr = Derived1::New();
    EXPECT_EQ(1, d1ptr->GetRegisterCount());

    Derived1Pointer d1ptr2;

    d1ptr2 = d1ptr;
    EXPECT_EQ(2, d1ptr->GetRegisterCount());
    EXPECT_EQ(2, d1ptr2->GetRegisterCount());
  }

  // Assignment to const pointer
  {
    Derived1Pointer d1ptr = Derived1::New();
    EXPECT_EQ(1, d1ptr->GetRegisterCount());

    ConstDerived1Pointer cd1ptr;

    cd1ptr = d1ptr;
    EXPECT_EQ(2, d1ptr->GetRegisterCount());
    EXPECT_EQ(2, cd1ptr->GetRegisterCount());
  }

  // Assignment to base pointer
  {
    Derived1Pointer d1ptr = Derived1::New();
    EXPECT_EQ(1, d1ptr->GetRegisterCount());

    BasePointer bptr;

    bptr = d1ptr;
    EXPECT_EQ(2, d1ptr->GetRegisterCount());
    EXPECT_EQ(2, static_cast<const Derived1 *>(bptr.GetPointer())->GetRegisterCount());
  }

  // Assignment to raw pointer
  {
    Derived1Pointer d1ptr = Derived1::New();
    EXPECT_EQ(1, d1ptr->GetRegisterCount());

    Derived1 * rptr;

    rptr = d1ptr;
    EXPECT_EQ(1, d1ptr->GetRegisterCount());
    EXPECT_TRUE(rptr != nullptr);
  }

  // Assignment to raw const pointer
  {
    Derived1Pointer d1ptr = Derived1::New();
    EXPECT_EQ(1, d1ptr->GetRegisterCount());

    const Derived1 * rptr;

    rptr = d1ptr;
    EXPECT_EQ(1, d1ptr->GetRegisterCount());
    EXPECT_TRUE(rptr != nullptr);
  }

  // Move constructor
  {
    Derived1Pointer d1ptr = Derived1::New();
    EXPECT_EQ(1, d1ptr->GetRegisterCount());

    Derived1Pointer d1ptr2(std::move(d1ptr));
    EXPECT_EQ(1, d1ptr2->GetRegisterCount());
    EXPECT_TRUE(d1ptr.IsNull());
  }

  // Move constructor to const
  {
    Derived1Pointer d1ptr = Derived1::New();
    EXPECT_EQ(1, d1ptr->GetRegisterCount());

    ConstDerived1Pointer cd1ptr(std::move(d1ptr));
    EXPECT_EQ(1, cd1ptr->GetRegisterCount());
    EXPECT_TRUE(d1ptr.IsNull());
  }

  // Move constructor to base
  {
    Derived1Pointer d1ptr = Derived1::New();
    EXPECT_EQ(1, d1ptr->GetRegisterCount());

    BasePointer bptr(std::move(d1ptr));
    EXPECT_EQ(1, static_cast<const Derived1 *>(bptr.GetPointer())->GetRegisterCount());
    EXPECT_TRUE(d1ptr.IsNull());
  }

  // Move assignment
  {
    Derived1Pointer d1ptr = Derived1::New();
    EXPECT_EQ(1, d1ptr->GetRegisterCount());

    Derived1Pointer d1ptr2;
    d1ptr2 = std::move(d1ptr);
    EXPECT_EQ(1, d1ptr2->GetRegisterCount());
    EXPECT_TRUE(d1ptr.IsNull());
  }

  // Move assignment to const
  {
    Derived1Pointer d1ptr = Derived1::New();
    EXPECT_EQ(1, d1ptr->GetRegisterCount());

    ConstDerived1Pointer cd1ptr;
    cd1ptr = std::move(d1ptr);

    EXPECT_EQ(1, cd1ptr->GetRegisterCount());
    EXPECT_TRUE(d1ptr.IsNull());
  }

  // Move assignment to base
  {
    Derived1Pointer d1ptr = Derived1::New();
    EXPECT_EQ(1, d1ptr->GetRegisterCount());

    BasePointer bptr;

    bptr = std::move(d1ptr);
    EXPECT_EQ(1, static_cast<const Derived1 *>(bptr.GetPointer())->GetRegisterCount());
    EXPECT_TRUE(d1ptr.IsNull());
  }
}
