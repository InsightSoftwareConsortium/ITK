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

#include "itkGTest.h"
#include "itkGTestPredicate.h"

#include "itkMetaDataDictionary.h"
#include "itkMetaDataObject.h"
#include "itkTestVerifyMetaData.h"

#include <iterator>
#include <numeric>     // For iota.
#include <type_traits> // For remove_const_t, remove_reference_t, etc.

namespace
{

itk::MetaDataDictionary
createMetaDataDictionary()
{
  itk::MetaDataDictionary metaDataDictionary;

  itk::EncapsulateMetaData<float>(metaDataDictionary, "one", static_cast<float>(1));
  itk::EncapsulateMetaData<float>(metaDataDictionary, "two", static_cast<float>(2));

  using ObjectType = itk::LightObject;
  using PointerType = typename ObjectType::Pointer;
  const PointerType obj = ObjectType::New();
  itk::EncapsulateMetaData<PointerType>(metaDataDictionary, "object", obj);

  return metaDataDictionary;
}


template <typename T>
static void
CheckMetaData(itk::MetaDataDictionary & metaDict, const std::string & key, const T & knownValue)
{
  itk::EncapsulateMetaData<T>(metaDict, key, knownValue);
  ITK_EXPECT_METADATA_VALUE(metaDict, key, knownValue);
}


static void
doExposeMetaDatas()
{
  // Simplified version of tests found in HDF5 reading/writing
  // that are broken out here to improve localization of bugs
  // found during linux-arm building
  itk::MetaDataDictionary metaDict;

  CheckMetaData<bool>(metaDict, "TestBool", false);
#if !defined(ITK_FUTURE_LEGACY_REMOVE)
  CheckMetaData<char>(metaDict, "TestChar", 'c');
#endif
  CheckMetaData<unsigned char>(metaDict, "TestUChar", 'u');
  CheckMetaData<short>(metaDict, "TestShort", 1);
  CheckMetaData<unsigned short>(metaDict, "TestUShort", 3);
  CheckMetaData<int>(metaDict, "TestInt", 5);
  CheckMetaData<unsigned int>(metaDict, "TestUInt", 7);
  CheckMetaData<long>(metaDict, "TestLong", 5);
  CheckMetaData<unsigned long>(metaDict, "TestULong", 7);
  CheckMetaData<long long>(metaDict, "TestLLong", -5);
  CheckMetaData<unsigned long long>(metaDict, "TestULLong", 7ull);
  CheckMetaData<float>(metaDict, "TestFloat", 1.23456f);
  CheckMetaData<double>(metaDict, "TestDouble", 1.23456);

#if !defined(ITK_FUTURE_LEGACY_REMOVE)
  itk::Array<char> metaDataCharArray(5);
  metaDataCharArray[0] = 'h';
  metaDataCharArray[1] = 'e';
  metaDataCharArray[2] = 'l';
  metaDataCharArray[3] = 'l';
  metaDataCharArray[4] = 'o';
  CheckMetaData<itk::Array<char>>(metaDict, "TestCharArray", metaDataCharArray);
#endif

  itk::Array<double> metaDataDoubleArray(5);
  metaDataDoubleArray[0] = 3.0;
  metaDataDoubleArray[1] = 1.0;
  metaDataDoubleArray[2] = 4.0;
  metaDataDoubleArray[3] = 5.0;
  metaDataDoubleArray[4] = 2.0;
  CheckMetaData<itk::Array<double>>(metaDict, "TestDoubleArray", metaDataDoubleArray);

  CheckMetaData<std::string>(metaDict, "StdString", "Test std::string");
}


template <typename T>
itk::MetaDataObjectBase::Pointer
createMetaDataObject(const T & invalue)
{
  typename itk::MetaDataObject<T>::Pointer temp = itk::MetaDataObject<T>::New();
  temp->SetMetaDataObjectValue(invalue);
  return temp;
}
} // namespace

TEST(MetaDataDictionary, Basic)
{
  // Isolate
  doExposeMetaDatas();

  // This test exercises and checks the non-constant interface
  itk::MetaDataDictionary dic = createMetaDataDictionary();

  EXPECT_TRUE(dic.HasKey("one"));
  EXPECT_TRUE(dic.HasKey("two"));
  EXPECT_TRUE(dic.HasKey("object"));

  EXPECT_FALSE(dic.HasKey("three"));
  EXPECT_FALSE(dic.HasKey(""));
  EXPECT_FALSE(dic.HasKey("ONE"));
  EXPECT_FALSE(dic.HasKey("SomethingElseThatDoesNotExist"));

  EXPECT_EQ(dic.GetKeys().size(), 3u);


  EXPECT_NE(dic.Get("one"), nullptr);
  EXPECT_NE(dic.Get("two"), nullptr);
  EXPECT_NE(dic.Get("object"), nullptr);
  EXPECT_THROW([[maybe_unused]] auto novalue = dic.Get("three"), itk::ExceptionObject);
  EXPECT_THROW([[maybe_unused]] auto novlaue = dic.Get(""), itk::ExceptionObject);
  EXPECT_THROW([[maybe_unused]] auto novalue = dic.Get("ONE"), itk::ExceptionObject);

  EXPECT_EQ(dic.GetKeys().size(), 3u);

  EXPECT_EQ(std::distance(dic.Begin(), dic.End()), 3u);


  auto iter = dic.Find("object");

  EXPECT_NE(iter, dic.End());
  EXPECT_EQ(iter->first, "object");

  iter = dic.Find("nothing");
  EXPECT_EQ(iter, dic.End());

  EXPECT_FALSE(dic.Erase("One"));
  EXPECT_EQ(std::distance(dic.Begin(), dic.End()), 3u);
  EXPECT_EQ(dic.GetKeys().size(), 3u);
  EXPECT_TRUE(dic.HasKey("one"));


  EXPECT_TRUE(dic.Erase("two"));
  EXPECT_EQ(std::distance(dic.Begin(), dic.End()), 2u);
  EXPECT_EQ(dic.GetKeys().size(), 2u);
  EXPECT_FALSE(dic.HasKey("two"));

  dic.Clear();
  EXPECT_EQ(std::distance(dic.Begin(), dic.End()), 0u);
  EXPECT_EQ(dic.GetKeys().size(), 0u);
  EXPECT_FALSE(dic.HasKey("one"));
  EXPECT_FALSE(dic.HasKey("two"));
  EXPECT_FALSE(dic.HasKey("object"));

  // move assignment
  dic = createMetaDataDictionary();

  EXPECT_TRUE(dic.HasKey("one"));
  EXPECT_TRUE(dic.HasKey("two"));
  EXPECT_TRUE(dic.HasKey("object"));
  EXPECT_EQ(dic.GetKeys().size(), 3u);


  EXPECT_EQ(dic["nothing"], nullptr);
  EXPECT_EQ(dic.GetKeys().size(), 4u);
  EXPECT_EQ(std::distance(dic.Begin(), dic.End()), 4u);
  EXPECT_TRUE(dic.HasKey("nothing"));
}

TEST(MetaDataDictionary, ConstBasic)
{
  // This test exercises and checks the constant interface
  const itk::MetaDataDictionary cdic = createMetaDataDictionary();

  EXPECT_TRUE(cdic.HasKey("one"));
  EXPECT_TRUE(cdic.HasKey("two"));
  EXPECT_TRUE(cdic.HasKey("object"));

  EXPECT_FALSE(cdic.HasKey("three"));
  EXPECT_FALSE(cdic.HasKey(""));
  EXPECT_FALSE(cdic.HasKey("ONE"));
  EXPECT_FALSE(cdic.HasKey("SomethingElseThatDoesNotExist"));

  EXPECT_EQ(cdic.GetKeys().size(), 3u);

  EXPECT_NE(cdic.Get("one"), nullptr);
  EXPECT_NE(cdic.Get("two"), nullptr);
  EXPECT_NE(cdic.Get("object"), nullptr);
  EXPECT_THROW([[maybe_unused]] auto novalue = cdic.Get("three"), itk::ExceptionObject);
  EXPECT_THROW([[maybe_unused]] auto novalue = cdic.Get(""), itk::ExceptionObject);
  EXPECT_THROW([[maybe_unused]] auto novalue = cdic.Get("ONE"), itk::ExceptionObject);

  EXPECT_EQ(cdic.GetKeys().size(), 3u);

  EXPECT_EQ(std::distance(cdic.Begin(), cdic.End()), 3u);


  auto iter = cdic.Find("object");

  EXPECT_NE(iter, cdic.End());
  EXPECT_EQ(iter->first, "object");

  iter = cdic.Find("nothing");
  EXPECT_EQ(iter, cdic.End());

  EXPECT_EQ(cdic["nothing"], nullptr);
  EXPECT_EQ(cdic.GetKeys().size(), 3u);
  EXPECT_EQ(std::distance(cdic.Begin(), cdic.End()), 3u);


  float f = -99;
  itk::ExposeMetaData<float>(cdic, "one", f);
  EXPECT_EQ(f, 1.0);
  itk::ExposeMetaData<float>(cdic, "two", f);
  EXPECT_EQ(f, 2.0);

  itk::LightObject::Pointer objPtr;
  itk::ExposeMetaData<itk::LightObject::Pointer>(cdic, "object", objPtr);
  EXPECT_FALSE(objPtr.IsNull());
}


TEST(MetaDataDictionary, CopyOnWrite)
{

  {
    itk::MetaDataDictionary       dic = createMetaDataDictionary();
    const itk::MetaDataDictionary dic_copy = dic;

    // The use_count is not exposed in the interface, but it is in the
    // print method.
    std::cout << "The use_count for the std::map in the dictionary should be 2." << std::endl;
    dic.Print(std::cout);

    dic["one"] = createMetaDataObject(11.0f);

    float f = -99;
    itk::ExposeMetaData<float>(dic, "one", f);
    EXPECT_EQ(f, 11.0f);
    itk::ExposeMetaData<float>(dic_copy, "one", f);
    EXPECT_EQ(f, 1.0f);
  }

  {
    itk::MetaDataDictionary       dic = createMetaDataDictionary();
    const itk::MetaDataDictionary dic_copy = dic;

    dic.Set("two", createMetaDataObject(22.0f));

    float f = -99;
    itk::ExposeMetaData<float>(dic, "two", f);
    EXPECT_EQ(f, 22.0f);
    itk::ExposeMetaData<float>(dic_copy, "two", f);
    EXPECT_EQ(f, 2.0f);
  }

  {
    itk::MetaDataDictionary       dic = createMetaDataDictionary();
    const itk::MetaDataDictionary dic_copy = dic;

    dic.Set("three", createMetaDataObject(3.0f));

    float f = -99;
    itk::ExposeMetaData<float>(dic, "three", f);
    EXPECT_EQ(f, 3.0f);
    EXPECT_FALSE(dic_copy.HasKey("three"));
  }

  {
    itk::MetaDataDictionary       dic = createMetaDataDictionary();
    const itk::MetaDataDictionary dic_copy = dic;

    dic.Erase("two");

    EXPECT_FALSE(dic.HasKey("two"));
    EXPECT_TRUE(dic_copy.HasKey("two"));
  }

  {
    itk::MetaDataDictionary       dic = createMetaDataDictionary();
    const itk::MetaDataDictionary dic_copy = dic;

    dic.Clear();

    EXPECT_FALSE(dic.HasKey("one"));
    EXPECT_FALSE(dic.HasKey("two"));
    EXPECT_FALSE(dic.HasKey("object"));
    EXPECT_EQ(dic.GetKeys().size(), 0u);

    EXPECT_TRUE(dic_copy.HasKey("one"));
    EXPECT_TRUE(dic_copy.HasKey("two"));
    EXPECT_TRUE(dic_copy.HasKey("object"));
    EXPECT_EQ(dic_copy.GetKeys().size(), 3u);

    float f = -99;
    itk::ExposeMetaData<float>(dic_copy, "one", f);
    EXPECT_EQ(f, 1.0f);
  }
}


TEST(MetaDataDictionary, Equal)
{
  const auto expectEqual = [](const itk::MetaDataDictionary & object1, const itk::MetaDataDictionary & object2) {
    // Test that equal objects can be used as arguments to GoogleTest EXPECT_EQ.
    EXPECT_EQ(object1, object2);
    EXPECT_EQ(object2, object1);

    // Test symmetry, as well as consistency between equal and unequal.
    EXPECT_TRUE(object1 == object2);
    EXPECT_TRUE(object2 == object1);
    EXPECT_FALSE(object1 != object2);
    EXPECT_FALSE(object2 != object1);
  };

  const auto expectUnequal = [](const itk::MetaDataDictionary & object1, const itk::MetaDataDictionary & object2) {
    // Test that unequal objects can be used as arguments to GoogleTest EXPECT_NE.
    EXPECT_NE(object1, object2);
    EXPECT_NE(object2, object1);

    // Test symmetry, as well as consistency between equal and unequal.
    EXPECT_TRUE(object1 != object2);
    EXPECT_TRUE(object2 != object1);
    EXPECT_FALSE(object1 == object2);
    EXPECT_FALSE(object2 == object1);
  };

  const itk::MetaDataDictionary defaultMetaDataDictionary{};

  expectEqual(defaultMetaDataDictionary, {});

  const auto createMetaDataDictionary = [](const int value) {
    itk::MetaDataDictionary metaDataDictionary;
    itk::EncapsulateMetaData(metaDataDictionary, "key", value);
    return metaDataDictionary;
  };

  const auto metaDataDictionary1 = createMetaDataDictionary(1);
  const auto metaDataDictionary2 = createMetaDataDictionary(2);

  expectEqual(metaDataDictionary1, metaDataDictionary1);
  expectEqual(metaDataDictionary2, metaDataDictionary2);
  expectEqual(metaDataDictionary1, createMetaDataDictionary(1));
  expectEqual(metaDataDictionary2, createMetaDataDictionary(2));

  expectUnequal(metaDataDictionary1, metaDataDictionary2);
  expectUnequal(metaDataDictionary1, defaultMetaDataDictionary);
  expectUnequal(metaDataDictionary2, defaultMetaDataDictionary);
}


TEST(MetaDataDictionary, SupportsCStyleArrays)
{
  itk::MetaDataDictionary dictionary;

  const auto check = [&dictionary](const auto & value) {
    using ValueType = std::remove_const_t<std::remove_reference_t<decltype(value)>>;

    const std::string key = "key";
    itk::EncapsulateMetaData(dictionary, key, value);
    const itk::MetaDataObjectBase * const base = dictionary.Get(key);
    ASSERT_NE(base, nullptr);

    const auto * const actualDataObject = dynamic_cast<const itk::MetaDataObject<ValueType> *>(base);
    ASSERT_NE(actualDataObject, nullptr);

    const ValueType & actualValue = actualDataObject->GetMetaDataObjectValue();

    // itk::EncapsulateMetaData is expected to make a _copy_ of the input value. So it should not just store a reference
    // to that value.
    EXPECT_NE(&actualValue, &value);

    const auto expectedDataObject = itk::MetaDataObject<ValueType>::New();
    ASSERT_NE(expectedDataObject, nullptr);
    expectedDataObject->SetMetaDataObjectValue(value);

    // The value from the dictionary is expected to compare equal to the original input value.
    EXPECT_EQ(*actualDataObject, *expectedDataObject);
  };

  for (const int i : { 0, 1 })
  {
    // Check a simple C-style array.
    const int simpleArray[] = { i, 1 - i };
    check(simpleArray);
  }

  // Check a two-dimensional array.
  constexpr size_t numberOfRows{ 3 };
  constexpr size_t numberOfColumns{ 4 };
  int              twoDimensionalArray[numberOfRows][numberOfColumns];

  // Just ensure that each element of the two-dimensional array has a different value.
  int * const beginOfData = &(twoDimensionalArray[0][0]);
  std::iota(beginOfData, beginOfData + numberOfRows * numberOfColumns, 1);

  check(twoDimensionalArray);
}
