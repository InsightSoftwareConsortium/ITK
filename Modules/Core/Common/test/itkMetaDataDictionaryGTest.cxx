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

#include "itkGTest.h"

#include "itkMetaDataDictionary.h"
#include "itkMetaDataObject.h"

#include <iterator>

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
  PointerType obj = ObjectType::New();
  itk::EncapsulateMetaData<PointerType>(metaDataDictionary, "object", obj);

  return metaDataDictionary;
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
  EXPECT_THROW(dic.Get("three"), itk::ExceptionObject);
  EXPECT_THROW(dic.Get(""), itk::ExceptionObject);
  EXPECT_THROW(dic.Get("ONE"), itk::ExceptionObject);

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
  EXPECT_THROW(cdic.Get("three"), itk::ExceptionObject);
  EXPECT_THROW(cdic.Get(""), itk::ExceptionObject);
  EXPECT_THROW(cdic.Get("ONE"), itk::ExceptionObject);

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
