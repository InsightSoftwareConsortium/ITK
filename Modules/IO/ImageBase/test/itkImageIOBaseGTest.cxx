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
#include "itkImageIOBase.h"
#include <gtest/gtest.h>

namespace
{
class DummyImageIO : public itk::ImageIOBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(DummyImageIO);

  using Self = DummyImageIO;
  using Superclass = ImageIOBase;
  using Pointer = itk::SmartPointer<Self>;

  itkNewMacro(Self);
  itkOverrideGetNameOfClassMacro(DummyImageIO);

protected:
  DummyImageIO() = default;
  ~DummyImageIO() override = default;

private:
  bool
  CanReadFile(const char *) override
  {
    itkExceptionMacro("Unimplemented");
  }

  void
  ReadImageInformation() override
  {
    itkExceptionMacro("Unimplemented");
  }

  void
  Read(void *) override
  {
    itkExceptionMacro("Unimplemented");
  }

  bool
  CanWriteFile(const char *) override
  {
    itkExceptionMacro("Unimplemented");
  }

  void
  WriteImageInformation() override
  {
    itkExceptionMacro("Unimplemented");
  }

  void
  Write(const void *) override
  {
    itkExceptionMacro("Unimplemented");
  }
};


template <typename T>
void
Test_GetComponentTypeInfo()
{
  const auto imageIO = DummyImageIO::New();
  imageIO->SetComponentType(itk::ImageIOBase::MapPixelType<T>::CType);

  // Expect the returned type info to correspond with the component type that was set.
  EXPECT_EQ(imageIO->GetComponentTypeInfo(), typeid(T));
}

template <typename... T>
void
Test_GetComponentTypeInfo_for_multiple_types()
{
  (Test_GetComponentTypeInfo<T>(), ...);
}

} // namespace

// Test that GetComponentTypeInfo() returns the type info of the currently selected component type.
TEST(ImageIOBase, GetComponentTypeInfo)
{
  Test_GetComponentTypeInfo_for_multiple_types<char,
                                               unsigned char,
                                               short,
                                               unsigned short,
                                               int,
                                               unsigned int,
                                               long,
                                               unsigned long,
                                               long long,
                                               unsigned long long,
                                               float,
                                               double>();
}
