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

#include "itkMatlabTransformIOFactory.h"
#include "itkTransformFileWriter.h"
#include "itkTransformFileReader.h"
#include "itkAffineTransform.h"
#include "itkTransformFactory.h"
#include "itkCompositeTransform.h"
#include "itksys/SystemTools.hxx"
#include "itkMatlabTransformIO.h"

#include "itkGTest.h"
#include "itkTestDriverIncludeRequiredFactories.h"


#define _STRING(s) #s
#define TOSTRING(s) std::string(_STRING(s))

namespace
{

template <typename TParametersValueType>
struct ITKIOTransformMatlabTest : public ::testing::Test
{
  void
  SetUp() override
  {
    RegisterRequiredFactories();
    itksys::SystemTools::ChangeDirectory(TOSTRING(ITK_TEST_OUTPUT_DIR));
    itk::ObjectFactoryBase::RegisterFactory(itk::MatlabTransformIOFactory::New());
  }

  using AffineTransformType = itk::AffineTransform<TParametersValueType, 4>;
  using AffineTransformTypeNotRegistered = itk::AffineTransform<TParametersValueType, 10>;
  using TransformWriterType = itk::TransformFileWriterTemplate<TParametersValueType>;
  using TransformReaderType = itk::TransformFileReaderTemplate<TParametersValueType>;

  typename AffineTransformType::Pointer
  CreateAndInitializeTransform()
  {
    auto affine = AffineTransformType::New();

    // Set its parameters
    {
      typename AffineTransformType::ParametersType p = affine->GetParameters();
      for (unsigned int i = 0; i < p.GetSize(); ++i)
      {
        p[i] = i;
      }
      affine->SetParameters(p);
    }
    {
      typename AffineTransformType::FixedParametersType p = affine->GetFixedParameters();
      for (unsigned int i = 0; i < p.GetSize(); ++i)
      {
        p[i] = i;
      }
      affine->SetFixedParameters(p);
    }
    return affine;
  }
};

using TestTypes = ::testing::Types<float, double>;
TYPED_TEST_SUITE(ITKIOTransformMatlabTest, TestTypes);

TYPED_TEST(ITKIOTransformMatlabTest, WriteAndReadValidTransform)
{
  const std::string filename =
    std::string("Transforms_") + (std::is_same_v<TypeParam, float> ? "float" : "double") + ".mat";

  auto affine = this->CreateAndInitializeTransform();
  auto writer = TestFixture::TransformWriterType::New();
  auto reader = TestFixture::TransformReaderType::New();

  writer->SetInput(affine);
  writer->SetFileName(filename);
  EXPECT_NO_THROW(writer->Update());

  reader->SetFileName(filename);
  EXPECT_NO_THROW(reader->Update());

  const auto * list = reader->GetTransformList();
  ASSERT_NE(list, nullptr);
  EXPECT_FALSE(list->empty());
}

TYPED_TEST(ITKIOTransformMatlabTest, WriteAndReadUnregisteredTransform)
{
  const std::string filename =
    std::string("TransformsBad_") + (std::is_same_v<TypeParam, float> ? "float" : "double") + ".mat";

  auto bogus = TestFixture::AffineTransformTypeNotRegistered::New();

  // Set its parameters
  {
    typename TestFixture::AffineTransformType::ParametersType p = bogus->GetParameters();
    for (unsigned int i = 0; i < p.GetSize(); ++i)
    {
      p[i] = i;
    }
    bogus->SetParameters(p);
  }
  {
    typename TestFixture::AffineTransformType::FixedParametersType p = bogus->GetFixedParameters();
    for (unsigned int i = 0; i < p.GetSize(); ++i)
    {
      p[i] = i;
    }
    bogus->SetFixedParameters(p);
  }

  auto writer = TestFixture::TransformWriterType::New();
  auto reader = TestFixture::TransformReaderType::New();

  writer->AddTransform(bogus);
  writer->SetFileName(filename);
  EXPECT_NO_THROW(writer->Update());

  reader->SetFileName(filename);
  EXPECT_THROW(reader->Update(), itk::ExceptionObject);
}

TEST(ITKIOTransformMatlab, ReadIncompleteParametersTransform)
{
  //
  // test endless loop bug in transform reader, triggered by no
  // EOL at end of file.
  // This test will exercise this reported bug:
  // https://public.kitware.com/Bug/view.php?id=7028

  std::ofstream file("IllegalTransform.txt");
  file << "#Insight Transform File V1.0" << std::endl
       << "#Transform 0" << std::endl
       << "Transform: AffineTransform_double_10_10" << std::endl
       << "Parameters: "
       << "  0 1 2 3 4 5 6 7 8 9 10 11 12"
       << " 13 14 15 16 17 18 19 20 21 22" << std::endl
       << "FixedParameters: 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18";
  file.close();

  using ReaderType = itk::TransformFileReaderTemplate<double>;
  auto reader = ReaderType::New();
  reader->SetFileName("IllegalTransform.txt");
  try
  {
    reader->Update();
    FAIL() << "Expected itk::ExceptionObject";
  }
  catch (const itk::ExceptionObject & ex)
  {
    std::cout << "Caught exception: " << ex << std::endl;
  }
}

TEST(ITKIOTransformMatlab, ReadInvalidMatFile)
{
  std::ofstream file("IllegalMat.mat");
  file << "Baz, Bar" << std::endl;
  file.close();

  using ReaderType = itk::TransformFileReaderTemplate<double>;
  auto reader = ReaderType::New();
  reader->SetFileName("IllegalMat.Mat");
  EXPECT_THROW(reader->Update(), itk::ExceptionObject);
}


} // namespace
