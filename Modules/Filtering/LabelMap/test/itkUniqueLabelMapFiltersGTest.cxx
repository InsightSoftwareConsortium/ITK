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

#include "itkImage.h"
#include "itkLabelImageToStatisticsLabelMapFilter.h"
#include "itkImageFileWriter.h"
#include <algorithm>


#include "itkLabelImageToLabelMapFilter.h"
#include "itkObjectByObjectLabelMapFilter.h"
#include "itkShapeLabelObjectAccessors.h"
#include "itkFlatStructuringElement.h"
#include "itkBinaryDilateImageFilter.h"
#include "itkLabelUniqueLabelMapFilter.h"
#include "itkTestingHashImageFilter.h"
#include "itksys/SystemTools.hxx"
#include "itkTestDriverIncludeRequiredFactories.h"


namespace
{

class UniqueLabelMapFixture : public ::testing::Test
{
public:
  UniqueLabelMapFixture() = default;
  ~UniqueLabelMapFixture() override = default;

protected:
  void
  SetUp() override
  {
    RegisterRequiredFactories();
  }
  void
  TearDown() override
  {}

  std::string
  GetTestName() const
  {
    return ::testing::UnitTest::GetInstance()->current_test_info()->name();
  }

  template <unsigned int D, typename TPixelType = unsigned short>
  struct FixtureUtilities
  {
    static const unsigned int Dimension = D;

    using LabelPixelType = TPixelType;
    using LabelImageType = itk::Image<LabelPixelType, Dimension>;
    using IndexType = typename LabelImageType::IndexType;

    using LabelObjectType = itk::StatisticsLabelObject<LabelPixelType, Dimension>;

    using LabelMapType = itk::LabelMap<itk::LabelObject<LabelPixelType, Dimension>>;


    static typename LabelImageType::Pointer
    CreateLabelImage(const std::vector<IndexType> & indices)
    {
      const size_t size = 25;
      auto         image = LabelImageType::New();

      typename LabelImageType::SizeType imageSize;
      imageSize.Fill(size);
      image->SetRegions(typename LabelImageType::RegionType(imageSize));
      image->Allocate();
      image->FillBuffer(0);

      for (LabelPixelType id = 0; id < indices.size(); ++id)
      {
        image->SetPixel(indices[id], id + 1);
      }
      return image;
    }

    static typename LabelMapType::Pointer
    LabelMapFromLabelImage(const LabelImageType * image, unsigned int dilateRadius = 0)
    {
      using ToLabelMapType = itk::LabelImageToLabelMapFilter<LabelImageType>;
      auto toLabelMap = ToLabelMapType::New();
      toLabelMap->SetInput(image);

      if (dilateRadius == 0)
      {
        toLabelMap->Update();
        return toLabelMap->GetOutput();
      }

      using KernelType = itk::FlatStructuringElement<Dimension>;
      using DilateType = itk::BinaryDilateImageFilter<LabelImageType, LabelImageType, KernelType>;
      auto                          dilate = DilateType::New();
      typename KernelType::SizeType rad;
      rad.Fill(dilateRadius);
      dilate->SetKernel(KernelType::Ball(rad));


      using OIType = itk::ObjectByObjectLabelMapFilter<LabelMapType, LabelMapType, DilateType>;
      auto oi = OIType::New();
      oi->SetInput(toLabelMap->GetOutput());
      oi->SetFilter(dilate);
      oi->SetPadSize(rad);

      oi->Update();

      return oi->GetOutput();
    }
  };

  template <typename TLabelMap>
  static typename itk::Image<typename TLabelMap::LabelObjectType::LabelType, TLabelMap::ImageDimension>::Pointer
  LabelMapToLabelImage(TLabelMap * labelMap)
  {
    using ImageType = itk::Image<typename TLabelMap::LabelObjectType::LabelType, TLabelMap::ImageDimension>;
    using L2IType = itk::LabelMapToLabelImageFilter<TLabelMap, ImageType>;
    auto l2i = L2IType::New();
    l2i->SetInput(labelMap);
    l2i->Update();
    return l2i->GetOutput();
  }


  template <typename TLabelMap>
  void
  CheckLabelMapOverlap(TLabelMap * labelMap)
  {
    for (auto & labelObject : labelMap->GetLabelObjects())
    {
      // Manually check each label object against all other label objects, to ensure that no two label objects share an
      // index.
      for (itk::SizeValueType lineNumber = 0; lineNumber < labelObject->GetNumberOfLines(); ++lineNumber)
      {
        auto line = labelObject->GetLine(lineNumber);
        auto idx = line.GetIndex();
        ASSERT_LE(line.GetLength(), labelObject->Size());
        for (itk::SizeValueType lengthIndex = 0; lengthIndex < line.GetLength(); ++lengthIndex)
        {
          for (auto & checkObject : labelMap->GetLabelObjects())
          {
            if (checkObject != labelObject)
            {
              EXPECT_FALSE(checkObject->HasIndex(idx))
                << "Label: " << int(labelObject->GetLabel()) << " and " << int(checkObject->GetLabel()) << " has index "
                << idx << std::endl;
            }
          }
          ++idx[0];
        }
      }
    }
  }

  template <typename TImageType>
  static std::string
  MD5Hash(const TImageType * image)
  {

    using HashFilter = itk::Testing::HashImageFilter<TImageType>;
    auto hasher = HashFilter::New();
    hasher->SetInput(image);
    hasher->InPlaceOff();
    hasher->Update();
    return hasher->GetHash();
  }
};
} // namespace


TEST_F(UniqueLabelMapFixture, EmptyImage)
{
  const std::vector<typename FixtureUtilities<2>::IndexType> indices = {};
  auto                                                       image = FixtureUtilities<2>::CreateLabelImage(indices);
  auto labelMap = FixtureUtilities<2>::LabelMapFromLabelImage(image.GetPointer(), 15);

  auto filter = itk::LabelUniqueLabelMapFilter<typename decltype(labelMap)::ObjectType>::New();
  filter->SetInput(labelMap);

  CheckLabelMapOverlap(filter->GetOutput());
  filter->Update();

  auto out = LabelMapToLabelImage(filter->GetOutput());

  // check the hash of out, should be all zeros
  EXPECT_EQ(MD5Hash(out.GetPointer()), "393017b9101a884b66d64849d99a7d05");
}


TEST_F(UniqueLabelMapFixture, OneLabel)
{
  const std::vector<typename FixtureUtilities<2>::IndexType> indices = { { 10, 10 } };
  auto                                                       image = FixtureUtilities<2>::CreateLabelImage(indices);
  auto labelMap = FixtureUtilities<2>::LabelMapFromLabelImage(image.GetPointer(), 0);

  auto filter = itk::LabelUniqueLabelMapFilter<typename decltype(labelMap)::ObjectType>::New();
  filter->SetInput(labelMap);
  filter->Update();

  CheckLabelMapOverlap(filter->GetOutput());

  auto out = LabelMapToLabelImage(filter->GetOutput());

  EXPECT_EQ(MD5Hash(out.GetPointer()), MD5Hash(image.GetPointer()));
  EXPECT_EQ(MD5Hash(out.GetPointer()), "9c8ee8f2fe887fd6d2393d6416df3fb6");
}


TEST_F(UniqueLabelMapFixture, OnesLabel)
{
  const std::vector<typename FixtureUtilities<2>::IndexType> indices = { { 0, 0 }, { 1, 0 }, { 2, 0 }, { 3, 0 },
                                                                         { 0, 4 }, { 2, 4 }, { 0, 5 } };
  auto                                                       image = FixtureUtilities<2>::CreateLabelImage(indices);
  auto labelMap = FixtureUtilities<2>::LabelMapFromLabelImage(image.GetPointer(), 0);

  auto filter = itk::LabelUniqueLabelMapFilter<typename decltype(labelMap)::ObjectType>::New();
  filter->SetInput(labelMap);
  filter->Update();

  CheckLabelMapOverlap(filter->GetOutput());

  auto out = LabelMapToLabelImage(filter->GetOutput());

  EXPECT_EQ(MD5Hash(out.GetPointer()), MD5Hash(image.GetPointer()));
  EXPECT_EQ(MD5Hash(out.GetPointer()), "220048b56395d98a8f20a5b1733bdde6");
}


TEST_F(UniqueLabelMapFixture, Dilate1)
{
  const std::vector<typename FixtureUtilities<2>::IndexType> indices = { { 0, 0 }, { 1, 0 }, { 2, 0 }, { 3, 0 },
                                                                         { 0, 4 }, { 2, 4 }, { 0, 10 } };
  auto                                                       image = FixtureUtilities<2>::CreateLabelImage(indices);
  auto labelMap = FixtureUtilities<2>::LabelMapFromLabelImage(image.GetPointer(), 1);

  auto filter = itk::LabelUniqueLabelMapFilter<typename decltype(labelMap)::ObjectType>::New();
  filter->SetInput(labelMap);
  filter->InPlaceOff();
  filter->ReverseOrderingOff();
  filter->Update();

  CheckLabelMapOverlap(filter->GetOutput());

  auto out = LabelMapToLabelImage(filter->GetOutput());

  EXPECT_EQ(MD5Hash(out.GetPointer()), "8bfc8570ee203fa68be18fe055cf389d");
  itk::WriteImage(out.GetPointer(), GetTestName() + "_off.png");

  filter->ReverseOrderingOn();
  filter->Update();

  CheckLabelMapOverlap(filter->GetOutput());

  out = LabelMapToLabelImage(filter->GetOutput());

  EXPECT_EQ(MD5Hash(out.GetPointer()), "bef76c79168969548f1a8090d46b5f7e");
  itk::WriteImage(out.GetPointer(), GetTestName() + "_on.png");
}


TEST_F(UniqueLabelMapFixture, Dilate2)
{
  const std::vector<typename FixtureUtilities<2>::IndexType> indices = { { 0, 0 }, { 1, 0 }, { 2, 0 }, { 3, 0 },
                                                                         { 0, 4 }, { 2, 4 }, { 0, 5 } };
  auto                                                       image = FixtureUtilities<2>::CreateLabelImage(indices);
  auto labelMap = FixtureUtilities<2>::LabelMapFromLabelImage(image.GetPointer(), 2);

  auto filter = itk::LabelUniqueLabelMapFilter<typename decltype(labelMap)::ObjectType>::New();
  filter->SetInput(labelMap);
  filter->InPlaceOff();
  filter->ReverseOrderingOff();
  filter->Update();

  CheckLabelMapOverlap(filter->GetOutput());

  auto out = LabelMapToLabelImage(filter->GetOutput());

  EXPECT_EQ(MD5Hash(out.GetPointer()), "02ec62c193413dd82cb0feaee1ee0b12");
  itk::WriteImage(out.GetPointer(), GetTestName() + "_off.png");


  filter->ReverseOrderingOn();
  filter->Update();

  CheckLabelMapOverlap(filter->GetOutput());

  out = LabelMapToLabelImage(filter->GetOutput());

  EXPECT_EQ(MD5Hash(out.GetPointer()), "1c6901199b01ac095511792f447578a3");

  itk::WriteImage(out.GetPointer(), GetTestName() + "_on.png");
}
