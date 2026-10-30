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

#include "itkImageToListSampleAdaptor.h"
#include "itkRandomImageSource.h"
#include "itkComposeImageFilter.h"
#include "itkGTest.h"

namespace
{
template <typename TImage>
void
itkImageToListSampleAdaptorTestTemplate()
{
  using ImageType = TImage;
  const unsigned int Dimension = ImageType::ImageDimension;

  using FloatImageType = itk::Image<float, Dimension>;

  // Now generate a random image
  using SourceType = itk::RandomImageSource<FloatImageType>;
  auto source = SourceType::New();

  itk::SizeValueType       size[Dimension] = { 17, 8, 20 };
  const itk::SizeValueType totalSize = size[0] * size[1] * size[2];

  source->SetSize(size);
  constexpr float minValue{ -100.0 };
  constexpr float maxValue{ 1000.0 };

  source->SetMin(static_cast<typename FloatImageType::PixelType>(minValue));
  source->SetMax(static_cast<typename FloatImageType::PixelType>(maxValue));

  using ComposeFilterType = itk::ComposeImageFilter<FloatImageType, ImageType>;
  auto composeFilter = ComposeFilterType::New();
  composeFilter->SetInput(source->GetOutput());
  composeFilter->Update();

  // creates a sample
  using ImageToListSampleAdaptorType = itk::Statistics::ImageToListSampleAdaptor<ImageType>;

  auto sample = ImageToListSampleAdaptorType::New();

  // Test if the methods throw exceptions if invoked before setting the image
  // purposely call Size() method prematurely in order to trigger an exception.
  EXPECT_THROW(sample->Size(), itk::ExceptionObject);
  // purposely call GetTotalFrequency() method prematurely in order to trigger an exception.
  EXPECT_THROW(sample->GetTotalFrequency(), itk::ExceptionObject);

  EXPECT_THROW(sample->GetMeasurementVector(0), itk::ExceptionObject);

  EXPECT_THROW(sample->GetImage(), itk::ExceptionObject);

  // purposely call GetFrequency() method prematurely in order to trigger an exception.
  EXPECT_THROW(sample->GetFrequency(0), itk::ExceptionObject);


  sample->SetImage(composeFilter->GetOutput());

  // tests begin

  // check size
  EXPECT_EQ(totalSize, sample->Size()) << "Size() is not returning the correct size";

  // check frequency
  EXPECT_EQ(totalSize, sample->GetTotalFrequency()) << "GetTotalFrequency() is not returning the correct frequency";


  sample->Print(std::cout);

  typename ImageType::IndexType index;
  typename ImageType::PixelType pixel;

  typename ImageToListSampleAdaptorType::InstanceIdentifier iid;

  for (unsigned int i = 0; i < size[2]; ++i)
  {
    for (unsigned int j = 0; j < size[1]; ++j)
    {
      for (unsigned int k = 0; k < size[0]; ++k)
      {
        index[0] = k;
        index[1] = j;
        index[2] = i;

        pixel = sample->GetImage()->GetPixel(index);
        iid = sample->GetImage()->ComputeOffset(index);
        EXPECT_EQ(sample->GetMeasurementVector(iid)[0], pixel[0]) << "Error in pixel value accessed using the adaptor";
      }
    }
  }

  // iterator tests
  //
  std::cerr << "Iterators..." << std::endl;
  {
    // forward iterator
    using IteratorType = typename ImageToListSampleAdaptorType::Iterator;

    IteratorType s_iter = sample->Begin();

    // copy constructor
    const IteratorType bs_iter(s_iter);
    EXPECT_EQ(bs_iter, s_iter) << "Iterator::Copy Constructor failed";

    // assignment operator
    IteratorType assignment_iter(bs_iter);
    assignment_iter = s_iter;
    EXPECT_EQ(assignment_iter, s_iter) << "Iterator::assignment operator failed";

    typename ImageToListSampleAdaptorType::InstanceIdentifier iid2 = 0;
    while (s_iter != sample->End())
    {
      EXPECT_EQ(sample->GetMeasurementVector(iid2), s_iter.GetMeasurementVector())
        << "Iterator::GetMeasurementVector (forward) failed";
      EXPECT_EQ(iid2, s_iter.GetInstanceIdentifier()) << "Iterator::GetInstanceIdentifier (forward) failed";
      EXPECT_EQ(s_iter.GetFrequency(), 1u) << "Iterator::GetFrequency (forward) failed";
      EXPECT_EQ(sample->GetFrequency(iid2), 1u) << "GetFrequency (forward) failed";
      ++iid2;
      ++s_iter;
    }

    EXPECT_EQ(s_iter, sample->End()) << "Iterator::End (forward) failed";
  }

  // Test the iterators
  std::cerr << "Const Iterators..." << std::endl;
  {
    // forward iterator
    using ConstIteratorType = typename ImageToListSampleAdaptorType::ConstIterator;

    ConstIteratorType s_iter = sample->Begin();

    // copy constructor
    const ConstIteratorType bs_iter(s_iter);
    EXPECT_EQ(bs_iter, s_iter) << "Iterator::Copy Constructor (from const) failed";

    // assignment operator
    ConstIteratorType assignment_iter(bs_iter);
    assignment_iter = s_iter;
    EXPECT_EQ(assignment_iter, s_iter) << "Const Iterator::operator= () failed";

    // copy from non-const iterator
    const typename ImageToListSampleAdaptorType::Iterator nonconst_iter = sample->Begin();
    typename ImageToListSampleAdaptorType::ConstIterator  s2_iter(nonconst_iter);
    EXPECT_EQ(s2_iter, s_iter) << "Iterator::Copy Constructor (from non-const) failed";
    // assignment from non-const iterator
    s2_iter = nonconst_iter;
    EXPECT_EQ(s2_iter, s_iter) << "Iterator::assignment (from non-const) failed";

    typename ImageToListSampleAdaptorType::InstanceIdentifier iid3 = 0;
    while (s_iter != sample->End())
    {
      EXPECT_EQ(sample->GetMeasurementVector(iid3), s_iter.GetMeasurementVector())
        << "Iterator::GetMeasurementVector (forward) failed";
      EXPECT_EQ(iid3, s_iter.GetInstanceIdentifier()) << "Iterator::GetInstanceIdentifier (forward) failed";
      EXPECT_EQ(s_iter.GetFrequency(), 1u) << "Iterator::GetFrequency (forward) failed";
      ++iid3;
      ++s_iter;
    }

    EXPECT_EQ(s_iter, sample->End()) << "Iterator::End (forward) failed";
  }
}
} // namespace

TEST(ImageToListSampleAdaptor, ConvertedLegacyTest)
{
  constexpr unsigned int Dimension{ 3 };

  using VectorImageType = itk::VectorImage<float, Dimension>;
  itkImageToListSampleAdaptorTestTemplate<VectorImageType>();

  using FixedArrayType = itk::FixedArray<float, 1>;
  using FixedArrayImageType = itk::Image<FixedArrayType, Dimension>;
  itkImageToListSampleAdaptorTestTemplate<FixedArrayImageType>();
}
