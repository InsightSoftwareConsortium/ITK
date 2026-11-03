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
// The example tests the class itk::Statistics::JointDomainImageToListSampleAdaptor.


#include "itkJointDomainImageToListSampleAdaptor.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkGTest.h"

TEST(JointDomainImageToListSampleAdaptor, ConvertedLegacyTest)
{
  constexpr unsigned int MeasurementVectorSize{ 8 };
  using MeasurementComponentType = unsigned long;
  using PixelType = itk::FixedArray<MeasurementComponentType, MeasurementVectorSize>;

  constexpr unsigned int ImageDimension{ 3 };
  using ImageType = itk::Image<PixelType, ImageDimension>;

  auto image = ImageType::New();

  ImageType::IndexType start{};
  auto                 size = itk::MakeFilled<ImageType::SizeType>(10);


  const unsigned long         totalSize = size[0] * size[1] * size[2];
  const ImageType::RegionType region(start, size);
  image->SetRegions(region);
  image->Allocate();

  using RegionIteratorType = itk::ImageRegionIteratorWithIndex<ImageType>;

  RegionIteratorType it(image, region);

  it.GoToBegin();

  while (!it.IsAtEnd())
  {
    PixelType value;
    for (unsigned int i = 0; i < MeasurementVectorSize; ++i)
    {
      value[i] = i + it.GetIndex()[0];
    }
    it.Set(value);
    ++it;
  }

  // define an adaptor type
  using JointDomainImageToListSampleAdaptorType = itk::Statistics::JointDomainImageToListSampleAdaptor<ImageType>;
  auto adaptor = JointDomainImageToListSampleAdaptorType::New();
  // Test if the methods throw exceptions if invoked before setting the image
  // calling Size() method prematurely in order to trigger an exception.
  EXPECT_THROW(adaptor->Size(), itk::ExceptionObject);
  // calling GetTotalFrequency() method prematurely in order to trigger an exception.
  EXPECT_THROW(adaptor->GetTotalFrequency(), itk::ExceptionObject);

  // calling GetMeasurementVector() method prematurely in order to trigger an exception.
  EXPECT_THROW(adaptor->GetMeasurementVector(0), itk::ExceptionObject);

  // calling GetImage() method prematurely in order to trigger an exception.
  EXPECT_THROW(adaptor->GetImage(), itk::ExceptionObject);

  // calling GetFrequency() method prematurely in order to trigger an exception.
  EXPECT_THROW(adaptor->GetFrequency(0), itk::ExceptionObject);


  adaptor->SetImage(image);

  // check size
  EXPECT_EQ(totalSize, adaptor->Size()) << "Size() is not returning the correct size";

  // check frequency
  EXPECT_EQ(totalSize, adaptor->GetTotalFrequency()) << "GetTotalFrequency() is not returning the correct frequency";


  adaptor->Print(std::cout);

  ImageType::IndexType index;

  using MeasurementVectorType = JointDomainImageToListSampleAdaptorType::MeasurementVectorType;

  MeasurementVectorType measurementVector;

  for (unsigned int i = 0; i < size[2]; ++i)
  {
    for (unsigned int j = 0; j < size[1]; ++j)
    {
      for (unsigned int k = 0; k < size[0]; ++k)
      {
        index[0] = k;
        index[1] = j;
        index[2] = i;

        JointDomainImageToListSampleAdaptorType::PointType tempPoint;
        adaptor->GetImage()->TransformIndexToPhysicalPoint(index, tempPoint);

        ImageType::PixelType pixel = adaptor->GetImage()->GetPixel(index);

        for (unsigned int d = 0; d < ImageDimension; ++d)
        {
          measurementVector[d] = tempPoint[d];
        }
        for (unsigned int c = 0; c < MeasurementVectorSize; ++c)
        {
          measurementVector[ImageDimension + c] = pixel[c];
        }

        JointDomainImageToListSampleAdaptorType::InstanceIdentifier iid = adaptor->GetImage()->ComputeOffset(index);

        MeasurementVectorType measurementVectorFromAdaptor = adaptor->GetMeasurementVector(iid);
        for (unsigned int m = 0; m < MeasurementVectorType::Length; ++m)
        {
          EXPECT_NEAR(measurementVectorFromAdaptor[m], measurementVector[m], 1.0E-6)
            << "Error in measurement vector value accessed using the adaptor";
        }
      }
    }
  }

  // Test the Use_PixelContainer boolean

  adaptor->SetUsePixelContainer(false);
  EXPECT_FALSE(adaptor->GetUsePixelContainer()) << "Error in Set/Get UsePixelContainer methods";

  adaptor->UsePixelContainerOn();
  EXPECT_TRUE(adaptor->GetUsePixelContainer()) << "Error in Set/Get UsePixelContainerOn method";

  // Get measurement vector from the pixel container and using ComputeIndex and compare
  // the result
  JointDomainImageToListSampleAdaptorType::MeasurementVectorType v1 = adaptor->GetMeasurementVector(4);
  adaptor->UsePixelContainerOff();
  JointDomainImageToListSampleAdaptorType::MeasurementVectorType v2 = adaptor->GetMeasurementVector(4);

  constexpr double epsilon{ 1e-3 };

  for (unsigned int m = 0; m < MeasurementVectorType::Length; ++m)
  {
    EXPECT_NEAR(v1[m], v2[m], epsilon)
      << "Accessing the measurement vector using the two method produced different result";
  }

  // Exercise GetFrequency method on the first and last element
  EXPECT_EQ(adaptor->GetFrequency(0), 1u) << "GetFrequency is not returning the expected value";
  EXPECT_EQ(adaptor->GetFrequency(adaptor->Size()), 1u) << "GetFrequency is not returning the expected value";

  // iterator tests
  //
  std::cerr << "Iterators..." << std::endl;
  {
    // forward iterator
    using IteratorType = JointDomainImageToListSampleAdaptorType::Iterator;

    IteratorType s_iter = adaptor->Begin();

    // copy constructor
    const IteratorType bs_iter(s_iter);
    EXPECT_EQ(bs_iter, s_iter) << "Iterator::Copy Constructor failed";

    // assignment operator
    IteratorType assignment_iter(bs_iter);
    assignment_iter = s_iter;
    EXPECT_EQ(assignment_iter, s_iter) << "Iterator::assignment operator failed";

    JointDomainImageToListSampleAdaptorType::InstanceIdentifier iid2 = 0;
    while (s_iter != adaptor->End())
    {
      EXPECT_EQ(adaptor->GetMeasurementVector(iid2), s_iter.GetMeasurementVector())
        << "Iterator::GetMeasurementVector (forward) failed";
      EXPECT_EQ(iid2, s_iter.GetInstanceIdentifier()) << "Iterator::GetInstanceIdentifier (forward) failed";
      EXPECT_EQ(s_iter.GetFrequency(), 1u) << "Iterator::GetFrequency (forward) failed";
      EXPECT_EQ(adaptor->GetFrequency(iid2), 1u) << "GetFrequency (forward) failed";
      ++iid2;
      ++s_iter;
    }

    EXPECT_EQ(s_iter, adaptor->End()) << "Iterator::End (forward) failed";
  }


  // Test the iterators
  std::cerr << "Const Iterators..." << std::endl;
  {
    // forward iterator
    using ConstIteratorType = JointDomainImageToListSampleAdaptorType::ConstIterator;

    ConstIteratorType s_iter = adaptor->Begin();

    // copy constructor
    const ConstIteratorType bs_iter(s_iter);
    EXPECT_EQ(bs_iter, s_iter) << "Iterator::Copy Constructor (from const) failed";

    // assignment operator
    ConstIteratorType assignment_iter(bs_iter);
    assignment_iter = s_iter;
    EXPECT_EQ(assignment_iter, s_iter) << "Const Iterator::operator= () failed";

    // copy from non-const iterator
    const JointDomainImageToListSampleAdaptorType::Iterator nonconst_iter = adaptor->Begin();
    JointDomainImageToListSampleAdaptorType::ConstIterator  s2_iter(nonconst_iter);
    EXPECT_EQ(s2_iter, s_iter) << "Iterator::Copy Constructor (from non-const) failed";
    // assignment from non-const iterator
    s2_iter = nonconst_iter;
    EXPECT_EQ(s2_iter, s_iter) << "Iterator::assignment (from non-const) failed";

    JointDomainImageToListSampleAdaptorType::InstanceIdentifier iid3 = 0;
    while (s_iter != adaptor->End())
    {
      EXPECT_EQ(adaptor->GetMeasurementVector(iid3), s_iter.GetMeasurementVector())
        << "Iterator::GetMeasurementVector (forward) failed";
      EXPECT_EQ(iid3, s_iter.GetInstanceIdentifier()) << "Iterator::GetInstanceIdentifier (forward) failed";
      EXPECT_EQ(s_iter.GetFrequency(), 1u) << "Iterator::GetFrequency (forward) failed";
      ++iid3;
      ++s_iter;
    }

    EXPECT_EQ(s_iter, adaptor->End()) << "Iterator::End (forward) failed";
  }
}
