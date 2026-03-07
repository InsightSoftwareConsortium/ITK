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

#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageRegionIterator.h"
#include "itkGTest.h"

#include <iostream>

namespace
{

void
AdaptorSupportedIteratorSpeed(itk::Image<float, 3> * img)
{
  itk::ImageRegionIteratorWithIndex<itk::Image<float, 3>> it(img, img->GetRequestedRegion());
  while (!it.IsAtEnd())
  {
    ++it;
  }
}

void
NoAdaptorSupportIteratorSpeed(itk::Image<float, 3> * img)
{
  itk::ImageRegionIterator<itk::Image<float, 3>> it(img, img->GetRequestedRegion());
  while (!it.IsAtEnd())
  {
    ++it;
  }
}

void
AdaptorSupportedModifyScalars(itk::Image<float, 3> * img)
{
  itk::ImageRegionIteratorWithIndex<itk::Image<float, 3>> it(img, img->GetRequestedRegion());
  while (!it.IsAtEnd())
  {
    it.Set(it.Get() + 3.435f);
    ++it;
  }
}

void
NoAdaptorSupportModifyScalars(itk::Image<float, 3> * img)
{
  itk::ImageRegionIterator<itk::Image<float, 3>> it(img, img->GetRequestedRegion());
  while (!it.IsAtEnd())
  {
    it.Set(it.Get() + 3.435f);
    ++it;
  }
}

void
BypassAdaptorSupportModifyScalars(itk::Image<float, 3> * img)
{
  itk::ImageRegionIteratorWithIndex<itk::Image<float, 3>> it(img, img->GetRequestedRegion());
  while (!it.IsAtEnd())
  {
    it.Value() += 3.435f;
    ++it;
  }
}

void
AdaptorSupportedModifyVectors(itk::Image<itk::Vector<float, 3>, 3> * img)
{
  constexpr unsigned int N{ 3 };
  using VectorType = itk::Vector<float, N>;

  itk::ImageRegionIteratorWithIndex<itk::Image<VectorType, 3>> it(img, img->GetRequestedRegion());
  while (!it.IsAtEnd())
  {
    VectorType temp_vector = it.Get();
    for (unsigned int i = 0; i < N; ++i)
    {
      temp_vector[i] += 3.435f;
    }
    it.Set(temp_vector);
    ++it;
  }
}

void
NoAdaptorSupportModifyVectors(itk::Image<itk::Vector<float, 3>, 3> * img)
{
  constexpr unsigned int N{ 3 };
  using VectorType = itk::Vector<float, N>;

  itk::ImageRegionIterator<itk::Image<VectorType, 3>> it(img, img->GetRequestedRegion());
  while (!it.IsAtEnd())
  {
    VectorType temp_vector = it.Get();
    for (unsigned int i = 0; i < N; ++i)
    {
      temp_vector[i] += 3.435f;
    }
    it.Set(temp_vector);
    ++it;
  }
}

void
BypassAdaptorSupportModifyVectors(itk::Image<itk::Vector<float, 3>, 3> * img)
{
  constexpr unsigned int N{ 3 };
  using VectorType = itk::Vector<float, N>;

  itk::ImageRegionIteratorWithIndex<itk::Image<VectorType, 3>> it(img, img->GetRequestedRegion());
  while (!it.IsAtEnd())
  {
    for (unsigned int i = 0; i < N; ++i)
    {
      (it.Value())[i] += 3.435f;
    }
    ++it;
  }
}

void
BypassNoAdaptorSupportModifyVectors(itk::Image<itk::Vector<float, 3>, 3> * img)
{
  constexpr unsigned int N{ 3 };
  using VectorType = itk::Vector<float, N>;

  itk::ImageRegionIterator<itk::Image<VectorType, 3>> it(img, img->GetRequestedRegion());
  while (!it.IsAtEnd())
  {
    for (unsigned int i = 0; i < N; ++i)
    {
      (it.Value())[i] += 3.435f;
    }
    ++it;
  }
}

} // namespace


TEST(AdaptorComparison, IteratorOperationsComplete)
{
  using ScalarImageType = itk::Image<float, 3>;
  using VectorImageType = itk::Image<itk::Vector<float, 3>, 3>;

  constexpr itk::Size<3> size{ { 100, 100, 100 } };
  itk::ImageRegion<3>    region{ size };

  auto scalar_image = ScalarImageType::New();
  auto vector_image = VectorImageType::New();

  scalar_image->SetRegions(region);
  scalar_image->AllocateInitialized();

  vector_image->SetRegions(region);
  vector_image->Allocate();

  auto initialVectorValue = itk::MakeFilled<VectorImageType::PixelType>(1.2345f);
  vector_image->FillBuffer(initialVectorValue);

  std::cout << "Speed of adaptor supporting iterator: ";
  EXPECT_NO_THROW(AdaptorSupportedIteratorSpeed(scalar_image));

  std::cout << "Speed of non-adaptor iterator: ";
  EXPECT_NO_THROW(NoAdaptorSupportIteratorSpeed(scalar_image));

  std::cout << "Modifying scalar image using adaptor iterator: ";
  EXPECT_NO_THROW(AdaptorSupportedModifyScalars(scalar_image));

  std::cout << "Modifying scalar image using non-adaptor iterator: ";
  EXPECT_NO_THROW(NoAdaptorSupportModifyScalars(scalar_image));

  std::cout << "Modifying vector image using adaptor iterator: ";
  EXPECT_NO_THROW(AdaptorSupportedModifyVectors(vector_image));

  std::cout << "Modifying vector image using non-adaptor iterator: ";
  EXPECT_NO_THROW(NoAdaptorSupportModifyVectors(vector_image));

  std::cout << "Modifying scalar image bypassing adaptor via adaptor iterator: ";
  EXPECT_NO_THROW(BypassAdaptorSupportModifyScalars(scalar_image));

  std::cout << "Modifying vector image bypassing adaptor via non-adaptor iterator: ";
  EXPECT_NO_THROW(BypassNoAdaptorSupportModifyVectors(vector_image));

  std::cout << "Modifying vector image bypassing adaptor via adaptor iterator: ";
  EXPECT_NO_THROW(BypassAdaptorSupportModifyVectors(vector_image));
}
