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
    // *it += 3.435f;
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
    // *it += 3.435f;
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

  // Set up some images
  auto scalar_image = ScalarImageType::New();
  auto vector_image = VectorImageType::New();

  scalar_image->SetRegions(region);
  scalar_image->AllocateInitialized();

  vector_image->SetRegions(region);
  vector_image->Allocate();

  auto initialVectorValue = itk::MakeFilled<VectorImageType::PixelType>(1.2345f); // arbitrary value;
  vector_image->FillBuffer(initialVectorValue);

  AdaptorSupportedIteratorSpeed(scalar_image);
  NoAdaptorSupportIteratorSpeed(scalar_image);
  AdaptorSupportedModifyScalars(scalar_image);
  NoAdaptorSupportModifyScalars(scalar_image);
  AdaptorSupportedModifyVectors(vector_image);
  NoAdaptorSupportModifyVectors(vector_image);
  BypassAdaptorSupportModifyScalars(scalar_image);
  BypassNoAdaptorSupportModifyVectors(vector_image);
  BypassAdaptorSupportModifyVectors(vector_image);
}
