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

#include <iostream>
#include <ctime>
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageRegionIterator.h"

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
  constexpr unsigned int N = 3;
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
  constexpr unsigned int N = 3;
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
  constexpr unsigned int N = 3;
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
  constexpr unsigned int N = 3;
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


int
itkAdaptorComparisonTest(int, char *[])
{
  using ScalarImageType = itk::Image<float, 3>;
  using VectorImageType = itk::Image<itk::Vector<float, 3>, 3>;

  // Set up some images
  itk::ImageRegion<3> region;
  itk::Size<3>        size;
  size[0] = 100;
  size[1] = 100;
  size[2] = 100;
  itk::Index<3> index;
  index[0] = 0;
  index[1] = 0;
  index[2] = 0;
  region.SetSize(size);
  region.SetIndex(index);

  auto scalar_image = ScalarImageType::New();
  auto vector_image = VectorImageType::New();

  scalar_image->SetRegions(region);
  scalar_image->AllocateInitialized();

  vector_image->SetRegions(region);
  vector_image->Allocate();

  auto initialVectorValue = itk::MakeFilled<VectorImageType::PixelType>(1.2345); // arbitrary value;
  vector_image->FillBuffer(initialVectorValue);

  // Time trials

  std::cout << "Speed of adaptor supporting iterator (for reference) \t";

  const clock_t adaptor_comp = [&]() -> auto {
    const auto start = clock();
    AdaptorSupportedIteratorSpeed(scalar_image);
    const auto stop = clock();
    return stop - start;
  }();

  std::cout << adaptor_comp << std::endl;
  const clock_t no_adaptor_comp = [=](auto scalarImage) {
    std::cout << "Speed of iterator that does not support adaptors (for reference) \t";
    const auto start = clock();
    NoAdaptorSupportIteratorSpeed(scalarImage);
    const auto stop = clock();
    return stop - start;
  }(scalar_image);
  std::cout << no_adaptor_comp << std::endl;
  {
    std::cout << "Modifying scalar image using adaptor iterator...\t";
    const auto start = clock();
    AdaptorSupportedModifyScalars(scalar_image);
    const auto stop = clock();
    std::cout << (stop - start) << "\t compensated = " << (stop - start) - adaptor_comp << std::endl;
  }
  {
    std::cout << "Modifying scalar image using non-adaptor iterator...\t";
    const auto start = clock();
    NoAdaptorSupportModifyScalars(scalar_image);
    const auto stop = clock();
    std::cout << (stop - start) << "\t compensated = " << (stop - start) - no_adaptor_comp << std::endl;
  }
  {
    std::cout << "Modifying vector image using adaptor iterator...\t";
    const auto start = clock();
    AdaptorSupportedModifyVectors(vector_image);
    const auto stop = clock();
    std::cout << (stop - start) << "\t compensated = " << (stop - start) - adaptor_comp << std::endl;
  }
  {
    std::cout << "Modifying vector image using non-adaptor iterator...\t";
    const auto start = clock();
    NoAdaptorSupportModifyVectors(vector_image);
    const auto stop = clock();
    std::cout << (stop - start) << "\t compensated = " << (stop - start) - no_adaptor_comp << std::endl;
  }
  {
    std::cout << "Modifying scalar image bypassing adaptor api using"
              << " adaptor iterator...\t";
    const auto start = clock();
    BypassAdaptorSupportModifyScalars(scalar_image);
    const auto stop = clock();
    std::cout << (stop - start) << "\t compensated = " << (stop - start) - adaptor_comp << std::endl;
  }
  {
    std::cout << "Modifying vector image bypassing adaptor api using"
              << " non-adaptor iterator...\t";
    const auto start = clock();
    BypassNoAdaptorSupportModifyVectors(vector_image);
    const auto stop = clock();
    std::cout << (stop - start) << "\t compensated = " << (stop - start) - adaptor_comp << std::endl;
  }
  {
    std::cout << "Modifying vector image bypassing adaptor api using"
              << " adaptor iterator...\t";
    const auto start = clock();
    BypassAdaptorSupportModifyVectors(vector_image);
    const auto stop = clock();
    std::cout << (stop - start) << "\t compensated = " << (stop - start) - adaptor_comp << std::endl;
  }
  return EXIT_SUCCESS;
}
