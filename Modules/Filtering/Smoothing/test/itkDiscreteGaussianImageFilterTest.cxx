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

#include <iostream>
#include "itkDiscreteGaussianImageFilter.h"
#include "itkNullImageToImageFilterDriver.hxx"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"
#include "itkConstantBoundaryCondition.h"

int
itkDiscreteGaussianImageFilterTest(int, char *[])
{
  try
  {
    constexpr unsigned int Dimension = 3;
    using ImageType = itk::Image<float, Dimension>;

    // Set up filter
    using FilterType = itk::DiscreteGaussianImageFilter<ImageType, ImageType>;
    using ArrayType = FilterType::ArrayType;

    FilterType::Pointer      filter = FilterType::New();
    itk::SimpleFilterWatcher watcher(filter);

    // Test other set/get functions
    ArrayType array;
    array[0] = 0.05;
    array[1] = 0.06;
    array[2] = 0.07;
    filter->SetMaximumError(array);

    array.Fill(0.04);
    filter->SetMaximumError(array.GetDataPointer());

    // Set the value ofthe standard deviation of the Gaussian used for smoothing
    FilterType::SigmaArrayType::ValueType sigmaValue = 1.0;
    FilterType::SigmaArrayType            sigma;
    sigma.Fill(sigmaValue);

    filter->SetSigma(sigmaValue);
    ITK_TEST_SET_GET_VALUE(sigmaValue, filter->GetSigma());

    filter->SetSigmaArray(sigma);
    ITK_TEST_SET_GET_VALUE(sigma, filter->GetSigmaArray());

    // Set the boundary condition used by the filter
    itk::ConstantBoundaryCondition<ImageType> constantBoundaryCondition;
    filter->SetInputBoundaryCondition(&constantBoundaryCondition);
    ITK_TEST_SET_GET_VALUE(&constantBoundaryCondition, filter->GetInputBoundaryCondition());

    filter->SetRealBoundaryCondition(&constantBoundaryCondition);
    ITK_TEST_SET_GET_VALUE(&constantBoundaryCondition, filter->GetRealBoundaryCondition());

    // set some parameters
    filter->SetVariance(1.0);
    filter->SetMaximumError(.01);
    filter->SetMaximumKernelWidth(32);
    filter->SetFilterDimensionality(Dimension);
    filter->SetUseImageSpacing(true);

    // Test some functions
    ArrayType varReturned = filter->GetVariance();
    for (unsigned int i = 0; i < Dimension; ++i)
    {
      if (varReturned[i] != 1.0)
      {
        std::cout << "GetVariance()[" << i << "] failed. Expected: " << 1.0 << " but got: " << varReturned[i]
                  << std::endl;
        return EXIT_FAILURE;
      }
    }
    ArrayType maxErrorReturned = filter->GetMaximumError();
    for (unsigned int i = 0; i < Dimension; ++i)
    {
      if (itk::Math::NotAlmostEquals(maxErrorReturned[i], 0.01))
      {
        std::cout << "GetMaximumError()[" << i << "] failed. Expected: " << 0.01 << " but got: " << maxErrorReturned[i]
                  << std::endl;
        return EXIT_FAILURE;
      }
    }
    if (filter->GetMaximumKernelWidth() != 32)
    {
      std::cout << "GetMaximumKernelWidth failed. Expected: " << 32 << " but got: " << filter->GetMaximumKernelWidth()
                << std::endl;
      return EXIT_FAILURE;
    }
    if (filter->GetFilterDimensionality() != Dimension)
    {
      std::cout << "GetFilterDimensionality failed. Expected: " << Dimension
                << " but got: " << filter->GetFilterDimensionality() << std::endl;
      return EXIT_FAILURE;
    }
    if (filter->GetUseImageSpacing() != true)
    {
      std::cout << "GetUseImageSpacing failed. Expected: " << true << " but got: " << filter->GetUseImageSpacing()
                << std::endl;
      return EXIT_FAILURE;
    }

    // Run Test
    itk::Size<3> sz;
    sz[0] = 100; // std::stoi(argv[1]);
    sz[1] = 100; // std::stoi(argv[2]);
    sz[2] = 40;

    itk::NullImageToImageFilterDriver<ImageType, ImageType> test1;
    test1.SetImageSize(sz);
    test1.SetFilter(filter);
    test1.Execute();
  }
  catch (const itk::ExceptionObject & err)
  {
    (&err)->Print(std::cerr);
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
