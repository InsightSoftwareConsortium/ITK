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

#include "itkFFTDiscreteGaussianImageFilterFactory.h"
#include "itkFFTDiscreteGaussianImageFilter.h"
#include "itkDiscreteGaussianImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"

int
itkFFTDiscreteGaussianImageFilterFactoryTest(int, char *[])
{
  const unsigned int ImageDimension = 3;
  using PixelType = float;
  using ImageType = itk::Image<PixelType, ImageDimension>;
  using BaseFilterType = itk::DiscreteGaussianImageFilter<ImageType>;
  using OverrideFilterType = typename itk::FFTDiscreteGaussianImageFilter<ImageType>;

  BaseFilterType::Pointer baseFilter;

  // Verify expected instance is returned before factory registration
  ITK_TRY_EXPECT_NO_EXCEPTION(baseFilter = BaseFilterType::New());
  if (dynamic_cast<OverrideFilterType *>(baseFilter.GetPointer()) != nullptr)
  {
    std::cout << "Object factory instantiation succeeded before a factory was registered" << std::endl;
    return EXIT_FAILURE;
  }

  // Register factory and verify instantiation override
  using FactoryType = itk::FFTDiscreteGaussianImageFilterFactory;
  FactoryType::Pointer overrideFactory = FactoryType::New();
  itk::ObjectFactoryBase::RegisterFactory(overrideFactory);

  ITK_TRY_EXPECT_NO_EXCEPTION(baseFilter = BaseFilterType::New());
  if (dynamic_cast<OverrideFilterType *>(baseFilter.GetPointer()) == nullptr)
  {
    std::cout << "Object factory instantiation failed" << std::endl;
    return EXIT_FAILURE;
  }

  // Unregister factory and verify behavior returns to default
  itk::ObjectFactoryBase::UnRegisterFactory(overrideFactory);

  ITK_TRY_EXPECT_NO_EXCEPTION(baseFilter = BaseFilterType::New());
  if (dynamic_cast<OverrideFilterType *>(baseFilter.GetPointer()) != nullptr)
  {
    std::cout << "Object factory instantiation succeeded after factory was removed" << std::endl;
    return EXIT_FAILURE;
  }

  // Register factory through static method
  itk::FFTDiscreteGaussianImageFilterFactory::RegisterOneFactory();

  ITK_TRY_EXPECT_NO_EXCEPTION(baseFilter = BaseFilterType::New());
  if (dynamic_cast<OverrideFilterType *>(baseFilter.GetPointer()) == nullptr)
  {
    std::cout << "Object factory instantiation failed" << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
