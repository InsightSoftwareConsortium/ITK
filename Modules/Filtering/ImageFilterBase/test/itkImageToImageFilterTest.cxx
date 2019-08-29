/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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

#include "itkImageToImageFilter.h"

namespace itk
{

template <typename TInputImage, typename TOutputImage>
class ImageToImageFilterTestHelper : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  using Self = ImageToImageFilterTestHelper;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkNewMacro(Self);
};

} // namespace itk

int
itkImageToImageFilterTest(int, char *[])
{

  constexpr unsigned int ImageDimension = 3;
  using InputPixelType = unsigned char;
  using OutputPixelType = signed short;

  using InputImageType = itk::Image<InputPixelType, ImageDimension>;
  using OutputImageType = itk::Image<OutputPixelType, ImageDimension>;

  using FilterType = itk::ImageToImageFilterTestHelper<InputImageType, OutputImageType>;

  InputImageType::Pointer inputImage1 = InputImageType::New();
  InputImageType::Pointer inputImage2 = InputImageType::New();
  InputImageType::Pointer inputImage3 = InputImageType::New();
  InputImageType::Pointer inputImage4 = InputImageType::New();

  FilterType::Pointer filter = FilterType::New();

  filter->Print(std::cout);
  std::cout << "Name of Class = " << filter->GetNameOfClass() << std::endl;
  std::cout << "Name of Superclass = " << filter->Superclass::GetNameOfClass() << std::endl;

  filter->SetInput(inputImage1);
  if (filter->GetInput() != inputImage1)
  {
    std::cerr << "Error in Set/GetInput()" << std::endl;
    return EXIT_FAILURE;
  }

  filter->SetInput(inputImage2);
  if (filter->GetInput() != inputImage2)
  {
    std::cerr << "Error in Set/GetInput()" << std::endl;
    return EXIT_FAILURE;
  }

  filter->SetInput(0, inputImage1);
  if (filter->GetInput(0) != inputImage1)
  {
    std::cerr << "Error in Set/GetInput(n)" << std::endl;
    return EXIT_FAILURE;
  }


  filter->PushBackInput(inputImage2);
  filter->PushBackInput(inputImage3);
  filter->PushFrontInput(inputImage4);

  if (filter->GetInput(0) != inputImage4)
  {
    std::cerr << "Error in PushFrontInput" << std::endl;
    return EXIT_FAILURE;
  }

  if (filter->GetInput(1) != inputImage1)
  {
    std::cerr << "Error in PushFrontInput" << std::endl;
    return EXIT_FAILURE;
  }

  if (filter->GetInput(2) != inputImage2)
  {
    std::cerr << "Error in PushBackInput" << std::endl;
    return EXIT_FAILURE;
  }

  if (filter->GetInput(3) != inputImage3)
  {
    std::cerr << "Error in PushBackInput" << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
