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

#include "itkConstNeighborhoodIterator.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionIterator.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkSobelOperator.h"
#include "itkTestingMacros.h"


int
itkSobelOperatorImageConvolutionTest(int argc, char * argv[])
{
  if (argc != 4)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputFileName direction outputFileName"
              << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;

  using PixelType = unsigned char;
  using ImageType = itk::Image<PixelType, Dimension>;

  using SobelOperatorType = itk::SobelOperator<PixelType, Dimension>;

  using NeighborhoodIteratorType = itk::ConstNeighborhoodIterator<ImageType>;
  using IteratorType = itk::ImageRegionIterator<ImageType>;


  const auto inputImage = itk::ReadImage<ImageType>(argv[1]);


  SobelOperatorType sobelOperator;

  auto direction = std::stoul(argv[2]);
  sobelOperator.SetDirection(direction);

  itk::Size<Dimension> radius;
  radius.Fill(1);
  sobelOperator.CreateToRadius(radius);

  NeighborhoodIteratorType it(radius, inputImage, inputImage->GetRequestedRegion());

  auto outputImage = ImageType::New();
  outputImage->SetRegions(inputImage->GetRequestedRegion());
  outputImage->Allocate(true);

  IteratorType out(outputImage, inputImage->GetRequestedRegion());

  itk::NeighborhoodInnerProduct<ImageType> innerProduct;

  for (it.GoToBegin(), out.GoToBegin(); !it.IsAtEnd(); ++it, ++out)
  {
    out.Set(innerProduct(it, sobelOperator));
  }


  itk::WriteImage(outputImage, argv[3]);


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
