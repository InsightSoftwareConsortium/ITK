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
#include "itkGridImageSource.h"
#include "itkImageFileWriter.h"
#include "itkBSplineKernelFunction.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"


int
itkGridImageSourceTest(int argc, char * argv[])
{
  if (argc != 12)
  {
    std::cout << "Usage: " << itkNameOfTestExecutableMacro(argv) << " outputImage"
              << " imageSize"
              << " sigma"
              << " variableSigma"
              << " gridSpacing"
              << " variableGridSpacing"
              << " gridOffset"
              << " gridAllDimensions"
              << " toggleLastGridDimension"
              << " useBSplineKernel"
              << " bSplineOrder" << std::endl;
    return EXIT_FAILURE;
  }


  constexpr unsigned int ImageDimension = 3;
  using PixelType = float;

  using ImageType = itk::Image<PixelType, ImageDimension>;

  // Instantiate the filter
  using GridSourceType = itk::GridImageSource<ImageType>;
  auto gridImage = GridSourceType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(gridImage, GridImageSource, GenerateImageSource);


  // Specify image parameters
  auto                size = static_cast<ImageType::SizeValueType>(std::stod(argv[2]));
  ImageType::SizeType imageSize;
  imageSize.Fill(size);

  ImageType::PointType origin;
  origin.Fill(0.0);

  ImageType::SpacingType imageSpacing;
  imageSpacing.Fill(1.0);

  ImageType::DirectionType direction;
  direction.SetIdentity();

  gridImage->SetSize(imageSize);
  gridImage->SetSpacing(imageSpacing);
  gridImage->SetOrigin(origin);
  gridImage->SetDirection(direction);


  // Specify grid parameters
  double scale = 255.0;
  gridImage->SetScale(scale);
  ITK_TEST_SET_GET_VALUE(scale, gridImage->GetScale());


  auto                      sigmaValue = static_cast<GridSourceType::ArrayType::ValueType>(std::stod(argv[3]));
  GridSourceType::ArrayType sigma;
  sigma.Fill(sigmaValue);
  auto variableSigma = static_cast<bool>(std::stoi(argv[4]));

  if (variableSigma)
  {
    if (sigma.Size() > 2)
    {
      sigma[1] = sigma[0] + 4.0;
      sigma[2] = sigma[0] + 5.0;
    }
  }
  gridImage->SetSigma(sigma);
  ITK_TEST_SET_GET_VALUE(sigma, gridImage->GetSigma());


  auto                      spacing = static_cast<GridSourceType::ArrayType::ValueType>(std::stod(argv[5]));
  GridSourceType::ArrayType gridSpacing;
  gridSpacing.Fill(spacing);


  auto variableGridSpacing = static_cast<bool>(std::stoi(argv[6]));
  if (variableGridSpacing)
  {
    for (unsigned int i = 0; i < gridSpacing.Size(); ++i)
    {
      gridSpacing[i] = gridSpacing[0] / 2.0;
    }
  }
  gridImage->SetGridSpacing(gridSpacing);
  ITK_TEST_SET_GET_VALUE(gridSpacing, gridImage->GetGridSpacing());


  auto                      offset = static_cast<GridSourceType::ArrayType::ValueType>(std::stod(argv[7]));
  GridSourceType::ArrayType gridOffset;
  gridOffset.Fill(offset);
  gridImage->SetGridOffset(gridOffset);
  ITK_TEST_SET_GET_VALUE(gridOffset, gridImage->GetGridOffset());


  auto                          gridAllDimensions = static_cast<bool>(std::stoi(argv[8]));
  GridSourceType::BoolArrayType whichDimension;
  whichDimension.Fill(gridAllDimensions);

  bool toggleLastGridDimension = std::stod(argv[9]);
  if (toggleLastGridDimension)
  {
    whichDimension[ImageDimension - 1] = !gridAllDimensions;
  }
  gridImage->SetWhichDimensions(whichDimension);
  ITK_TEST_SET_GET_VALUE(whichDimension, gridImage->GetWhichDimensions());


  auto useBSplineKernel = static_cast<bool>(std::stoi(argv[10]));
  if (useBSplineKernel)
  {
    unsigned int bSplineOrder = std::stoi(argv[11]);
    // Specify B-Spline function
    if (bSplineOrder == 3)
    {
      using KernelType = itk::BSplineKernelFunction<3>;
      auto kernel = KernelType::New();
      gridImage->SetKernelFunction(kernel);
    }
    else
    {
      using KernelType = itk::BSplineKernelFunction<0>;
      auto kernel = KernelType::New();
      gridImage->SetKernelFunction(kernel);
    }
  }


  itk::SimpleFilterWatcher watcher(gridImage, "GridImageSource");

  ITK_TRY_EXPECT_NO_EXCEPTION(gridImage->Update());

  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();
  writer->SetFileName(argv[1]);
  writer->SetInput(gridImage->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
