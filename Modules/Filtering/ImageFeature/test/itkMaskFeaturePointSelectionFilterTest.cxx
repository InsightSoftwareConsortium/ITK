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

#include <ctime>
#include <iostream>
#include "itkIndex.h"
#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkImageFileWriter.h"
#include "itkMaskFeaturePointSelectionFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRGBPixel.h"
#include "itkRegionOfInterestImageFilter.h"
#include "itkTestingMacros.h"


int
itkMaskFeaturePointSelectionFilterTest(int argc, char * argv[])
{
  if (argc < 7)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv)
              << " inputImageFile outputImageFile nonConnectivity blockRadius computeStructureTensors selectFraction "
                 "[maskImage]"
              << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 3;

  using InputPixelType = unsigned char;
  using OutputPixelType = itk::RGBPixel<InputPixelType>;

  using InputImageType = itk::Image<InputPixelType, Dimension>;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;

  using PointSetPixelType = itk::Matrix<itk::SpacePrecisionType, Dimension, Dimension>;
  using PointSetType = itk::PointSet<PointSetPixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<InputImageType>;

  using FilterType = itk::MaskFeaturePointSelectionFilter<InputImageType, InputImageType, PointSetType>;

  // Set up the reader
  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  // Set up filter
  auto filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, MaskFeaturePointSelectionFilter, ImageToMeshFilter);


  filter->SetInput(reader->GetOutput());

  // Test exceptions
  unsigned int nonConnectivity = Dimension;
  filter->SetNonConnectivity(nonConnectivity);
  ITK_TRY_EXPECT_EXCEPTION(filter->Update());

  nonConnectivity = static_cast<unsigned int>(std::stoi(argv[3]));
  filter->SetNonConnectivity(nonConnectivity);
  ITK_TEST_SET_GET_VALUE(nonConnectivity, filter->GetNonConnectivity());

  auto blockRadiusValue = static_cast<typename FilterType::SizeType::SizeValueType>(std::stod(argv[4]));
  typename FilterType::SizeType blockRadius;
  blockRadius.Fill(blockRadiusValue);
  filter->SetBlockRadius(blockRadius);
  ITK_TEST_SET_GET_VALUE(blockRadius, filter->GetBlockRadius());

  auto computeStructureTensors = static_cast<bool>(std::stoi(argv[5]));
  ITK_TEST_SET_GET_BOOLEAN(filter, ComputeStructureTensors, computeStructureTensors);

  auto selectFraction = std::stod(argv[6]);
  filter->SetSelectFraction(selectFraction);
  ITK_TEST_SET_GET_VALUE(selectFraction, filter->GetSelectFraction());

  // Use the whole input image as a mask if none is provided
  using MaskPixelType = unsigned char;
  using MaskImageType = itk::Image<MaskPixelType, Dimension>;
  MaskImageType::Pointer maskImage;

  if (argc >= 8)
  {
    maskImage = itk::ReadImage<InputImageType>(argv[7]);
    filter->SetMaskImage(maskImage);
    ITK_TEST_SET_GET_VALUE(maskImage, filter->GetMaskImage());
  }
  filter->SetMaskImage(maskImage);
  ITK_TEST_SET_GET_VALUE(maskImage, filter->GetMaskImage());


  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());


  // Set up the writer
  using WriterType = itk::ImageFileWriter<OutputImageType>;
  auto writer = WriterType::New();

  using InputIteratorType = itk::ImageRegionConstIterator<InputImageType>;
  InputIteratorType inputIterator(reader->GetOutput(), reader->GetOutput()->GetBufferedRegion());
  using OutputIteratorType = itk::ImageRegionIterator<OutputImageType>;

  auto outputImage = OutputImageType::New();
  outputImage->CopyInformation(reader->GetOutput());
  outputImage->SetBufferedRegion(reader->GetOutput()->GetBufferedRegion());
  outputImage->SetRequestedRegion(reader->GetOutput()->GetRequestedRegion());
  outputImage->Allocate();

  OutputIteratorType outputIterator(outputImage, outputImage->GetBufferedRegion());
  inputIterator.GoToBegin();
  outputIterator.GoToBegin();

  // Copy input image to output image
  while (!outputIterator.IsAtEnd())
  {
    OutputPixelType rgbPixel;
    rgbPixel.SetRed(inputIterator.Get());
    rgbPixel.SetGreen(inputIterator.Get());
    rgbPixel.SetBlue(inputIterator.Get());
    outputIterator.Set(rgbPixel);
    ++outputIterator;
    ++inputIterator;
  }

  // Highlight the feature points identified in the output image
  using PointIteratorType = PointSetType::PointsContainer::ConstIterator;

  PointIteratorType pointItr = filter->GetOutput()->GetPoints()->Begin();
  PointIteratorType pointEnd = filter->GetOutput()->GetPoints()->End();

  OutputImageType::IndexType index;

  // Highlight the feature point in red color
  OutputPixelType colorValue;
  colorValue.SetRed(255u);
  colorValue.SetGreen(0u);
  colorValue.SetBlue(0u);

  while (pointItr != pointEnd)
  {
    if (outputImage->TransformPhysicalPointToIndex(pointItr.Value(), index))
    {
      outputImage->SetPixel(index, colorValue);
    }
    pointItr++;
  }
  writer->SetFileName(argv[2]);
  writer->SetInput(outputImage);
  writer->Update();

  return EXIT_SUCCESS;
}
