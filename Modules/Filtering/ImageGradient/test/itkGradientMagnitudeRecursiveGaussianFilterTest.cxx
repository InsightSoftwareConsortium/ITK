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

#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"

template <typename TImage1Type, typename TImage2Type>
class ImageInformationIsEqual
{
public:
  static bool
  Check(const TImage1Type * image1, const TImage2Type * image2)
  {
    if (image1->GetSpacing() != image2->GetSpacing())
    {
      return false;
    }
    if (image1->GetOrigin() != image2->GetOrigin())
    {
      return false;
    }
    if (image1->GetDirection() != image2->GetDirection())
    {
      return false;
    }
    return true;
  }
};

int
itkGradientMagnitudeRecursiveGaussianFilterTest(int argc, char * argv[])
{
  if (argc != 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " sigma normalizeAcrossScale" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int myDimension = 3;
  using myImageType = itk::Image<float, myDimension>;
  using myIndexType = itk::Index<myDimension>;
  using mySizeType = itk::Size<myDimension>;
  using myRegionType = itk::ImageRegion<myDimension>;

  auto inputImage = myImageType::New();

  mySizeType size;
  size.Fill(8);

  myIndexType start;
  start.Fill(0);

  myRegionType region;
  region.SetIndex(start);
  region.SetSize(size);

  // initialize the image with default-valued pixels (in this case, 0.0)
  inputImage->SetRegions(region);
  inputImage->Allocate(true);

  // Set the metadata for the image
  myImageType::PointType     origin;
  myImageType::SpacingType   spacing;
  myImageType::DirectionType direction;

  origin[0] = 1.0;
  origin[1] = 2.0;
  origin[2] = 3.0;
  spacing[0] = .1;
  spacing[1] = .2;
  spacing[2] = .3;
  direction.SetIdentity();
  direction(1, 1) = -1.0;

  inputImage->SetSpacing(spacing);
  inputImage->SetOrigin(origin);
  inputImage->SetDirection(direction);

  // Declare Iterator type for the input image
  using myIteratorType = itk::ImageRegionIteratorWithIndex<myImageType>;

  size.Fill(4);
  start.Fill(2);

  // Create one iterator for an internal region
  region.SetSize(size);
  region.SetIndex(start);
  myIteratorType itb(inputImage, region);

  // Initialize the content the internal region
  while (!itb.IsAtEnd())
  {
    itb.Set(100.0);
    ++itb;
  }

  using myFilterType = itk::GradientMagnitudeRecursiveGaussianImageFilter<myImageType>;
  using myGradientImageType = myFilterType::OutputImageType;


  auto filter = myFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, GradientMagnitudeRecursiveGaussianImageFilter, InPlaceImageFilter);


  itk::SimpleFilterWatcher watcher(filter);

  auto sigma = static_cast<typename myFilterType::RealType>(std::stod(argv[1]));
  filter->SetSigma(sigma);
  ITK_TEST_SET_GET_VALUE(sigma, filter->GetSigma());

  auto normalizeAcrossScale = static_cast<bool>(std::stoi(argv[2]));
  ITK_TEST_SET_GET_BOOLEAN(filter, NormalizeAcrossScale, normalizeAcrossScale);

  filter->SetInput(inputImage);

  // Execute the filter
  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());


  // Get the Smart Pointer to the Filter Output
  // It is important to do it AFTER the filter is Updated
  // Because the object connected to the output may be changed
  // by another during GenerateData() call
  myGradientImageType::Pointer outputImage = filter->GetOutput();

#ifndef NDEBUG
  using myOutputIteratorType = itk::ImageRegionIteratorWithIndex<myGradientImageType>;

  // Create an iterator for going through the output image
  myOutputIteratorType itg(outputImage, outputImage->GetRequestedRegion());

  //  Print the content of the result image
  std::cout << " Result " << std::endl;
  itg.GoToBegin();
  while (!itg.IsAtEnd())
  {
    std::cout << itg.Get() << std::endl;
    ++itg;
  }
#endif

  if (!ImageInformationIsEqual<myImageType, myImageType>::Check(inputImage, outputImage))
  {
    std::cout << "ImageInformation mismatch!" << std::endl;
    std::cout << "inputImage Origin:  " << inputImage->GetOrigin() << std::endl;
    std::cout << "outputImage Origin: " << outputImage->GetOrigin() << std::endl;
    std::cout << "inputImage Spacing:  " << inputImage->GetSpacing() << std::endl;
    std::cout << "outputImage Spacing: " << outputImage->GetSpacing() << std::endl;
    std::cout << "inputImage Direction:  " << inputImage->GetDirection() << std::endl;
    std::cout << "outputImage Direction: " << outputImage->GetDirection() << std::endl;
    return EXIT_FAILURE;
  }

  // check that the same filter is able to run on a smaller image
  size.Fill(5);
  region.SetSize(size);

  inputImage->SetRegions(region);
  inputImage->Allocate();
  inputImage->FillBuffer(1);

  // Execute the filter
  ITK_TRY_EXPECT_NO_EXCEPTION(filter->UpdateLargestPossibleRegion());

  // All objects should be automatically destroyed at this point
  std::cout << std::endl << "Test PASSED ! " << std::endl;
  return EXIT_SUCCESS;
}
