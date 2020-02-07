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

#include "itkVectorMagnitudeImageFilter.h"
#include "itkImageRegionIterator.h"

int
itkVectorMagnitudeImageFilterTest(int, char *[])
{
  // Declare the type of the pixels
  using VectorPixelType = itk::CovariantVector<float, 3>;

  // Declare the types of the images
  using VectorImageType = itk::Image<VectorPixelType, 2>;
  using FloatImageType = itk::Image<float, 2>;

  // Define the size start index of the image
  VectorImageType::SizeType size;
  size.Fill(3);

  VectorImageType::IndexType start;
  start.Fill(0);

  VectorImageType::RegionType region(start, size);

  // Construct an image
  VectorImageType::Pointer image = VectorImageType::New();
  image->SetRegions(region);
  image->Allocate();

  // Create a default pixel
  VectorPixelType pixel;
  pixel[0] = 4;
  pixel[1] = 2;
  pixel[2] = 4;
  image->FillBuffer(pixel);

  // Declare Iterator type for the input image
  using VectorIteratorType = itk::ImageRegionIterator<VectorImageType>;

  // Create an iterator for the image
  VectorIteratorType imageIterator(image, image->GetRequestedRegion());

  // Declare the vector magnitude image filter
  using myMagnitudeFilterType = itk::VectorMagnitudeImageFilter<VectorImageType, FloatImageType>;

  // Create the filter
  myMagnitudeFilterType::Pointer magnitude = myMagnitudeFilterType::New();

  magnitude->SetInput(image);

  // Now compute the magnitude of the gradient
  try
  {
    magnitude->Update();
  }
  catch (...)
  {
    std::cerr << "Exception thrown during Update() " << std::endl;
    return EXIT_FAILURE;
  }

  // Get the filter output
  FloatImageType::Pointer outputImage = magnitude->GetOutput();

  // Declare Iterator type for the output image
  using myOutputIteratorType = itk::ImageRegionIterator<FloatImageType>;

  // Create an iterator for going through the output image
  myOutputIteratorType outputIterator(outputImage, outputImage->GetBufferedRegion());

  // Compare the result image to the known result

  outputIterator.GoToBegin();
  imageIterator.GoToBegin();
  while (!outputIterator.IsAtEnd())
  {
    // Check if the magnitude of each pixel is 6.0 (to a small tolerance)
    if (itk::Math::abs(outputIterator.Get() - 6.0) > 1e-5)
    {
      std::cerr << "Every pixel magnitude should be 6! This pixel is " << imageIterator.Get() << " and has magnitude "
                << outputIterator.Get() << std::endl;
      return EXIT_FAILURE;
    }
    ++outputIterator;
    ++imageIterator;
  }

  std::cout << std::endl << "Test PASSED !! " << std::endl;

  return EXIT_SUCCESS;
}
