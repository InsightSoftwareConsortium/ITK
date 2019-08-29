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

#include "itkShowDistanceMap.h"
#include "itkSignedDanielssonDistanceMapImageFilter.h"
#include "itkStdStreamStateSave.h"

int
itkSignedDanielssonDistanceMapImageFilterTest11(int, char *[])
{
  // Save the format stream variables for std::cout
  // They will be restored when coutState goes out of scope
  itk::StdStreamStateSave coutState(std::cout);

  std::cout << "Test ITK Liza Signed Danielsson Distance Map" << std::endl << std::endl;
  std::cout << "Compute the distance map of a 5x5 image" << std::endl;
  std::cout << "with a point at (4,4) (value=1)" << std::endl << std::endl;

  using myImageType2D1 = itk::Image<unsigned char, 2>;
  using myImageType2D2 = itk::Image<float, 2>;

  /* Allocate the 2D image */
  myImageType2D1::SizeType   size2D = { { 5, 5 } };
  myImageType2D1::IndexType  index2D = { { 0, 0 } };
  myImageType2D1::RegionType region2D;
  region2D.SetSize(size2D);
  region2D.SetIndex(index2D);

  myImageType2D1::Pointer inputImage2D = myImageType2D1::New();
  inputImage2D->SetLargestPossibleRegion(region2D);
  inputImage2D->SetBufferedRegion(region2D);
  inputImage2D->SetRequestedRegion(region2D);
  inputImage2D->Allocate(true);

  /* Set pixel (4,4) with the value 1
   * The SignedDanielsson Distance is performed for each pixel with a value > 0
   * The ClosestPoints computation is based on the value of the pixel.
   */

  index2D[0] = 3;
  index2D[1] = 3;
  inputImage2D->SetPixel(index2D, 1);

  /* Create SignedDanielssonDistance Map filter */
  using myFilterType2D = itk::SignedDanielssonDistanceMapImageFilter<myImageType2D1, myImageType2D2>;

  myFilterType2D::Pointer filter2D = myFilterType2D::New();

  filter2D->SetInput(inputImage2D);

  myImageType2D2::Pointer outputDistance2D = filter2D->GetOutput();
  filter2D->Update();

  ShowDistanceMap(outputDistance2D);

  /* Test Squared Distance functionality */
  // Get the value of pixel at location [0,0] before distance is squared
  myImageType2D2::IndexType index;
  index[0] = 0;
  index[1] = 0;
  std::cout << "here" << std::endl;
  const double distance1 = outputDistance2D->GetPixel(index);
  std::cout << "distance1: " << distance1 << std::endl;

  filter2D->SquaredDistanceOn();
  // filter2D->SquaredDistanceOff();

  if (filter2D->GetSquaredDistance() != true)
  {
    std::cerr << "filter2D->GetSquaredDistance() != true" << std::endl;
    return EXIT_FAILURE;
  }

  //  filter2D->SetSquaredDistance( true );
  filter2D->Update();

  const double distance2 = outputDistance2D->GetPixel(index);
  std::cout << "distance2: " << distance2 << std::endl;
  const myImageType2D2::PixelType epsilon = 1e-5;
  if (std::fabs(distance2 - distance1 * distance1) > epsilon)
  {
    std::cerr << "Error in use of the SetSquaredDistance() method" << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Squared Distance Map " << std::endl;
  ShowDistanceMap(outputDistance2D);

  /* Test for images with anisotropic spacing */
  myImageType2D1::SpacingType anisotropicSpacing;

  anisotropicSpacing[0] = 1.0;
  anisotropicSpacing[1] = 5.0;

  inputImage2D->SetSpacing(anisotropicSpacing);

  inputImage2D->FillBuffer(0);

  index2D[0] = 2;
  index2D[1] = 2;
  inputImage2D->SetPixel(index2D, 1);

  filter2D->SetInput(inputImage2D);
  filter2D->UseImageSpacingOn();
  filter2D->SquaredDistanceOff();
  filter2D->InsideIsPositiveOn();
  // filter2D->SetBackgroundValue(0);

  if (filter2D->GetUseImageSpacing() != true)
  {
    std::cerr << "filter2D->GetUseImageSpacing() != true" << std::endl;
    return EXIT_FAILURE;
  }

  if (filter2D->GetInsideIsPositive() != true)
  {
    std::cerr << "filter2D->GetInsideIsPositive() != true" << std::endl;
    return EXIT_FAILURE;
  }

  if (filter2D->GetSquaredDistance() == true)
  {
    std::cerr << "filter2D->GetSquaredDistance() == true & it should not" << std::endl;
    return EXIT_FAILURE;
  }
  filter2D->SetUseImageSpacing(true);
  myImageType2D2::Pointer outputDistance2D2 = filter2D->GetOutput();
  filter2D->Update();

  std::cout << "Use ImageSpacing Distance Map with squared distance turned off" << std::endl;
  ShowDistanceMap(outputDistance2D2);

  return EXIT_SUCCESS;
}
