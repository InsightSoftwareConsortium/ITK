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

#include "itkShowDistanceMap.h"
#include "itkDanielssonDistanceMapImageFilter.h"
#include "itkStdStreamStateSave.h"
#include "itkTestingMacros.h"

int
itkDanielssonDistanceMapImageFilterTest(int, char *[])
{
  // Save the format stream variables for std::cout
  // They will be restored when coutState goes out of scope
  const itk::StdStreamStateSave coutState(std::cout);

  std::cout << "Test ITK Danielsson Distance Map" << std::endl << std::endl;
  std::cout << "Compute the distance map of a 9x9 image" << std::endl;
  std::cout << "with a point at (4,4) (value=1)" << std::endl << std::endl;
  std::cout << "with a point at (1,6) (value=2)" << std::endl << std::endl;


  using myImageType2D1 = itk::Image<unsigned char, 2>;
  using myImageType2D2 = itk::Image<float, 2>;

  /* Allocate the 2D image */
  constexpr myImageType2D1::SizeType size2D{ 9, 9 };
  myImageType2D1::IndexType          index2D = { { 0, 0 } };
  const myImageType2D1::RegionType   region2D{ index2D, size2D };

  auto inputImage2D = myImageType2D1::New();
  inputImage2D->SetRegions(region2D);
  inputImage2D->AllocateInitialized();

  // Set pixel (4,4) with the value 1
  // and pixel (1,6) with the value 2
  // The Danielsson Distance is performed for each pixel with a value > 0
  // The ClosestPoints computation is based on the value of the pixel.

  index2D[0] = 4;
  index2D[1] = 4;
  inputImage2D->SetPixel(index2D, 1);
  index2D[0] = 1;
  index2D[1] = 6;
  inputImage2D->SetPixel(index2D, 2);

  // Create Danielsson Distance Map filter
  using myFilterType2D = itk::DanielssonDistanceMapImageFilter<myImageType2D1, myImageType2D2>;

  auto filter2D = myFilterType2D::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter2D, DanielssonDistanceMapImageFilter, ImageToImageFilter);


  filter2D->SetInput(inputImage2D);

  const myImageType2D2::Pointer outputDistance2D = filter2D->GetOutput();

  using VoronoiImageType = myFilterType2D::VoronoiImageType;

  const VoronoiImageType::Pointer outputVoronoi2D = filter2D->GetVoronoiMap();

  const myFilterType2D::VectorImagePointer outputComponents = filter2D->GetVectorDistanceMap();

  ITK_TRY_EXPECT_NO_EXCEPTION(filter2D->Update());


  ShowDistanceMap(outputDistance2D);
  std::cout << "Voronoi Map Image 2D" << std::endl << std::endl;
  ShowDistanceMap(outputVoronoi2D);

  // Show VectorsComponents Points map
  std::cout << std::endl << std::endl;
  std::cout << "Components Map Image 2D" << std::endl << std::endl;

  itk::ImageSliceConstIteratorWithIndex<myFilterType2D::VectorImageType> it2D4(outputComponents,
                                                                               outputComponents->GetRequestedRegion());

  it2D4.SetFirstDirection(0);
  it2D4.SetSecondDirection(1);

  while (!it2D4.IsAtEnd())
  {
    while (!it2D4.IsAtEndOfSlice())
    {
      while (!it2D4.IsAtEndOfLine())
      {
        std::cout << '[';
        for (unsigned int i = 0; i < 2; ++i)
        {
          std::cout << it2D4.Get()[i];
          if (i == 0)
          {
            std::cout << ',';
          }
        }
        std::cout << ']';
        std::cout << '\t';
        ++it2D4;
      }
      std::cout << std::endl;
      it2D4.NextLine();
    }
    it2D4.NextSlice();
  }


  // Test Squared Distance functionality
  myImageType2D2::IndexType index;
  index[0] = 0;
  index[1] = 0;
  const double distance1 = outputDistance2D->GetPixel(index);

  constexpr bool squaredDistance{ true };
  ITK_TEST_SET_GET_BOOLEAN(filter2D, SquaredDistance, squaredDistance);

  ITK_TRY_EXPECT_NO_EXCEPTION(filter2D->Update());


  const double                        distance2 = outputDistance2D->GetPixel(index);
  constexpr myImageType2D2::PixelType epsilon{ 1e-5 };
  if (itk::Math::abs(distance2 - distance1 * distance1) > epsilon)
  {
    std::cerr << "Error in use of the SetSquaredDistance() method" << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Squared Distance Map " << std::endl;
  ShowDistanceMap(outputDistance2D);


  // Test for images with anisotropic spacing
  const myImageType2D1::SpacingType anisotropicSpacing({ 1.0, 5.0 });

  inputImage2D->SetSpacing(anisotropicSpacing);

  inputImage2D->FillBuffer(0);

  index2D[0] = 4;
  index2D[1] = 4;
  inputImage2D->SetPixel(index2D, 1);

  filter2D->SetInput(inputImage2D);

  constexpr bool inputIsBinary{ true };
  ITK_TEST_SET_GET_BOOLEAN(filter2D, InputIsBinary, inputIsBinary);

  constexpr bool useImageSpacing{ true };
  ITK_TEST_SET_GET_BOOLEAN(filter2D, UseImageSpacing, useImageSpacing);

  ITK_TRY_EXPECT_NO_EXCEPTION(filter2D->Update());

  index2D[1] = 5;
  auto expectedValue = static_cast<myImageType2D2::PixelType>(anisotropicSpacing[1]);
  expectedValue *= expectedValue;
  const myImageType2D2::PixelType pixelValue = filter2D->GetOutput()->GetPixel(index2D);
  if (itk::Math::abs(expectedValue - pixelValue) > epsilon)
  {
    std::cerr << "Error when image spacing is anisotropic." << std::endl;
    std::cerr << "Pixel value was " << pixelValue << ", expected " << expectedValue << std::endl;
    return EXIT_FAILURE;
  }

  ShowDistanceMap(outputDistance2D);

  // Create a large 3D image with a small foreground object.  The foreground is denoted by a pixel value of 0,
  // and the background by a non-zero pixel value.  This will test speedups to the code that ignore all background
  // pixels in the computation since those pixels do not influence the result.

  // Allocate the 3D image
  using ImageType3D = itk::Image<float, 3>;
  constexpr ImageType3D::SizeType  size3D{ 200, 200, 200 };
  constexpr ImageType3D::IndexType index3D{ 0, 0 };
  const ImageType3D::RegionType    region3D{ index3D, size3D };
  auto                             inputImage3D = ImageType3D::New();
  inputImage3D->SetRegions(region3D);
  inputImage3D->Allocate();
  inputImage3D->FillBuffer(1);

  // Set a few pixels in the middle of the image to 0.  These are the foreground pixels for which the distance will be
  // solved.
  ImageType3D::IndexType foregroundIndex;
  ImageType3D::SizeType  foregroundSize;
  for (unsigned int i = 0; i < 3; ++i)
  {
    foregroundSize[i] = 5;
    foregroundIndex[i] = (size3D[i] / 2) - foregroundSize[i] / 2;
  }
  const ImageType3D::RegionType foregroundRegion{ foregroundIndex, foregroundSize };

  itk::ImageRegionIteratorWithIndex<ImageType3D> it3D(inputImage3D, foregroundRegion);
  for (it3D.GoToBegin(); !it3D.IsAtEnd(); ++it3D)
  {
    it3D.Set(0);
  }

  // Create Danielsson Distance Map filter
  using myFilterType3D = itk::DanielssonDistanceMapImageFilter<ImageType3D, ImageType3D>;

  auto filter3D = myFilterType3D::New();

  filter3D->SetInput(inputImage3D);

  ITK_TRY_EXPECT_NO_EXCEPTION(filter3D->Update());


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
