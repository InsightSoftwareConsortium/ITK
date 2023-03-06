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
#include "itkRescaleIntensityImageFilter.h"


template <typename ImageType>
typename ImageType::Pointer
MakeOnes3x3Image()
{
  typename ImageType::Pointer onesImage = ImageType::New();
  {
    typename ImageType::SizeType   smallest_size{ { 3, 3 } };
    typename ImageType::IndexType  start_index{ { 0, 0 } };
    typename ImageType::RegionType my_region(start_index, smallest_size);
    onesImage->SetRegions(my_region);
  }
  onesImage->Allocate();
  onesImage->FillBuffer(1);
  return onesImage;
}

template <typename ImageType>
typename ImageType::Pointer
DoConvolution(typename ImageType::Pointer inputImage, unsigned long int direction)
{
  using PixelType = typename ImageType::PixelType;
  constexpr std::size_t Dimension = ImageType::ImageDimension;

  using SobelOperatorType = itk::SobelOperator<PixelType, Dimension>;

  using NeighborhoodIteratorType = itk::ConstNeighborhoodIterator<ImageType>;
  using IteratorType = itk::ImageRegionIterator<ImageType>;

  SobelOperatorType sobelOperator;

  sobelOperator.SetDirection(direction);

  itk::Size<Dimension> radius;
  radius.Fill(1);
  sobelOperator.CreateToRadius(radius);

  NeighborhoodIteratorType it(radius, inputImage, inputImage->GetRequestedRegion());

  auto outputImage = ImageType::New();
  outputImage->SetRegions(inputImage->GetRequestedRegion());
  outputImage->Allocate(true);

  IteratorType                             out(outputImage, inputImage->GetRequestedRegion());
  itk::NeighborhoodInnerProduct<ImageType> innerProduct;
  for (it.GoToBegin(), out.GoToBegin(); !it.IsAtEnd(); ++it, ++out)
  {
    const auto pixelValue = innerProduct(it, sobelOperator);
    out.Set(pixelValue);
  }
  return outputImage;
}

template <typename PixelType, unsigned long Dimension>
int
DoSimpleConvolutionTest(unsigned long direction, const std::string & pixelType)
{
  using ImageType = typename itk::Image<PixelType, Dimension>;

  typename ImageType::Pointer smallestOnesImage = MakeOnes3x3Image<ImageType>();
  typename ImageType::Pointer output3x3Image = DoConvolution<ImageType>(smallestOnesImage, direction);

  typename ImageType::IndexType center_index{ { 1, 1 } };
  typename ImageType::PixelType center_value = output3x3Image->GetPixel(center_index);
  if (center_value != 0)
  {
    std::cout << "ERROR: Constant image convolution with SobelOperator should return 0, "
              << "but value of " << +center_value << " was computed. [" << pixelType << "]" << std::endl;
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}

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
  const auto direction = std::stoul(argv[2]);
  int        return_status = EXIT_SUCCESS;

  constexpr unsigned int Dimension = 2;
  /* Sanity Checking For Sobel Operator */
  //{
  /*
   * Demonstrate that signed types do **NOT** work with SobelOperator
   * The unsigned checks require disableing Signed ConceptChecking in SobelOperator
   * added in March 2023.
   */
  /*
  return_status += DoSimpleConvolutionTest<unsigned char, 2>(direction, "unsigned char");
  return_status += DoSimpleConvolutionTest<unsigned short, 2>(direction, "unsigned short");
  return_status += DoSimpleConvolutionTest<unsigned int, 2>(direction, "unsigned int");
  return_status += DoSimpleConvolutionTest<unsigned long, 2>(direction, "unsigned long");
  */
  //}
  {
    /*
     * Demonstrate that signed types do work with SobelOperator
     */
    return_status += DoSimpleConvolutionTest<char, 2>(direction, "char");
    return_status += DoSimpleConvolutionTest<short, 2>(direction, "short");
    return_status += DoSimpleConvolutionTest<int, 2>(direction, "int");
    return_status += DoSimpleConvolutionTest<long, 2>(direction, "long");
    return_status += DoSimpleConvolutionTest<float, 2>(direction, "float");
    return_status += DoSimpleConvolutionTest<double, 2>(direction, "double");
  }

  {
    /* Test on a real image */
    using PixelType = int16_t;
    using ImageType = itk::Image<PixelType, Dimension>;

    const auto inputImage = itk::ReadImage<ImageType>(argv[1]);

    auto signedSobelImage = DoConvolution<ImageType>(inputImage, direction);

    using OutputImageType = itk::Image<uint8_t, Dimension>;
    // Assume min/max values are approximately +/- same magnitude so that the output images
    // to be stored in uint8_t have an implied 0 at about pixel value 128.  Many web based viewers
    // for the difference images in the testing outputs render better in this positive png range.
    using RescaleIntensityType = itk::RescaleIntensityImageFilter<ImageType, OutputImageType>;
    RescaleIntensityType::Pointer rescalerForVisualization = RescaleIntensityType::New();
    rescalerForVisualization->SetInput(signedSobelImage);
    rescalerForVisualization->SetOutputMinimum(0);
    rescalerForVisualization->SetOutputMaximum(255);
    rescalerForVisualization->Update();
    itk::WriteImage(rescalerForVisualization->GetOutput(), argv[3]);
  }

  std::cout << "Test finished." << std::endl;
  return return_status;
}
