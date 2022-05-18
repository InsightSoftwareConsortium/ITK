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

#include "itkImageFileReader.h"
#include "itkDCMTKImageIO.h"
#include "itkImageRegionConstIterator.h"
#include "itkMultiplyImageFilter.h"
#include "itkAddImageFilter.h"
#include "itkSubtractImageFilter.h"
#include "itkStatisticsImageFilter.h"
#include "itkTestingMacros.h"

int
itkDCMTKImageIOSlopeInterceptTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing Parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " originalImage slopeInterceptImage" << std::endl;
    return EXIT_FAILURE;
  }

  using PixelType = short;
  using ImageType = itk::Image<PixelType, 3>;
  using ReaderType = itk::ImageFileReader<ImageType>;
  using ImageIOType = itk::DCMTKImageIO;

  const PixelType rescaleSlope(2);
  const PixelType rescaleIntercept(-99);

  auto dcmImageIO = ImageIOType::New();

  ImageType::Pointer images[2];
  for (unsigned int i = 0; i < 2; ++i)
  {
    auto reader = ReaderType::New();
    reader->SetFileName(argv[i + 1]);
    reader->SetImageIO(dcmImageIO);

    ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());


    images[i] = reader->GetOutput();
  }

  //
  // the two inputs are a DICOM image without slope/intercept tags,
  // and the same image with slpe/intercept tags.  I read the first
  // image and apply the slope/intercept, and then subtract the first
  // from the second, then look for non-zero min/max/mean.  They
  // should be identical.
  using ItType = itk::ImageRegionConstIterator<ImageType>;
  ItType it1(images[0], images[0]->GetLargestPossibleRegion());
  ItType it2(images[1], images[1]->GetLargestPossibleRegion());
  for (it1.GoToBegin(), it2.GoToBegin(); !it1.IsAtEnd() && !it2.IsAtEnd(); ++it1, ++it2)
  {
    PixelType       pix1(it1.Get());
    const PixelType pix2(it2.Get());
    pix1 = (pix1 * rescaleSlope) + rescaleIntercept;
    if (pix1 != pix2)
    {
      std::cerr << "computed pixel doesn't match pixel from slopeIntercept image: computed = " << pix1
                << " slopeIntercept image = " << pix2 << std::endl;
      return EXIT_FAILURE;
    }
  }


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
