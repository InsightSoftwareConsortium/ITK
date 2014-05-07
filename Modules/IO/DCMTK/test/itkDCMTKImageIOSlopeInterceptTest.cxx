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

#include "itkImageFileReader.h"
#include "itkDCMTKImageIO.h"
#include "itkImageRegionConstIterator.h"
#include "itkMultiplyImageFilter.h"
#include "itkAddImageFilter.h"
#include "itkSubtractImageFilter.h"
#include "itkStatisticsImageFilter.h"

int itkDCMTKImageIOSlopeInterceptTest(int ac, char * av[])
{
  if(ac < 3)
    {
    std::cerr << "Usage: " << av[0]
              << " <original image> <slope intercept image>"
              << std::endl;
    return EXIT_FAILURE;
    }

  typedef short                             PixelType;
  typedef itk::Image< PixelType, 3 >        ImageType;
  typedef itk::ImageFileReader< ImageType > ReaderType;
  typedef itk::DCMTKImageIO                 ImageIOType;

  const PixelType rescaleSlope(2);
  const PixelType rescaleIntercept(-99);

  ImageIOType::Pointer dcmImageIO = ImageIOType::New();

  ImageType::Pointer images[2];
  for(unsigned i = 0; i < 2; ++i)
    {
    ReaderType::Pointer reader = ReaderType::New();
    reader->SetFileName( av[i+1] );
    reader->SetImageIO( dcmImageIO );

    try
      {
      reader->Update();
      }
    catch (itk::ExceptionObject & e)
      {
      std::cerr << "exception in file reader " << std::endl;
      std::cerr << e << std::endl;
      return EXIT_FAILURE;
      }
    images[i] = reader->GetOutput();
    }

  //
  // the two inputs are a DICOM image without slope/intercept tags,
  // and the same image with slpe/intercept tags.  I read the first
  // image and apply the slope/intercept, and then subtract the first
  // from the second, then look for non-zero min/max/mean.  They
  // should be identical.
  typedef itk::ImageRegionConstIterator<ImageType> ItType;
  ItType it1(images[0],images[0]->GetLargestPossibleRegion());
  ItType it2(images[1],images[1]->GetLargestPossibleRegion());
  for(it1.GoToBegin(), it2.GoToBegin(); !it1.IsAtEnd() && !it2.IsAtEnd(); ++it1, ++it2)
    {
    PixelType pix1(it1.Get());
    const PixelType pix2(it2.Get());
    pix1 = (pix1 * rescaleSlope) + rescaleIntercept;
    if(pix1 != pix2)
      {
      std::cerr << "computed pixel doesn't match pixel from slopeIntercept image: computed = "
                << pix1 << " slopeIntercept image = " << pix2 << std::endl;
      return EXIT_FAILURE;
      }
    }
  return EXIT_SUCCESS;
}
