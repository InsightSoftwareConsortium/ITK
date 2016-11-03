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
#include "itkImageRegionIteratorWithIndex.h"
#include <itkTestingComparisonImageFilter.h>
#include <itkMath.h>
#include <itkNumericTraits.h>
#include "metaImage.h"

int itkImageFileReaderPositiveSpacingTest(int ac, char* av[])
{

  if (ac < 1)
    {
    std::cout << "usage: ITKImageIOBaseTestDriver itkImageFileReaderPositiveSpacingTest" << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::Image<short,2>               ImageNDType;
  typedef itk::ImageFileReader<ImageNDType> ReaderType;

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(av[1]);
  reader->Update();
  ImageNDType::Pointer image = reader->GetOutput();
  image->DisconnectPipeline();
  ImageNDType::SpacingType spacing = image->GetSpacing();
  for(unsigned int ii=0; ii < image->GetImageDimension(); ii++)
    {
    if(spacing[ii] < 0)
      {
      std::cerr << "Spacing should be a positive value. Found " << spacing[ii] << std::endl;
      return EXIT_FAILURE;
      }
    }
  std::cout << "Spacing: " << spacing << std::endl;
  ImageNDType::DirectionType direction = image->GetDirection();
  std::cout << "Direction: " << "\n" << direction << std::endl;

  MetaImage metaImage;
  if ( !metaImage.Read(av[1], false) )
    {
    std::cerr << "File cannot be opened "
              << av[1] << " for reading."
              << std::endl
              << "Reason: "
              << itksys::SystemTools::GetLastSystemError();
    return EXIT_FAILURE;
    }
  ImageNDType::DirectionType ioDirection;
  ImageNDType::SpacingType ioSpacing;
  ImageNDType::PointType ioOrigin;
  ImageNDType::SizeType ioSize;
  const double *transformMatrix = metaImage.TransformMatrix();
  for ( unsigned int ii = 0; ii < ImageNDType::ImageDimension; ii++ )
    {
    ioSize[ii] = metaImage.DimSize(ii);
    ioSpacing[ii] = metaImage.ElementSpacing(ii);
    ioOrigin[ii] = metaImage.Position(ii);
    // Please note: direction cosines are stored as columns of the
    // direction matrix
    for ( unsigned int jj = 0; jj < ImageNDType::ImageDimension; jj++ )
      {
      ioDirection[jj][ii] = transformMatrix[ii * ImageNDType::ImageDimension + jj];
      }
    }
  std::cout << "Spacing baseline: " << ioSpacing << std::endl;
  std::cout << "Direction baseline: " << "\n" << ioDirection << std::endl;

  // Go through entire image and make sure that at each physical pixel location, the value of the images, with negative
  // spacing and positive spacing, are the same.
  // itk::Testing::ComparisonImageFilter does not take into account spacing and orientation.
  // We manually go through all pixels in the input image and compare their value without
  // their value in the image loaded without ensuring positive spacing.
  ImageNDType::DirectionType scale;
  ImageNDType::DirectionType indexToPhysicalPoint;
  ImageNDType::DirectionType physicalPointToIndex;
  for( unsigned int ii = 0; ii < ImageNDType::ImageDimension; ii++ )
    {
    scale[ii][ii] = ioSpacing[ii];
    }
  indexToPhysicalPoint = ioDirection * scale;
  physicalPointToIndex = indexToPhysicalPoint.GetInverse();

  typedef itk::ImageRegionIteratorWithIndex< ImageNDType > IteratorType;
  IteratorType it(image, image->GetLargestPossibleRegion());
  for(it.GoToBegin();!it.IsAtEnd();++it)
    {
    ImageNDType::IndexType index = it.GetIndex();
    ImageNDType::PointType point;
    image->TransformIndexToPhysicalPoint(index, point);
    // Compute index from physical point in baseline
    ImageNDType::IndexType baselineIndex;
    for ( unsigned int ii = 0; ii < ImageNDType::ImageDimension; ii++ )
      {
      double sum = itk::NumericTraits< double >::ZeroValue();
      for ( unsigned int jj = 0; jj < ImageNDType::ImageDimension; jj++ )
        {
        sum += physicalPointToIndex[ii][jj] * ( point[jj] - ioOrigin[jj] );
        }
      baselineIndex[ii] = itk::Math::RoundHalfIntegerUp< ImageNDType::IndexType::IndexValueType >(sum);
      }
    if( index != baselineIndex)
      {
      std::cerr << "Difference found between original image and flipped spacing image: "
                << "Indices do not correspond." << "\n"
                << "Flipped image: " << index << "\n"
                << "Baseline image:" << baselineIndex
                << std::endl;
      return EXIT_FAILURE;
      }
    }
  return EXIT_SUCCESS;
}
