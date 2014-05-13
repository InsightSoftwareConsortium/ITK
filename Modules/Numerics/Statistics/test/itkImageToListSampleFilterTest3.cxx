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
// The example tests the class itk::Statistics::ImageToListSampleFilter.
// The class is capable of generating an itk::ListSample from an image
// confined to a mask (if specified). This test exercises that.


#include "itkImageToListSampleFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkVectorImage.h"

int itkImageToListSampleFilterTest3(int, char* [] )
{
  const unsigned int MeasurementVectorSize = 8;
  typedef float MeasurementComponentType;

  const unsigned int ImageDimension = 3;
  typedef itk::VectorImage< MeasurementComponentType, ImageDimension > ImageType;

  typedef itk::Image< unsigned char, ImageDimension > MaskImageType;

  typedef ImageType::PixelType  PixelType;

  ImageType::Pointer image = ImageType::New();

  image->SetNumberOfComponentsPerPixel( MeasurementVectorSize );

  ImageType::IndexType start;
  ImageType::SizeType  size;

  start.Fill( 0 );
  size.Fill( 10 );

  ImageType::RegionType region( start, size );
  image->SetRegions( region );
  image->Allocate();
  typedef itk::ImageRegionIteratorWithIndex< ImageType > IteratorType;
  IteratorType it( image, region );
  it.GoToBegin();

  PixelType value;
  value.SetSize( MeasurementVectorSize );

  while (!it.IsAtEnd())
    {
    for( unsigned int i=0; i< MeasurementVectorSize; i++ )
      {
      value[i] = i + it.GetIndex()[0];
      }
    it.Set( value );
    ++it;
    }

  MaskImageType::Pointer maskImage = MaskImageType::New();
  maskImage->SetRegions( region );
  maskImage->Allocate(true); // initialize
                                                    // buffer to zero

  MaskImageType::IndexType startMask;
  MaskImageType::SizeType sizeMask;

  startMask[0] = 2;
  startMask[1] = 3;
  startMask[2] = 5;

  sizeMask[0] = 7;
  sizeMask[1] = 3;
  sizeMask[2] = 4;

  MaskImageType::RegionType regionMask( startMask, sizeMask);
  typedef itk::ImageRegionIteratorWithIndex< MaskImageType > MaskIteratorType;
  MaskIteratorType mit( maskImage, regionMask );
  mit.GoToBegin();
  while (!mit.IsAtEnd())
    {
    mit.Set((unsigned char)255);
    ++mit;
    }

  // Generate a list sample from "image" confined to the mask, "maskImage".
  typedef itk::Statistics::ImageToListSampleFilter<
    ImageType, MaskImageType > ImageToListSampleFilterType;
  ImageToListSampleFilterType::Pointer filter
                              = ImageToListSampleFilterType::New();

  filter->SetInput( image );
  filter->SetMaskImage( maskImage );
  filter->SetMaskValue( 255 );

  try
    {
    filter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  typedef ImageToListSampleFilterType::ListSampleType ListSampleType;
  const ListSampleType * list = filter->GetOutput();

  // Check the sum of the pixels in the list sample. This should
  // be 420.0
  ListSampleType::ConstIterator lit = list->Begin();
  float sum = 0.0;

  while (lit != list->End())
    {
    sum += lit.GetMeasurementVector()[0];
    ++lit;
    }

  const double epsilon = 1e-6;

  if ( std::fabs( sum - 420.0 ) > epsilon )
    {
    std::cerr << "Computed sum of pixels in the list sample (masked) is : "
              << sum
              << " but should be 420.0";
    return EXIT_FAILURE;
    }

  std::cerr << "[PASSED]" << std::endl;
  return EXIT_SUCCESS;
}
