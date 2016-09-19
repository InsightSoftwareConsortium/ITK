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

#include <iostream>

#include "itkAffineTransform.h"
#include "itkResampleImageFilter.h"
#include "itkImageFileWriter.h"
#include "itkMath.h"
#include "itkTestingMacros.h"

int itkResampleImageTest(int, char* [] )
{

  const unsigned int NDimensions = 2;

  typedef float                               PixelType;
  typedef itk::Image<PixelType, NDimensions>  ImageType;
  typedef ImageType::IndexType                ImageIndexType;
  typedef ImageType::Pointer                  ImagePointerType;
  typedef ImageType::RegionType               ImageRegionType;
  typedef ImageType::SizeType                 ImageSizeType;
  typedef double                              CoordRepType;

  typedef itk::AffineTransform<CoordRepType,NDimensions>               AffineTransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,CoordRepType>  InterpolatorType;


  // Create and configure an image
  ImagePointerType image = ImageType::New();
  ImageIndexType  index = {{0,  0}};
  ImageSizeType   size  = {{18, 12}};
  ImageRegionType region;
  region.SetSize ( size );
  region.SetIndex( index );
  image->SetLargestPossibleRegion( region );
  image->SetBufferedRegion( region );
  image->Allocate();

  // Fill image with a ramp
  itk::ImageRegionIteratorWithIndex<ImageType> iter(image, region);
  PixelType value;
  for (iter.GoToBegin(); !iter.IsAtEnd(); ++iter)
    {
    index = iter.GetIndex();
    value = index[0] + index[1];
    iter.Set(value);
    }

  // Create an affine transformation
  AffineTransformType::Pointer aff = AffineTransformType::New();
  aff->Scale(0.5);

  // Create a linear interpolation image function
  InterpolatorType::Pointer interp = InterpolatorType::New();
  interp->SetInputImage(image);

  // Create and configure a resampling filter
  itk::ResampleImageFilter< ImageType, ImageType >::Pointer resample =
    itk::ResampleImageFilter< ImageType, ImageType >::New();

  EXERCISE_BASIC_OBJECT_METHODS( resample, ResampleImageFilter, ImageToImageFilter );

  resample->SetInput(image);
  TEST_SET_GET_VALUE( image, resample->GetInput() );

  resample->SetSize(size);
  TEST_SET_GET_VALUE( size, resample->GetSize() );

  resample->SetTransform(aff);
  TEST_SET_GET_VALUE( aff, resample->GetTransform() );

  resample->SetInterpolator(interp);
  TEST_SET_GET_VALUE( interp, resample->GetInterpolator() );

  index.Fill( 0 );
  resample->SetOutputStartIndex( index );
  TEST_SET_GET_VALUE( index, resample->GetOutputStartIndex() );

  ImageType::PointType origin;
  origin.Fill( 0.0 );
  resample->SetOutputOrigin( origin );
  TEST_SET_GET_VALUE( origin, resample->GetOutputOrigin() );

  ImageType::SpacingType spacing;
  spacing.Fill( 1.0 );
  resample->SetOutputSpacing( spacing );
  TEST_SET_GET_VALUE( spacing, resample->GetOutputSpacing() );


  // Run the resampling filter
  resample->Update();

  // Check if desired results were obtained
  bool passed = true;
  ImageType::RegionType region2;
  region2 = resample->GetOutput()->GetRequestedRegion();
  itk::ImageRegionIteratorWithIndex<ImageType>
      iter2(resample->GetOutput(), region2);
  PixelType pixval;
  const double tolerance = 1e-30;
  for (iter2.GoToBegin(); !iter2.IsAtEnd(); ++iter2)
    {
    index  = iter2.GetIndex();
    value  = iter2.Get();
    pixval = value;
    PixelType expectedValue = static_cast<PixelType>( (index[0] + index[1]) / 2.0 );
    if ( !itk::Math::FloatAlmostEqual( expectedValue, pixval, 10, tolerance ) )
      {
      std::cout << "Error in resampled image: Pixel " << index
                << "value    = " << value << "  "
                << "pixval   = " << pixval << "  "
                << "expected = " << expectedValue << std::endl;
      passed = false;
      }
    }

  // Report success or failure
  if (!passed)
    {
    std::cout << "Resampling test failed" << std::endl;
    return EXIT_FAILURE;
    }

  // Exercise error handling

  try
    {
    std::cout << "Setting interpolator to ITK_NULLPTR" << std::endl;
    passed = false;
    resample->SetInterpolator( ITK_NULLPTR );
    resample->Update();
    }
  catch( itk::ExceptionObject& err )
    {
    std::cout << err << std::endl;
    passed = true;
    resample->ResetPipeline();
    resample->SetInterpolator( interp );
    }

  if (!passed)
    {
    std::cout << "Resampling test failed" << std::endl;
    return EXIT_FAILURE;
    }

 std::cout << "Test passed." << std::endl;
 return EXIT_SUCCESS;

}
