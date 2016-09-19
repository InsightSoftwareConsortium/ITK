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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkChangeInformationImageFilter.h"
#include "itkResampleImageFilter.h"
#include "itkTestingMacros.h"

/* Further testing of itkResampleImageFilter
 * Test that ResampleImageFilter can handle a ReferenceImage with
 * meta information that differs from the input imake.
 * Output is compared with baseline image using the cmake itk_add_test
 * '--compare' option.
 */

int itkResampleImageTest3(int argc, char * argv [] )
{

  if( argc < 3 )
    {
    std::cerr << "Usage: "<< argv[0];
    std::cerr << " inputImage resampledImage" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int NDimensions = 2;

  typedef unsigned char                          PixelType;
  typedef itk::Image<PixelType, NDimensions>     ImageType;
  typedef double                                 CoordRepType;

  typedef itk::IdentityTransform<CoordRepType,NDimensions>  TransformType;
  typedef itk::LinearInterpolateImageFunction<ImageType,CoordRepType>
                                                            InterpolatorType;

  typedef itk::ImageFileReader< ImageType >                ReaderType;
  typedef itk::ImageFileWriter< ImageType >                WriterType;
  typedef itk::ChangeInformationImageFilter< ImageType >   ChangeInfoType;
  typedef itk::ResampleImageFilter< ImageType, ImageType > ResampleFilterType;

  ReaderType::Pointer reader1 = ReaderType::New();
  reader1->SetFileName( argv[1] );
  reader1->Update();

  // Create an identity transformation
  TransformType::Pointer identityTransform = TransformType::New();

  // Create a linear interpolation image function
  InterpolatorType::Pointer interpolator = InterpolatorType::New();

  // Create an image with flipped directions
  ImageType::DirectionType direction;
  direction[0][0] = -1.0;
  direction[0][1] = 0.0;
  direction[1][0] = 0.0;
  direction[1][1] = -1.0;
  ImageType::RegionType inputRegion =
  reader1->GetOutput()->GetLargestPossibleRegion();
  ImageType::PointType origin;
  origin[0] = inputRegion.GetSize()[0];
  origin[1] = inputRegion.GetSize()[1];

  ChangeInfoType::Pointer changeInfo = ChangeInfoType::New();
  changeInfo->ChangeDirectionOn();
  changeInfo->SetOutputDirection( direction );
  changeInfo->ChangeOriginOn();
  changeInfo->SetOutputOrigin( origin );
  changeInfo->SetInput( reader1->GetOutput() );

  // Create and configure a resampling filter
  ResampleFilterType::Pointer resample = ResampleFilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( resample, ResampleImageFilter, ImageToImageFilter );

  resample->SetInput( reader1->GetOutput() );
  TEST_SET_GET_VALUE( reader1->GetOutput(), resample->GetInput() );

  resample->SetReferenceImage( changeInfo->GetOutput() );
  TEST_SET_GET_VALUE( changeInfo->GetOutput(), resample->GetReferenceImage() );

  resample->UseReferenceImageOn();
  TEST_EXPECT_TRUE( resample->GetUseReferenceImage() );

  resample->SetTransform( identityTransform );
  TEST_SET_GET_VALUE( identityTransform, resample->GetTransform() );

  resample->SetInterpolator( interpolator );
  TEST_SET_GET_VALUE( interpolator, resample->GetInterpolator() );

  WriterType::Pointer writer1 = WriterType::New();
  writer1->SetFileName( argv[2] );
  writer1->SetInput( resample->GetOutput() );

  // Run the resampling filter
  try
    {
    writer1->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

 std::cout << "Test passed." << std::endl;
 return EXIT_SUCCESS;

}
