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
#include "itkPipelineMonitorImageFilter.h"
#include "itkStreamingImageFilter.h"
#include "itkTestingMacros.h"
#include "itkMath.h"

/* itkResampleImageFilter output compared to streamed output
 */

int itkResampleImageTest7( int , char *[] )
{

  constexpr unsigned int NDimensions = 2;

  using PixelType = float;

  using ImageType = itk::Image<PixelType, NDimensions>;
  using ImageIndexType = ImageType::IndexType;
  using ImagePointerType = ImageType::Pointer;
  using ImageRegionType = ImageType::RegionType;
  using ImageSizeType = ImageType::SizeType;

  using CoordRepType = double;

  using AffineTransformType = itk::AffineTransform<CoordRepType,NDimensions>;

  using InterpolatorType = itk::LinearInterpolateImageFunction<ImageType,CoordRepType>;

  // Create and configure an image
  ImagePointerType image = ImageType::New();
  ImageIndexType  index = {{0,  0}};
  ImageSizeType   size  = {{64,64}};
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
  aff->Scale(0.9);

  // Create a linear interpolation image function
  InterpolatorType::Pointer interp = InterpolatorType::New();
  interp->SetInputImage(image);

  // Create and configure a resampling filter
  itk::ResampleImageFilter< ImageType, ImageType >::Pointer resample =
    itk::ResampleImageFilter< ImageType, ImageType >::New();

  EXERCISE_BASIC_OBJECT_METHODS( resample, ResampleImageFilter, ImageToImageFilter );

  using MonitorFilter = itk::PipelineMonitorImageFilter<ImageType>;
  MonitorFilter::Pointer monitor = MonitorFilter::New();
  monitor->SetInput( image );

  resample->SetInterpolator(interp);

  resample->SetInput( monitor->GetOutput() );
  TEST_SET_GET_VALUE( monitor->GetOutput(), resample->GetInput() );

  resample->SetSize( size );
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

  using StreamerType = itk::StreamingImageFilter<ImageType,ImageType>;
  StreamerType::Pointer streamer = StreamerType::New();

  std::cout << "Test with normal AffineTransform." << std::endl;
  streamer->SetInput( resample->GetOutput() );

  unsigned char numStreamDiv;

  // Run the resampling filter without streaming, i.e. 1 StreamDivisions
  numStreamDiv= 1; // do not split, i.e. do not stream
  streamer->SetNumberOfStreamDivisions(numStreamDiv);
  TRY_EXPECT_NO_EXCEPTION( streamer->Update() );

  if (!monitor->VerifyAllInputCanStream(numStreamDiv))
    {
    std::cerr << "Avoiding streaming failed to execute as expected!" << std::endl;
    std::cerr << monitor;
    std::cerr << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }
  monitor->ClearPipelineSavedInformation();

  ImagePointerType outputNoSDI= streamer->GetOutput(); // save output for later comparison
  outputNoSDI->DisconnectPipeline(); // disconnect to create new output

  // Run the resampling filter with streaming
  numStreamDiv= 8; // split into numStream pieces for streaming.
  image->Modified(); // enforce re-execution even though nothing of filter changed
  streamer->SetNumberOfStreamDivisions(numStreamDiv);
  TRY_EXPECT_NO_EXCEPTION( streamer->Update() );

  if (!monitor->VerifyAllInputCanStream(numStreamDiv))
    {
    std::cerr << "Streaming failed to execute as expected!" << std::endl;
    std::cerr << monitor;
    std::cerr << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }
  monitor->ClearPipelineSavedInformation();

  ImagePointerType outputSDI= streamer->GetOutput();
  outputSDI->DisconnectPipeline();

  itk::ImageRegionIterator<ImageType>
      itNoSDI(outputNoSDI, outputNoSDI->GetLargestPossibleRegion()),
      itSDI(outputSDI, outputSDI->GetLargestPossibleRegion());
  for(itNoSDI.GoToBegin(), itSDI.GoToBegin();
      !itNoSDI.IsAtEnd() && !itSDI.IsAtEnd();
      ++itNoSDI, ++itSDI)
    {
    if(itk::Math::NotAlmostEquals( itNoSDI.Value(), itSDI.Value() ))
      {
      std::cout << "Pixels differ "
                << itNoSDI.Value() << " "
                << itSDI.Value() << std::endl;
      std::cerr << "Test failed!" << std::endl;
      std::cerr << "Error in pixel value at index [" << itNoSDI.GetIndex() << "]" << std::endl;
      std::cerr << "Expected difference " << itNoSDI.Get() - itSDI.Get() << std::endl;
      std::cerr << " differs from 0 ";
      return EXIT_FAILURE;
      }
    }
  if(itNoSDI.IsAtEnd() != itSDI.IsAtEnd())
    {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Iterators don't agree on end of image" << std::endl;
    std::cerr << "at index [" << itNoSDI.GetIndex() << "]" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}
