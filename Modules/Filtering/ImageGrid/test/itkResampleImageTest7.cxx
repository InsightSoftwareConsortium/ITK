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

int itkResampleImageTest7( int argc, char * argv [] )
{

  if (argc > 1)
    {
    std::cout << "Usage: " << argv[0]
              << " noParams" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int NDimensions = 2;

  typedef unsigned char                          PixelType;
  typedef itk::Image<PixelType, NDimensions>     ImageType;
  typedef ImageType::IndexType         ImageIndexType;
  typedef ImageType::Pointer           ImagePointerType;
  typedef ImageType::RegionType        ImageRegionType;
  typedef ImageType::SizeType          ImageSizeType;
  typedef double                                 CoordRepType;

  typedef itk::AffineTransform<CoordRepType,NDimensions>
                                                 AffineTransformType;

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
  std::cout << "init image..." << std::flush;
  itk::ImageRegionIteratorWithIndex<ImageType> iter(image, region);
  PixelType value;
  for (iter.GoToBegin(); !iter.IsAtEnd(); ++iter)
    {
    index = iter.GetIndex();
    value = index[0] + index[1];
    iter.Set(value);
    }
  std::cout << "Done." << std::endl;

  // Create an affine transformation
  AffineTransformType::Pointer affineTransform = AffineTransformType::New();
  affineTransform->Scale(2.0);

  // Create and configure a resampling filter
  typedef itk::ResampleImageFilter< ImageType, ImageType > ResampleFilterType;
  ResampleFilterType::Pointer resample = ResampleFilterType::New();

  typedef itk::PipelineMonitorImageFilter< ImageType > MonitorFilter;
  MonitorFilter::Pointer monitor = MonitorFilter::New();

  typedef itk::StreamingImageFilter<ImageType,ImageType> StreamerType;
  StreamerType::Pointer streamer = StreamerType::New();

  std::cout << "Test with normal AffineTransform." << std::endl;
  resample->SetInput( image );
  resample->SetTransform( affineTransform );
  monitor->SetInput( resample->GetOutput() );
  streamer->SetInput( monitor->GetOutput() );

  unsigned char numStreamDiv;
  
  // Run the resampling filter without streaming, i.e. 1 StreamDivisions
  numStreamDiv= 1; // do not split, i.e. do not stream
  try
    {
    streamer->SetNumberOfStreamDivisions(numStreamDiv);
    streamer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }
  
  if (!monitor->VerifyAllInputCanStream(numStreamDiv))
    {
    std::cout << "Avoiding streaming failed to execute as expected!" << std::endl;
    std::cout << monitor;
    return EXIT_FAILURE;
    }

  ImagePointerType outputNoSDI= streamer->GetOutput();
  outputNoSDI->DisconnectPipeline();

  // Run the resampling filter with streaming
  numStreamDiv= 8; // split into numStream pieces for streaming.
  try
    {
    streamer->SetNumberOfStreamDivisions(numStreamDiv);
    streamer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }
  
  if (!monitor->VerifyAllInputCanStream(numStreamDiv))
    {
    std::cout << "Streaming failed to execute as expected!" << std::endl;
    std::cout << monitor;
    return EXIT_FAILURE;
    }

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
      return EXIT_FAILURE;
      }
    }
  if(itNoSDI.IsAtEnd() != itSDI.IsAtEnd())
    {
    std::cout << "Iterators don't agree on end of image" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}
