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
#include "itkVTKImageIO.h"

#include "itkImageFileWriter.h"
#include "itkImageFileReader.h"

#include "itkPipelineMonitorImageFilter.h"
#include "itkStreamingImageFilter.h"

int itkVTKImageIO2Test2(int argc, char* argv[])
{
  //
  // This test is designed to test the non-streaming capabilities of
  // VTKImageIO with tensors
  //


  if( argc < 2 )
    {
    std::cerr << "Usage: " << argv[0] << " outputFileName" << std::endl;
    return EXIT_FAILURE;
    }

  std::string outputFileName = argv[1];

  typedef itk::SymmetricSecondRankTensor<double, 3> PixelType;
  typedef itk::Image< PixelType, 3 >                ImageType;
  typedef itk::ImageFileReader< ImageType >         ReaderType;
  typedef itk::ImageFileWriter< ImageType >         WriterType;

  // write a 10^3 image

  {
  // allocate an 10x10x10 image
  ImageType::Pointer image = ImageType::New();
  ImageType::SizeType imageSize;
  imageSize.Fill(10);
  image->SetRegions( imageSize );
  image->Allocate();

  unsigned int cnt = 0;
  itk::ImageRegionIterator< ImageType > i( image, image->GetLargestPossibleRegion() );
  i.GoToBegin();
  while (! i.IsAtEnd() )
    {
    // fill the image switching between these pixels
    switch (cnt*3%5)
      {
      case 0:
        i.Set( itk::NumericTraits<PixelType>::ZeroValue() );
        break;
      case 1:
        i.Set( itk::NumericTraits<PixelType>::OneValue() );
        break;
      case 2:
        i.Set( itk::NumericTraits<PixelType>::OneValue() );
        break;
      }
    ++cnt;
    ++i;
    }

  typedef itk::VTKImageIO IOType;
  IOType::Pointer vtkIO = IOType::New();

  WriterType::Pointer writer = WriterType::New();
  writer->SetImageIO( vtkIO );
  writer->SetInput( image );
  writer->SetFileName( outputFileName.c_str() );
  writer->Update();
  }

  // check that a request to stream is not
  {
  typedef itk::VTKImageIO IOType;
  IOType::Pointer vtkIO = IOType::New();

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( outputFileName.c_str() );
  reader->SetImageIO( vtkIO );

  typedef itk::PipelineMonitorImageFilter<ImageType> MonitorFilter;
  MonitorFilter::Pointer monitor = MonitorFilter::New();
  monitor->SetInput(reader->GetOutput());
  const unsigned int numberOfDataPieces = 10;


  typedef itk::StreamingImageFilter<ImageType, ImageType> StreamingFilter;
  StreamingFilter::Pointer streamer = StreamingFilter::New();
  streamer->SetInput(monitor->GetOutput());
  streamer->SetNumberOfStreamDivisions(numberOfDataPieces);

  streamer->Update();

 bool passed = true;

 if( !monitor->VerifyAllInputCanNotStream() )
   {
   passed = false;
   }

 if( !passed )
   {
   std::cout << monitor << std::endl;
   std::cout << "pipeline did not execute as expected!" << std::endl;
   return EXIT_FAILURE;
   }
  }

  // request to stream write but is should be denied
  {

  typedef itk::VTKImageIO IOType;
  IOType::Pointer vtkIO = IOType::New();

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( outputFileName.c_str() );
  reader->SetImageIO( vtkIO );
  reader->UpdateLargestPossibleRegion();


  typedef itk::PipelineMonitorImageFilter<ImageType> MonitorFilter;
  MonitorFilter::Pointer monitor = MonitorFilter::New();
  monitor->SetInput(reader->GetOutput());
  const unsigned int numberOfDataPieces = 10;

  WriterType::Pointer writer = WriterType::New();
  writer->SetImageIO( vtkIO );
  writer->SetInput( monitor->GetOutput() );
  writer->SetNumberOfStreamDivisions( numberOfDataPieces );
  writer->SetFileName( outputFileName.c_str() );

  writer->Update();

 bool passed = true;

 if( !monitor->VerifyAllInputCanNotStream() )
   {
   passed = false;
   }

 if( !passed )
   {
   std::cout << monitor << std::endl;
   std::cout << "pipeline did not execute as expected!" << std::endl;
   return EXIT_FAILURE;
   }
  }

  return EXIT_SUCCESS;
}
