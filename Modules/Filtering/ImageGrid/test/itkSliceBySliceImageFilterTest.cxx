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

#include "itkSliceBySliceImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkPipelineMonitorImageFilter.h"
#include "itkTestingMacros.h"
#include "itkMedianImageFilter.h"


void sliceCallBack(itk::Object* object, const itk::EventObject &, void*)
{
  // the same typedefs than in the main function - should be done in a nicer way
  const int                 Dimension = 3;
  typedef unsigned char     PixelType;

  typedef itk::Image< PixelType, Dimension >                   ImageType;
  typedef itk::SliceBySliceImageFilter< ImageType, ImageType > FilterType;
  typedef itk::MedianImageFilter< FilterType::InternalInputImageType,
    FilterType::InternalOutputImageType >                      MedianType;

  // real stuff begins here
  // get the slice by slice filter and the median filter
  FilterType * filter = dynamic_cast< FilterType * >( object );
  MedianType * median = dynamic_cast< MedianType * >( filter->GetModifiableInputFilter() );

  // std::cout << "callback! slice: " << filter->GetSliceIndex() << std::endl;

  // set half of the slice number as radius
  MedianType::InputSizeType radius;
  radius.Fill( filter->GetSliceIndex() / 2 );
  median->SetRadius( radius );
}

int itkSliceBySliceImageFilterTest(int argc, char * argv[])
{

  if( argc != 4 )
    {
    std::cerr << "usage: " << argv[0] << " input output slicingDimension" << std::endl;
    return EXIT_FAILURE;
    }

  const int                 Dimension = 3;
  typedef unsigned char     PixelType;

  typedef itk::Image< PixelType, Dimension >      ImageType;

  typedef itk::ImageFileReader< ImageType >       ReaderType;

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  typedef itk::SliceBySliceImageFilter< ImageType, ImageType > FilterType;

  FilterType::Pointer filter = FilterType::New();
  filter->DebugOn();

  filter->SetInput( reader->GetOutput() );

  typedef itk::MedianImageFilter< FilterType::InternalInputImageType,
                                  FilterType::InternalOutputImageType > MedianType;

  MedianType::Pointer median = MedianType::New();
  filter->SetFilter( median );

  typedef itk::PipelineMonitorImageFilter<FilterType::InternalOutputImageType> MonitorType;
  MonitorType::Pointer monitor = MonitorType::New();

  itk::CStyleCommand::Pointer command = itk::CStyleCommand::New();
  command->SetCallback( *sliceCallBack );

  filter->AddObserver( itk::IterationEvent(), command );

  itk::SimpleFilterWatcher watcher(filter, "filter");

  typedef itk::ImageFileWriter< ImageType > WriterType;

  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( filter->GetOutput() );
  writer->SetFileName( argv[2] );

  unsigned int slicingDimension;
  std::istringstream istrm( argv[3] );
  istrm >> slicingDimension;
  filter->SetDimension( slicingDimension );
  std::cout << "Slicing dimension: " << slicingDimension << std::endl;
  std::cout << "Slicing dimension: " << filter->GetDimension() << std::endl;

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }


  // set up a requested region of just one pixel and verify that was
  // all that was produced.
  std::cout << "Testing with requested region..." << std::endl;
  ImageType::Pointer temp = filter->GetOutput();
  temp->DisconnectPipeline();
  temp = ITK_NULLPTR;

  ImageType::RegionType rr = reader->GetOutput()->GetLargestPossibleRegion();
  for (unsigned int i = 0; i < ImageType::ImageDimension; ++i)
    {
    rr.SetIndex(i, rr.GetIndex(i)+rr.GetSize(i)/2);
    rr.SetSize(i,1);
    }


  monitor->SetInput(median->GetOutput());
  filter->SetOutputFilter(monitor);
  filter->GetOutput()->SetRequestedRegion(rr);


  try
    {
    filter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  // check that one slice executed is just one pixel and the input
  // filter just update that region
  TEST_EXPECT_EQUAL( monitor->GetNumberOfUpdates(), 1 );
  TEST_EXPECT_EQUAL( monitor->GetOutputRequestedRegions()[0].GetNumberOfPixels(), 1 );
  TEST_EXPECT_TRUE( monitor->VerifyAllInputCanStream(1) );

  //
  // Test that a sliced version of the input image information is passed
  // through to the internal filters, with proper origin and
  // spacing. We are setting the input image to have a non-zero
  // starting index.
  //
  ImageType::Pointer image = ImageType::New();
  {
  ImageType::RegionType region = reader->GetOutput()->GetLargestPossibleRegion();
  region.SetIndex(0,10);
  image->SetRegions(region);
  image->Allocate(true);
  }

  ImageType::SpacingType spacing;
  ImageType::PointType origin;
  for ( unsigned int i = 0; i < ImageType::ImageDimension; ++i )
    {
    spacing[i] = i + 0.1;
    origin[i] = i + 0.2;
    }
  image->SetSpacing(spacing);
  image->SetOrigin(origin);

  filter->SetInput(image);
  filter->Update();

  FilterType::InternalInputImageType::SpacingType expectedInternalSpacing;
  FilterType::InternalInputImageType::PointType expectedInternalOrigin;
  for ( unsigned int i = 0, internal_i = 0; internal_i < FilterType::InternalImageDimension; ++i, ++internal_i )
    {
    if ( i == slicingDimension )
      {
      ++i;
      }

    expectedInternalSpacing[internal_i] = spacing[i];
    expectedInternalOrigin[internal_i] = origin[i];
    }
  TEST_EXPECT_EQUAL( monitor->GetUpdatedOutputSpacing(), expectedInternalSpacing );
  TEST_EXPECT_EQUAL( monitor->GetUpdatedOutputOrigin(), expectedInternalOrigin );

  //
  // Exercise PrintSelf()
  //
  filter->Print( std::cout );

  //
  // Exercise exceptions
  //
  bool caughtException;
  FilterType::Pointer badFilter = FilterType::New();

  std::cout << "Testing with no filter set..." << std::endl;
  badFilter->SetInput( reader->GetOutput() );
  caughtException = false;
  try
    {
    badFilter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cout << "Caught expected exception" << std::endl;
    std::cout << excp << std::endl;
    caughtException = true;
    }
  if (!caughtException)
    {
    return EXIT_FAILURE;
    }

  std::cout << "Testing with no output filter set..." << std::endl;
  badFilter->SetInput( reader->GetOutput() );
  badFilter->SetInputFilter( median );
  caughtException = false;
  try
    {
    badFilter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cout << "Caught expected exception" << std::endl;
    std::cout << excp << std::endl;
    caughtException = true;
    }
  if (!caughtException)
    {
    return EXIT_FAILURE;
    }

  // check ITK_NULLPTR input/output
  TRY_EXPECT_EXCEPTION(badFilter->SetInputFilter(ITK_NULLPTR));
  TRY_EXPECT_EXCEPTION(badFilter->SetOutputFilter(ITK_NULLPTR));

  return EXIT_SUCCESS;
}
