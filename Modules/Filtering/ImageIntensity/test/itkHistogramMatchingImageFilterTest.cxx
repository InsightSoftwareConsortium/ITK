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

#include "itkHistogramMatchingImageFilter.h"
#include "itkCommand.h"

/**
 * This file tests the functionality of the HistogramMatchingImageFilter.
 * This test uses artificial data, where we multiply different intensity
 * classes by different factors and test whether we can recover the
 * reference image.
 */

double refPattern( unsigned long offset )
{
  if ( offset < 40  ) { return 5.0;  }
  if ( offset < 160 ) { return 10.0; }
  if ( offset < 200 ) { return 15.0; }
  if ( offset < 320 ) { return 20.0; }
  return 0.0;
}

double srcPattern( unsigned long offset )
{
  if ( offset < 40  ) { return 5.0  * 1.5; }
  if ( offset < 160 ) { return 10.0 * 0.9; }
  if ( offset < 200 ) { return 15.0 * 1.0; }
  if ( offset < 320 ) { return 20.0 * 0.8; }
  return 0.0;
}
namespace
{

// The following classe is used to support callbacks
// on the filter in the pipeline that follows later
class ShowProgressObject
{
public:
  ShowProgressObject(itk::ProcessObject* o)
    {m_Process = o;}
  void ShowProgress()
    {std::cout << "Progress " << m_Process->GetProgress() << std::endl;}
  itk::ProcessObject::Pointer m_Process;
};
}


template <typename TScalar>
int itkHistogramMatchingImageFilterTest()
{

  typedef TScalar PixelType;
  enum {ImageDimension = 3};
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::ImageRegionIterator<ImageType>  Iterator;

  typename ImageType::SizeType size;
  size[0] = 30;
  size[1] = 20;
  size[2] = 2;

  typename ImageType::RegionType region;
  region.SetSize( size );

  typename ImageType::Pointer reference = ImageType::New();
  typename ImageType::Pointer source = ImageType::New();

  reference->SetLargestPossibleRegion( region );
  reference->SetBufferedRegion( region );
  reference->Allocate();

  // Change the origin of the reference image.
  typename ImageType::PointType origin;
  origin[0] = 1.0;
  origin[1] = 10.0;
  origin[2] = 100.0;
  reference->SetOrigin(origin);

  source->SetLargestPossibleRegion( region );
  source->SetBufferedRegion( region );
  source->Allocate();

  Iterator refIter( reference, region );
  Iterator srcIter( source, region );

  unsigned long counter = 0;

  while ( !refIter.IsAtEnd() )
    {
    refIter.Set( static_cast<PixelType>( refPattern( counter ) ) );
    srcIter.Set( static_cast<PixelType>( srcPattern( counter ) ) );

    ++refIter;
    ++srcIter;
    ++counter;
    }


  typedef itk::HistogramMatchingImageFilter<ImageType,ImageType> FilterType;
  typename FilterType::Pointer filter = FilterType::New();

  filter->SetReferenceImage( reference );
  filter->SetSourceImage( source );
  filter->SetNumberOfHistogramLevels( 50 );
  filter->SetNumberOfMatchPoints( 8 );
  filter->ThresholdAtMeanIntensityOn();

  ShowProgressObject progressWatch(filter);
  itk::SimpleMemberCommand<ShowProgressObject>::Pointer command;
  command = itk::SimpleMemberCommand<ShowProgressObject>::New();
  command->SetCallbackFunction(&progressWatch,
                               &ShowProgressObject::ShowProgress);
  filter->AddObserver(itk::ProgressEvent(), command);

  filter->Update();
  filter->Print( std::cout );

  // Walk the output and compare with reference
  Iterator outIter( filter->GetOutput(), region );

  refIter.GoToBegin();

  bool passed = true;

  while( !outIter.IsAtEnd() )
    {
    PixelType diff = refIter.Get() - outIter.Get();
    if ( itk::Math::abs( diff ) > 1 )
      {
      passed = false;
      std::cout << "Test failed at: " << outIter.GetIndex() << " ";
      std::cout << "Output value: " << outIter.Get() << " ";
      std::cout << "Ref value: " << refIter.Get() << std::endl;
      }
    ++outIter;
    ++refIter;
    }


  // Exercise auxiliary functions
  std::cout << "Exercise auxiliary functions" << std::endl;
  std::cout << filter->GetNumberOfHistogramLevels() << std::endl;
  std::cout << filter->GetNumberOfMatchPoints() << std::endl;

  std::cout << "Source Histogram: " <<
    filter->GetSourceHistogram() << std::endl;
  std::cout << "Reference Histogram: " <<
    filter->GetReferenceHistogram() << std::endl;
  std::cout << "Output Histogram: " <<
    filter->GetOutputHistogram() << std::endl;

  std::cout << "Threshold At Mean Intensity? ";
  std::cout << filter->GetThresholdAtMeanIntensity() << std::endl;

  filter->ThresholdAtMeanIntensityOff();
  filter->Update();
  filter->Print( std::cout );

  if ( !passed )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}
int itkHistogramMatchingImageFilterTest(int, char* [] )
{
  if(itkHistogramMatchingImageFilterTest<float>() != EXIT_SUCCESS)
    {
    return EXIT_FAILURE;
    }
#if !defined(ITKV3_COMPATIBILITY)
  if(itkHistogramMatchingImageFilterTest<long>() != EXIT_SUCCESS)
    {
    return EXIT_FAILURE;
    }
#endif
  if(itkHistogramMatchingImageFilterTest<unsigned long>() != EXIT_SUCCESS)
    {
    return EXIT_FAILURE;
    }
  if(itkHistogramMatchingImageFilterTest<int>() != EXIT_SUCCESS)
    {
    return EXIT_FAILURE;
    }
  if(itkHistogramMatchingImageFilterTest<unsigned int>() != EXIT_SUCCESS)
    {
    return EXIT_FAILURE;
    }
  if(itkHistogramMatchingImageFilterTest<short>() != EXIT_SUCCESS)
    {
    return EXIT_FAILURE;
    }
  if(itkHistogramMatchingImageFilterTest<unsigned short>() != EXIT_SUCCESS)
    {
    return EXIT_FAILURE;
    }
  if(itkHistogramMatchingImageFilterTest<char>() != EXIT_SUCCESS)
    {
    return EXIT_FAILURE;
    }
  if(itkHistogramMatchingImageFilterTest<unsigned char>() != EXIT_SUCCESS)
    {
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
