/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkHistogramMatchingImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkHistogramMatchingImageFilter.h"
#include "itkImage.h"
#include "itkImageRegionIterator.h"
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


int itkHistogramMatchingImageFilterTest(int, char* [] )
{

  typedef float PixelType;
  enum {ImageDimension = 3};
  typedef itk::Image<PixelType,ImageDimension> ImageType;
  typedef itk::ImageRegionIterator<ImageType> Iterator;

  ImageType::SizeType size;
  size[0] = 30;
  size[1] = 20;
  size[2] = 2;

  ImageType::RegionType region;
  region.SetSize( size );
  
  ImageType::Pointer reference = ImageType::New();
  ImageType::Pointer source = ImageType::New();

  reference->SetLargestPossibleRegion( region );
  reference->SetBufferedRegion( region );
  reference->Allocate();

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
  FilterType::Pointer filter = FilterType::New();

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
    if ( vnl_math_abs( diff ) > 1 )
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
