/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShiftScaleInPlaceImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include <iostream>

#include "itkIndex.h"
#include "itkImage.h"
#include "itkImageRegionIterator.h"

#include "itkShiftScaleImageFilter.h"
#include "itkShiftScaleInPlaceImageFilter.h"
#include "itkRandomImageSource.h"

#include "itkFilterWatcher.h"

class ShiftScaleInPlaceFilterWatcher : public FilterWatcher
{
 public:
  ShiftScaleInPlaceFilterWatcher(itk::ProcessObject* o, std::string name)
    : FilterWatcher(o), m_Name( name ) {};
  
  virtual void StartFilter()
    {
    m_Start = ::clock();
    std::cout << "-------- Start " << m_Process->GetNameOfClass()
              << " (" << m_Name << ")"
              << std::endl
              << std::flush;
    }

  virtual void EndFilter()
    {
    m_End = ::clock();
    std::cout << "-------- End " << m_Process->GetNameOfClass()
              << " (" << m_Name << ")";
    std::cout << std::endl;
    }

  virtual void ShowProgress() {}

  std::string m_Name;
};


int itkShiftScaleInPlaceImageFilterTest(int, char* [] )
{
  int status = 0;
  
  typedef itk::Image<char,3> TestInputImage;
  typedef itk::Image<char,3> TestOutputImage;

  // Now generate a real image

  typedef itk::RandomImageSource<TestInputImage> SourceType;
  SourceType::Pointer source = SourceType::New();
  unsigned long randomSize[3] = {17, 8, 20};

  // Set up Start, End and Progress callbacks
  ShiftScaleInPlaceFilterWatcher sourceWatch(source, "source");

  // Set up source
  source->SetSize(randomSize);
  double minValue = -10.0;
  double maxValue = 10.0;

  source->SetMin( static_cast< TestInputImage::PixelType >( minValue ) );
  source->SetMax( static_cast< TestInputImage::PixelType >( maxValue ) );


  // Define two standard shift scale filters so we can check whether the
  // in place filtering works
  typedef itk::ShiftScaleImageFilter<TestInputImage,TestOutputImage> FilterType;
  FilterType::Pointer zeroFilter = FilterType::New();
  ShiftScaleInPlaceFilterWatcher zeroFilterWatch(zeroFilter, "zeroFilter");
  zeroFilter->SetInput(source->GetOutput());
  zeroFilter->SetScale(0.0);

  FilterType::Pointer filter = FilterType::New();
  ShiftScaleInPlaceFilterWatcher filterWatch(filter, "filter");
  filter->SetInput(zeroFilter->GetOutput());
  filter->SetShift(20);
  
  
  // Define two consumers of the shift scale filter, one another shift scale
  // and the other an in place shift scale
  typedef itk::ShiftScaleInPlaceImageFilter<TestInputImage> InPlaceFilterType;
  InPlaceFilterType::Pointer inPlaceFilter = InPlaceFilterType::New();
  ShiftScaleInPlaceFilterWatcher inPlaceWatch(inPlaceFilter, "inPlaceFilter");
  inPlaceFilter->SetInput( filter->GetOutput() );
  inPlaceFilter->SetShift( 100 );

  FilterType::Pointer secondFilter = FilterType::New();
  ShiftScaleInPlaceFilterWatcher secondFilterWatch( secondFilter, "secondFilter" );
  secondFilter->SetInput( filter->GetOutput() );
  secondFilter->SetShift( 50 );
  

  // Test itkSetMacros and itkGetMacros
  inPlaceFilter->GetShift();
  //SetScale();
  inPlaceFilter->GetScale();
  long value = inPlaceFilter->GetUnderflowCount();
  long value2 = inPlaceFilter-> GetOverflowCount();

  try
    {
    // update the in place filter
    std::cout << "=========== Updating the in place filter. ==============="
              << std::endl;
    inPlaceFilter->UpdateLargestPossibleRegion();
    std::cout << std::endl << std::endl;
    std::cout << "=========== Updating the second filter. This should cause the first filter to re-execute ==============="
              << std::endl;
    secondFilter->UpdateLargestPossibleRegion();
    std::cout << std::endl << std::endl;

    // check the images
    itk::Index<3> index;
    index.Fill( 5 );

    if (filter->GetOutput()->GetPixel(index) != 20)
      {
      std::cout << "Filter pixel value = " << (int) filter->GetOutput()->GetPixel(index)
                << std::endl;
      status |= 0x01;
      }
    if (inPlaceFilter->GetOutput()->GetPixel(index) != 120)
      {
      std::cout << "In place filter pixel value = " << (int) inPlaceFilter->GetOutput()->GetPixel(index)
                << std::endl;
      status |= 0x02;
      }
    if (secondFilter->GetOutput()->GetPixel(index) != 70)
      {
      std::cout << "Second filter pixel value = " << (int) secondFilter->GetOutput()->GetPixel(index)
                << std::endl;
      status |= 0x04;
      }
    
    std::cout << "=========== Updating the in place filter again. This should only update the in place filter.  ==============="
              << std::endl;
    inPlaceFilter->UpdateLargestPossibleRegion();
    std::cout << std::endl << std::endl;

    if (filter->GetOutput()->GetPixelContainer()->Size() != 0)
      {
      std::cout << "Filter still has an input!" << std::endl;
      status |= 0x08;
      }
    if (inPlaceFilter->GetOutput()->GetPixel(index) != 120)
      {
      std::cout << "In place filter pixel value = " << (int) inPlaceFilter->GetOutput()->GetPixel(index)
                << std::endl;
      status |= 0x10;
      }
    if (secondFilter->GetOutput()->GetPixel(index) != 70)
      {
      std::cout << "Second filter pixel value = " << (int) secondFilter->GetOutput()->GetPixel(index)
                << std::endl;
      status |= 0x20;
      }
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception detected: "  << e;
    return -1;
    }

  std::cout << "Status = " << status << std::endl;
  return status;
}
