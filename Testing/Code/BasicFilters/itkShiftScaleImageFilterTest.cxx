/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShiftScaleImageFilterTest.cxx
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

#include "itkImage.h"
#include "itkImageRegionIterator.h"

#include "itkShiftScaleImageFilter.h"
#include "itkRandomImageSource.h"
#include "itkCommand.h"

// The following three classes are used to support callbacks
// on the filter in the pipeline that follows later
class WatchFilter
{
public:
  WatchFilter(itk::ProcessObject* o)
    {m_Process = o;}
  void ShowProgress()
    {std::cout << "Progress " << m_Process->GetProgress() << std::endl;}
  void StartFilter()
    {std::cout << "-------- Start Filter " << m_Process->GetNameOfClass() << m_Process;}
  void EndFilter()
    {std::cout << "-------- End Filter " << m_Process->GetNameOfClass() << m_Process;}
  itk::ProcessObject::Pointer m_Process;
};

int main()
{
  std::cout << "itkShiftScaleImageFilterTest Start" << std::endl;

  int status = 0;
  typedef itk::Image<char,3> TestInputImage;
  typedef itk::Image<unsigned char,3> TestOutputImage;

  TestInputImage::Pointer    inputImage  = TestInputImage::New();
  TestInputImage::RegionType region;
  TestInputImage::SizeType   size; size.Fill(64);
  TestInputImage::IndexType  index; index.Fill(0);

  region.SetIndex (index);
  region.SetSize (size);

  // first try a constant image
  double fillValue = -100.0;
  inputImage->SetRegions( region );
  inputImage->Allocate();
  inputImage->FillBuffer(fillValue);

  typedef itk::ShiftScaleImageFilter<TestInputImage,TestOutputImage> FilterType;
  FilterType::Pointer filter = FilterType::New();

  // Set up Start, End and Progress callbacks
  itk::SimpleMemberCommand<WatchFilter>::Pointer startFilterCommand;
  itk::SimpleMemberCommand<WatchFilter>::Pointer endFilterCommand;
  itk::SimpleMemberCommand<WatchFilter>::Pointer progressFilterCommand;
  
  startFilterCommand =    itk::SimpleMemberCommand<WatchFilter>::New();
  endFilterCommand =      itk::SimpleMemberCommand<WatchFilter>::New();
  progressFilterCommand = itk::SimpleMemberCommand<WatchFilter>::New();

  WatchFilter filterWatch(filter);
  startFilterCommand->SetCallbackFunction(&filterWatch,
                                    &WatchFilter::StartFilter);
  endFilterCommand->SetCallbackFunction(&filterWatch,
                                  &WatchFilter::EndFilter);
  progressFilterCommand->SetCallbackFunction(&filterWatch,
                                       &WatchFilter::ShowProgress);
  filter->AddObserver(itk::StartEvent(), startFilterCommand);
  filter->AddObserver(itk::EndEvent(), endFilterCommand);
  filter->AddObserver(itk::ProgressEvent(), progressFilterCommand);

  // Filter the image
  filter->SetInput (inputImage);
  filter->UpdateLargestPossibleRegion();

  // Now generate a real image

  typedef itk::RandomImageSource<TestInputImage> SourceType;
  SourceType::Pointer source = SourceType::New();
  unsigned long randomSize[3] = {17, 8, 20};

#if 0
  // Set up Start, End and Progress callbacks
  itk::SimpleMemberCommand<WatchFilter>::Pointer startSourceCommand;
  itk::SimpleMemberCommand<WatchFilter>::Pointer endSourceCommand;
  itk::SimpleMemberCommand<WatchFilter>::Pointer progressSourceCommand;
  
  startSourceCommand =    itk::SimpleMemberCommand<WatchFilter>::New();
  endSourceCommand =      itk::SimpleMemberCommand<WatchFilter>::New();
  progressSourceCommand = itk::SimpleMemberCommand<WatchFilter>::New();

  WatchFilter sourceWatch(source);
  startSourceCommand->SetCallbackFunction(&sourceWatch,
                                    &WatchFilter::StartFilter);
  endSourceCommand->SetCallbackFunction(&sourceWatch,
                                  &WatchFilter::EndFilter);
  progressSourceCommand->SetCallbackFunction(&sourceWatch,
                                       &WatchFilter::ShowProgress);
  source->AddObserver(itk::StartEvent(), startSourceCommand);
  source->AddObserver(itk::EndEvent(), endSourceCommand);
  source->AddObserver(itk::ProgressEvent(), progressSourceCommand);
#endif
  
  source->SetSize(randomSize);
  double minValue = -128.0;
  double maxValue = 127.0;

  source->SetMin(minValue);
  source->SetMax(maxValue);
  std::cout << source;
  
  filter->SetInput(source->GetOutput());
  filter->SetScale(4.0);
  try
    {
    filter->UpdateLargestPossibleRegion();
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception detected: "  << e;
    return -1;
    }
  
  return 0;
}
