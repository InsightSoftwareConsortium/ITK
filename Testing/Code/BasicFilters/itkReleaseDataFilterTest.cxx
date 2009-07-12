/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkReleaseDataFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif
#include "itkImage.h"
#include "itkRandomImageSource.h"
#include "itkMeanImageFilter.h"
#include "itkTextOutput.h"
#include "itkImageRegionIterator.h"
#include "itkShiftScaleImageFilter.h"
#include "itkShrinkImageFilter.h"
#include "itkStreamingImageFilter.h"

#include "../IO/itkPipelineMonitorImageFilter.h"

int itkReleaseDataFilterTest(int, char* [] )
{
  // Comment the following if you want to use the itk text output window
  itk::OutputWindow::SetInstance(itk::TextOutput::New());


  typedef itk::Image<float,2> ImageType;
  typedef itk::PipelineMonitorImageFilter<ImageType> MonitorFilter;
  
  
  // use all the static GlobalReleaseData methods
  ImageType::SetGlobalReleaseDataFlag( ImageType::GetGlobalReleaseDataFlag() );
  ImageType::GlobalReleaseDataFlagOff();
  ImageType::GlobalReleaseDataFlagOn();
  
  typedef itk::RandomImageSource<ImageType> RandomImageSourceType;
  RandomImageSourceType::Pointer random = RandomImageSourceType::New();
  random->SetMin(0.0);
  random->SetMax(1000.0);


  ImageType::SizeValueType randomSize[2];

  randomSize[0] = randomSize[1] = 16;

  random->SetSize(randomSize);
  
  ImageType::SpacingValueType spacing[2] = {0.7, 2.1};
  random->SetSpacing( spacing );

  ImageType::PointValueType origin[2] = {15, 400};
  random->SetOrigin( origin );
  
  MonitorFilter::Pointer monitor1 = MonitorFilter::New();
  monitor1->SetInput( random->GetOutput() );


  // pipeline a

  // Create a mean image
  typedef itk::MeanImageFilter<ImageType, ImageType> MeanImageFilterType;
  MeanImageFilterType::Pointer mean1 = MeanImageFilterType::New();
  mean1->SetInput( monitor1->GetOutput() );

  // define the neighborhood size used for the mean filter 
  ImageType::SizeType neighRadius;
  neighRadius.Fill(2);
  mean1->SetRadius( neighRadius );
 
  MonitorFilter::Pointer monitor2a = MonitorFilter::New();
  monitor2a->SetInput( mean1->GetOutput() );


  // pipeline b
  typedef itk::ShiftScaleImageFilter<ImageType, ImageType> ShiftScaleImageFilterType;
  ShiftScaleImageFilterType::Pointer shiftscale = ShiftScaleImageFilterType::New();
  shiftscale->SetInput( monitor1->GetOutput() );
  shiftscale->SetScale( 2.0 );
  shiftscale->SetShift( -100.0 );
  
  typedef itk::ShrinkImageFilter<ImageType, ImageType> ShrinkImageFilterType;
  ShrinkImageFilterType::Pointer shrinker = ShrinkImageFilterType::New();
  shrinker->SetInput( shiftscale->GetOutput() );
  shrinker->SetShrinkFactors( 2 );
    
  MonitorFilter::Pointer monitor2b = MonitorFilter::New();
  monitor2b->SetInput( shrinker->GetOutput() );

  typedef itk::StreamingImageFilter<ImageType, ImageType> StreamingImageFilterType;
  StreamingImageFilterType::Pointer streamer = StreamingImageFilterType::New();
  streamer->SetInput( monitor2b->GetOutput() );
  streamer->SetNumberOfStreamDivisions( 4 );



  ImageType::SizeType zeroSize;
  zeroSize.Fill(0);


  std::cout << "---- Updating \"a\" Pipeline ---" << std::endl;
  monitor2a->Update();
  if ( !monitor1->VerifyAllInputCanStream(1) || 
       !monitor2a->VerifyAllInputCanStream(1) || 
       !monitor2b->VerifyAllNoUpdate() )
    {
    std::cout << "Monitor1:\n" << monitor1 << std::endl;
    std::cout << "Monitor2a:\n" << monitor2a << std::endl;    
    std::cout << "Monitor2b:\n" << monitor2b << std::endl;
    std::cout << "Monitor's VerifyAllInputCanStream failed!" << std::endl;
    return EXIT_FAILURE;
    }
  if ( random->GetOutput()->GetBufferedRegion().GetSize() != zeroSize ||
       monitor1->GetOutput()->GetBufferedRegion().GetSize() != zeroSize)
    {
    std::cout << "Random's output was not release!" << std::endl;
    return EXIT_FAILURE;
    }


  // no updates should happen
  std::cout << "---- Reupdating \"a\" Pipeline ---" << std::endl;
  monitor2a->Update();
  if ( !monitor1->VerifyAllNoUpdate() || 
       !monitor2a->VerifyAllNoUpdate() || 
       !monitor2b->VerifyAllNoUpdate() )
    {    
    std::cout << "monitor1:\n" << monitor1 << std::endl;
    std::cout << "monitor2a:\n" << monitor2a << std::endl;
    std::cout << "Monitor2b:\n" << monitor2b << std::endl;
    std::cout << "Monitor's VerifyAllNoUpdate failed!" << std::endl;
    return EXIT_FAILURE;
    }
  if ( random->GetOutput()->GetBufferedRegion().GetSize() != zeroSize )
    {
    std::cout << "Random's output was not release!" << std::endl;
    return EXIT_FAILURE;
    }
  monitor2a->ClearPipelineSavedInformation();


  std::cout << "---- Streaming \"b\" Pipeline ---" << std::endl;
  streamer->Update();
  if ( !monitor1->VerifyAllInputCanStream(4) || 
       !monitor2a->VerifyAllNoUpdate() || 
       !monitor2b->VerifyAllInputCanStream(4) )
    {    
    std::cout << "monitor1:\n" << monitor1 << std::endl;
    std::cout << "monitor2a:\n" << monitor2a << std::endl;
    std::cout << "Monitor2b:\n" << monitor2b << std::endl;
    std::cout << "Monitor's VerifyAllNoUpdate failed!" << std::endl;
    return EXIT_FAILURE;
    }
  if ( random->GetOutput()->GetBufferedRegion().GetSize() != zeroSize ||
       shiftscale->GetOutput()->GetBufferedRegion().GetSize() != zeroSize ||
       shrinker->GetOutput()->GetBufferedRegion().GetSize() != zeroSize )
    {
    std::cout << "random or shiftscale or shrink's output was not release!" << std::endl;
    return EXIT_FAILURE;
    }


  return EXIT_SUCCESS;
} 
