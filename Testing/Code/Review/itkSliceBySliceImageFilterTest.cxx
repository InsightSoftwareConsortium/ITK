/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSliceBySliceImageFilterTest.cxx
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

#include "itkSliceBySliceImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"

#include "itkMedianImageFilter.h"

#include "itkCommand.h"


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
  MedianType * median = dynamic_cast< MedianType * >( filter->GetInputFilter() );

  // std::cout << "callback! slice: " << filter->GetSliceIndex() << std::endl;
  
  // set half of the slice number as radius
  MedianType::InputSizeType radius;
  radius.Fill( filter->GetSliceIndex() / 2 );
  median->SetRadius( radius );
}

int itkSliceBySliceImageFilterTest(int argc, char * argv[])
{

  if( argc != 3 )
    {
    std::cerr << "usage: " << argv[0] << " input output" << std::endl;
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

  filter->SetInput( reader->GetOutput() );

  typedef itk::MedianImageFilter< FilterType::InternalInputImageType,
                                  FilterType::InternalOutputImageType > MedianType;

  MedianType::Pointer median = MedianType::New();
  filter->SetFilter( median );
   
  itk::CStyleCommand::Pointer command = itk::CStyleCommand::New();
  command->SetCallback( *sliceCallBack );
  
  filter->AddObserver( itk::IterationEvent(), command );

  itk::SimpleFilterWatcher watcher(filter, "filter");

  typedef itk::ImageFileWriter< ImageType > WriterType;

  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( filter->GetOutput() );
  writer->SetFileName( argv[2] );
    
  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

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

  return EXIT_SUCCESS;
}
