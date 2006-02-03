/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkModulusImageFilterTest.cxx
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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkModulusImageFilter.h"
#include "itkDanielssonDistanceMapImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkNumericTraits.h"

int itkModulusImageFilterTest(int argc, char * argv[])
{
  if( argc < 3 )
  {
    std::cerr << "Missing Arguments" << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputImage outputImage " << std::endl;
    return EXIT_FAILURE;
  }

  const int dim = 2;
  
  typedef unsigned char PType;
  typedef itk::Image< PType, dim > IType;

  typedef itk::ImageFileReader< IType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  // get the distance map inside the spots
  // spot are already black so there is no need to invert the image
  typedef itk::DanielssonDistanceMapImageFilter< IType, IType > DistanceFilter;
  DistanceFilter::Pointer distance = DistanceFilter::New();
  distance->SetInput( reader->GetOutput() );

  typedef itk::ModulusImageFilter< IType, IType > FilterType;
  FilterType::Pointer filter = FilterType::New();
  filter->SetInput( distance->GetOutput() );
  filter->SetDividend( 8 );
  filter->InPlaceOn();
  filter->SetFunctor( filter->GetFunctor() );

  itk::SimpleFilterWatcher watcher(filter);

  typedef itk::RescaleIntensityImageFilter< IType, IType > ThresholdType;
  ThresholdType::Pointer rescale = ThresholdType::New();
  rescale->SetInput( filter->GetOutput() );
  rescale->SetOutputMaximum( itk::NumericTraits< PType >::max() );
  rescale->SetOutputMinimum( itk::NumericTraits< PType >::NonpositiveMin() );

  typedef itk::ImageFileWriter< IType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( rescale->GetOutput() );
  writer->SetFileName( argv[2] );
  writer->Update();

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception caught ! " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

