/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkValuedRegionalMinimaImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// a test routine for regional extrema using flooding
#include "itkValuedRegionalMinimaImageFilter.h"
#include "itkMaximumImageFilter.h"
#include "itkHConcaveImageFilter.h"
#include "itkInvertIntensityImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkCommand.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkAndImageFilter.h"
#include "itkCommand.h"
#include "itkSimpleFilterWatcher.h"



int itkValuedRegionalMinimaImageFilterTest(int argc, char * argv[])
{
  const int dim = 2;
  
  if( argc < 5 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " InputImage  OutputImageFile1 OutputImageFile2  " 
              << "OutputImageFile3" << std::endl;
    return EXIT_FAILURE;
    }
 
  typedef unsigned char PType;
  typedef itk::Image< PType, dim > IType;

  typedef itk::ImageFileReader< IType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[2] );

  typedef itk::ValuedRegionalMinimaImageFilter< IType, IType > FilterType;
  FilterType::Pointer filter = FilterType::New();
  filter->SetInput( reader->GetOutput() );
  filter->SetFullyConnected( atoi(argv[1]) );
  itk::SimpleFilterWatcher watcher(filter, "filter");

  typedef itk::ImageFileWriter< IType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( filter->GetOutput() );
  writer->SetFileName( argv[3] );
  writer->Update();

  // produce the same output with other filters
  typedef itk::HConcaveImageFilter< IType, IType > ConcaveType;
  ConcaveType::Pointer concave = ConcaveType::New();
  concave->SetInput( reader->GetOutput() );
  concave->SetFullyConnected( atoi(argv[1]) );
  concave->SetHeight( 1 );

  // concave gives minima with value=1 and others with value=0
  // rescale the image so we have minima=255 other=0
  typedef itk::RescaleIntensityImageFilter< IType, IType > RescaleType;
  RescaleType::Pointer rescale = RescaleType::New();
  rescale->SetInput( concave->GetOutput() );
  rescale->SetOutputMaximum( 255 );
  rescale->SetOutputMinimum( 0 );

  // in the input image, select the values of the pixel at the minima
  typedef itk::AndImageFilter< IType, IType, IType > AndType;
  AndType::Pointer a = AndType::New();
  a->SetInput(0, rescale->GetOutput() );
  a->SetInput(1, reader->GetOutput() );

  // all pixel which are not minima must have value=255.
  // get the non minima pixel by inverting the rescaled image
  // we will have minima value=0 and non minima value=255
  typedef itk::InvertIntensityImageFilter< IType, IType > InvertType;
  InvertType::Pointer invert = InvertType::New();
  invert->SetInput( rescale->GetOutput() );

  // get the highest value from "a" and from invert. The minima have
  // value>=0 in "a" image and the non minima have a value=0. In invert,
  // the non minima have a value=255 and the minima a value=0
  typedef itk::MaximumImageFilter< IType, IType, IType > MaxType;
  MaxType::Pointer max = MaxType::New();
  max->SetInput(0, invert->GetOutput() );
  max->SetInput(1, a->GetOutput() );

  WriterType::Pointer writer2 = WriterType::New();
  writer2->SetInput( max->GetOutput() );
  writer2->SetFileName( argv[4] );
  writer2->Update();

  return EXIT_SUCCESS;
}

