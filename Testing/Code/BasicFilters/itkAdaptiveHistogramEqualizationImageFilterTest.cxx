/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAdaptiveHistogramEqualizationImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkPlaheImageFilter.h"

int itkAdaptiveHistogramEqualizationImageFilterTest( int argc, char * argv[] )

{
  if( argc < 6 ) 
    { 
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  outputImageFile radius alpha beta" << std::endl;
    return 1;
    }

  
  typedef    float    InputPixelType;
  typedef    float    OutputPixelType;
  static const int ImageDimension=2;
  
  typedef itk::Image< InputPixelType,  ImageDimension >   InputImageType;
  typedef itk::Image< OutputPixelType, ImageDimension >   OutputImageType;
  typedef itk::ImageFileReader< InputImageType >  ReaderType;
  typedef itk::PlaheImageFilter<
               InputImageType >  FilterType;

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  FilterType::ImageSizeType radius;
  radius.Fill( atoi(argv[3]) );

  FilterType::Pointer filter = FilterType::New();
  filter->SetInput( reader->GetOutput() );
  filter->SetRadius( radius );
  filter->SetAlpha( atof(argv[4]) );
  filter->SetBeta( atof(argv[5]) );

  //
  //  The output of the filter is connected here to a intensity rescaler filter
  //  and then to a writer. Invoking \code{Update()} on the writer triggers the
  //  execution of both filters.
  //

  typedef unsigned char WritePixelType;

  typedef itk::Image< WritePixelType, 2 > WriteImageType;

  typedef itk::RescaleIntensityImageFilter< 
               InputImageType, WriteImageType > RescaleFilterType;

  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();
  rescaler->SetOutputMinimum(   0 );
  rescaler->SetOutputMaximum( 255 );
  rescaler->SetInput( filter->GetOutput() );
  

  typedef itk::ImageFileWriter< WriteImageType >  WriterType;

  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[2] );
 
  writer->SetInput( rescaler->GetOutput() );
  writer->Update();

  return 0;

}

