/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKappaSigmaThresholdImageFilterTest.cxx
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

#include "itkKappaSigmaThresholdImageFilter.h"

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkFilterWatcher.h"

int itkKappaSigmaThresholdImageFilterTest(int argc, char* argv[] )
{

  if( argc < 5 )
    {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImageFile outputImageFile iterations sigmaFactor";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  typedef  unsigned char  InputPixelType;
  typedef  unsigned char  MaskPixelType;
  typedef  unsigned char  OutputPixelType;

  const unsigned int Dimension = 2;

  typedef itk::Image< InputPixelType,  Dimension >   InputImageType;
  typedef itk::Image< MaskPixelType,   Dimension >   MaskImageType;
  typedef itk::Image< OutputPixelType, Dimension >   OutputImageType;

  typedef itk::KappaSigmaThresholdImageFilter<
    InputImageType, MaskImageType, OutputImageType >  FilterType;

  typedef itk::ImageFileReader< InputImageType >  ReaderType;

  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  FilterType::Pointer filter = FilterType::New();
  WriterType::Pointer writer = WriterType::New();

  FilterWatcher watcher(filter);

  reader->SetFileName( argv[1] );
  filter->SetInput( reader->GetOutput() );
  writer->SetInput( filter->GetOutput() );

  filter->SetOutsideValue( 0 );
  filter->SetInsideValue( 255 );
  filter->SetMaskValue( 255 );
  filter->SetSigmaFactor( atof( argv[3] ) );
  filter->SetNumberOfIterations( atoi( argv[4] ) );

  filter->Print( std::cout );

  std::cout << " GetOutsideValue()       = " << filter->GetOutsideValue() << std::endl;
  std::cout << " GetInsideValue()        = " << filter->GetInsideValue() << std::endl;
  std::cout << " GetMaskValue()          = " << filter->GetMaskValue() << std::endl;
  std::cout << " GetSigmaFactor()        = " << filter->GetSigmaFactor() << std::endl;
  std::cout << " GetNumberOfIterations() = " << filter->GetNumberOfIterations() << std::endl;

  filter->Update();

  std::cout << "Computed Threshold is: " << filter->GetThreshold() << std::endl;

  writer->SetFileName( argv[2] );
  writer->Update();

  return EXIT_SUCCESS;
}
