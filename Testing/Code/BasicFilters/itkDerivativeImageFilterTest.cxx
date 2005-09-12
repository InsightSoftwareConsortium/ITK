/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDerivativeImageFilterTest.cxx
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
#include <iostream>
#include "itkImage.h"
#include "itkDerivativeImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkSimpleFilterWatcher.h"

int itkDerivativeImageFilterTest(int argc, char *argv [] )
{
  if( argc < 5 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile normalizedOutputImageFile ";
    std::cerr << " derivativeOrder direction" << std::endl;
    return EXIT_FAILURE;
    }


  // Test using an unsigned integral pixel type and generate a signed
  // integral pixel type
  typedef   unsigned short  InputPixelType;
  typedef   short  OutputPixelType;

  const unsigned int Dimension = 2;

  typedef itk::Image< InputPixelType,  Dimension >   InputImageType;
  typedef itk::Image< OutputPixelType, Dimension >   OutputImageType;


  typedef itk::ImageFileReader< InputImageType  >  ReaderType;

  ReaderType::Pointer reader = ReaderType::New();

  reader->SetFileName( argv[1] );

  // Define the filter
  typedef itk::DerivativeImageFilter<
               InputImageType, OutputImageType >  FilterType;

  FilterType::Pointer filter = FilterType::New();

  // setup the filter
  filter->SetOrder(     atoi( argv[3] ) );
  filter->SetDirection( atoi( argv[4] ) );

  itk::SimpleFilterWatcher watcher(filter, "Derivative");
  
  // wire the pipeline
  filter->SetInput( reader->GetOutput() );

  // Write the output
  typedef itk::Image< unsigned char, Dimension >  WriteImageType;

  typedef itk::RescaleIntensityImageFilter< 
                                  OutputImageType,
                                  WriteImageType >    NormalizeFilterType;

  typedef itk::ImageFileWriter< WriteImageType >       NormalizedWriterType;

  NormalizeFilterType::Pointer normalizer = NormalizeFilterType::New();
  NormalizedWriterType::Pointer normalizedWriter = NormalizedWriterType::New();

  normalizer->SetInput( filter->GetOutput() );
  normalizedWriter->SetInput( normalizer->GetOutput() );

  normalizer->SetOutputMinimum(   0 );
  normalizer->SetOutputMaximum( 255 );

  normalizedWriter->SetFileName( argv[2] );
  normalizedWriter->Update();

  return EXIT_SUCCESS;
}
