/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorThresholdSegmentationLevelSetImageFilterTest.cxx
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
#include <fstream>
#include "itkVectorThresholdSegmentationLevelSetImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTextOutput.h"
#include "itkImageRegionIterator.h"
#include "itkNumericTraits.h"
#include "itkRGBPixel.h"
#include "itkRescaleIntensityImageFilter.h"

int itkVectorThresholdSegmentationLevelSetImageFilterTest(int ac, char* av[] )
{
  // Comment the following if you want to use the itk text output window
  itk::OutputWindow::SetInstance(itk::TextOutput::New());

  if(ac < 4)
    {
    std::cerr << "Usage: " << av[0] << " InputImage BaselineImage threshold\n";
    return -1;
    }

  const unsigned int Dimension = 2;

  typedef unsigned char PixelComponentType;
  typedef itk::RGBPixel<PixelComponentType> RGBPixelType;
  typedef unsigned char InputPixelType;
  typedef float         OutputPixelType;
  typedef unsigned char WritePixelType;

  typedef itk::Image< InputPixelType,  Dimension > InputImageType;
  typedef itk::Image< RGBPixelType,    Dimension > RGBImageType;
  typedef itk::Image< OutputPixelType, Dimension > OutputImageType;
  typedef itk::Image< WritePixelType,  Dimension > WriteImageType;
  
  typedef itk::ImageFileReader< InputImageType >   InputReaderType;
  typedef itk::ImageFileReader< RGBImageType >     RGBReaderType;
    
  RGBReaderType::Pointer   rgbReader   = RGBReaderType::New();
  InputReaderType::Pointer inputReader = InputReaderType::New();

  inputReader->SetFileName(av[1]);
  rgbReader->SetFileName(av[2]);
  
  // Create a filter
  typedef itk::VectorThresholdSegmentationLevelSetImageFilter<
                                              InputImageType,
                                              RGBImageType,
                                              OutputPixelType
                                                > FilterType;

  FilterType::Pointer filter = FilterType::New();

  filter->SetInput( inputReader->GetOutput() );

  filter->SetFeatureImage( rgbReader->GetOutput() );

  FilterType::MeanVectorType        mean; 
  FilterType::CovarianceMatrixType  covariance; 

  const double threshold = atof( av[3] );
  
  filter->SetMean( mean );
  filter->SetCovariance(  covariance );
  filter->SetThreshold( threshold );

  try
    {
    rgbReader->Update();
    filter->Update();
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception detected: "  << e.GetDescription();
    return -1;
    }

  // Test the GetMacros
  if( filter->GetThreshold() != threshold )
    {
    std::cerr << "Error GetThreshold returns a value";
    std::cerr << " different from the one in SetThreshold" << std::endl;
    std::cerr << "threshold      = " << threshold << std::endl;
    std::cerr << "GetThreshold() = " << filter->GetThreshold() << std::endl;
    return EXIT_FAILURE;
    }


  typedef itk::RescaleIntensityImageFilter< OutputImageType, WriteImageType > RescalerType;
  RescalerType::Pointer rescaler = RescalerType::New();

  rescaler->SetInput( filter->GetOutput() );

  
  // Generate test image
  typedef itk::ImageFileWriter< WriteImageType >   WriterType;
  WriterType::Pointer writer = WriterType::New();
    
  writer->SetInput( rescaler->GetOutput() );
  writer->SetFileName( av[2] );
  writer->Update();


  return 0;
}
