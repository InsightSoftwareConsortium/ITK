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

  if(ac < 6)
    {
    std::cerr << "Usage: " << av[0] << " InputInitialImage InputColorImage BaselineImage threshold curvatureScaling\n";
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

  // Mean values hand coded for the VisibleWomanSlice.png color file
  typedef FilterType::MeanVectorType  MeanVectorType; 
  MeanVectorType  mean = MeanVectorType(3); 
  
  mean[0] = 44.7841;
  mean[1] = 37.5714;
  mean[2] = 29.5385;

  filter->SetMean( mean );

  // Covariance values hand coded for the VisibleWomanSlice.png color file
  typedef FilterType::CovarianceMatrixType  CovarianceMatrixType; 
  CovarianceMatrixType  covariance = CovarianceMatrixType( 3, 3 ); 

  covariance[0][0] = 81.2391;
  covariance[1][1] = 82.3301;
  covariance[2][2] = 51.9178;
  covariance[0][1] = 74.0973;
  covariance[0][2] = 59.0182;
  covariance[1][2] = 62.9698;
  covariance[1][0] = covariance[0][1];
  covariance[2][0] = covariance[0][2];
  covariance[2][1] = covariance[1][2];

  filter->SetCovariance(  covariance );

  const double threshold = atof( av[4] );
  
  filter->SetThreshold( threshold );

  const double curvatureScaling = atof( av[5] );
  
  filter->SetCurvatureScaling( curvatureScaling );

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
  
  rescaler->SetOutputMinimum(   0 );
  rescaler->SetOutputMaximum( 255 );
  
  // Generate test image
  typedef itk::ImageFileWriter< WriteImageType >   WriterType;
  WriterType::Pointer writer = WriterType::New();
    
  writer->SetInput( rescaler->GetOutput() );
  writer->SetFileName( av[3] );
  writer->Update();

  std::cout << "Test PASSED !" << std::endl;

  return EXIT_SUCCESS;
}
