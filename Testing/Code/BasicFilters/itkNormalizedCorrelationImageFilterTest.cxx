/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNormalizedCorrelationImageFilterTest.cxx
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
#include "itkNormalizedCorrelationImageFilter.h"
#include "itkAnnulusOperator.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkNearestNeighborInterpolateImageFunction.h"
#include "itkResampleImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"

int itkNormalizedCorrelationImageFilterTest(int ac, char* av[] )
{
  if(ac < 4)
    {
    std::cerr << "Usage: " << av[0] << " InputImage MaskImage OutputImage\n";
    return -1;
    }

  const unsigned int Dimension = 2;
  typedef unsigned char PixelType;
  typedef float CorrelationPixelType;

  typedef itk::Image<PixelType, Dimension> InputImageType;
  typedef itk::Image<CorrelationPixelType, Dimension> CorrelationImageType;
  
  itk::ImageFileReader<InputImageType>::Pointer input 
    = itk::ImageFileReader<InputImageType>::New();
  input->SetFileName(av[1]);
  input->Update();
  
  // define an operator
  typedef itk::AnnulusOperator<CorrelationPixelType, Dimension> AnnulusType;
  AnnulusType annulus;
  annulus.SetInnerRadius( 5 );
  annulus.SetThickness( 2 );
  annulus.NormalizeOn();
  annulus.BrightCenterOn();
  annulus.CreateOperator();

  // create a mask
  itk::ImageFileReader<InputImageType>::Pointer mask
    = itk::ImageFileReader<InputImageType>::New();
  mask->SetFileName(av[2]);

  // resample the mask to be the size of the input
  typedef itk::NearestNeighborInterpolateImageFunction<InputImageType> InterpolatorType;
  InterpolatorType::Pointer interpolator = InterpolatorType::New();

  typedef itk::ResampleImageFilter<InputImageType, InputImageType> ResampleType;
  ResampleType::Pointer resample = ResampleType::New();
  resample->SetInput(mask->GetOutput());
  resample->SetOutputParametersFromImage( input->GetOutput() );
  
  
  // Create a filter
  typedef itk::NormalizedCorrelationImageFilter<InputImageType, InputImageType, CorrelationImageType> FilterType;
  
  FilterType::Pointer filter = FilterType::New();
  itk::SimpleFilterWatcher watcher(filter, "Normalized correlation");

  filter->SetInput(input->GetOutput());
  filter->SetTemplate( annulus );
  filter->SetMaskImage( resample->GetOutput() );

  
  typedef itk::BinaryThresholdImageFilter<CorrelationImageType, InputImageType> ThresholdType;
  ThresholdType::Pointer threshold = ThresholdType::New();
  threshold->SetInput( filter->GetOutput() );
  threshold->SetLowerThreshold( 0.1 );
  threshold->SetUpperThreshold( 1.0 );
  threshold->SetInsideValue( 255 );
  threshold->SetOutsideValue( 0 );
  
  // Generate test image
  itk::ImageFileWriter<InputImageType>::Pointer writer;
  writer = itk::ImageFileWriter<InputImageType>::New();
  writer->SetInput( threshold->GetOutput() );
  writer->SetFileName( av[3] );

  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception detected: "  << e.GetDescription();
    return -1;
    }
  catch (...)
    {
    std::cerr << "Some other exception occurred" << std::endl;
    return -2;
    }

  return 0;
}
