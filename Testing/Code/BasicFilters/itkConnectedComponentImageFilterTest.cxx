/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConnectedComponentImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkConnectedComponentImageFilter.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionIterator.h"
#include "itkUnaryFunctorImageFilter.h"
#include "itkScalarToRGBPixelFunctor.h"
#include "itkFilterWatcher.h"


int itkConnectedComponentImageFilterTest(int argc, char* argv[] )
{
  if( argc < 5 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImage  outputImage threshold_low threshold_hi" << std::endl;
    return 1;
    }

  typedef   unsigned short  InternalPixelType;
  const     unsigned int    Dimension = 2;
  
  typedef itk::Image< InternalPixelType, Dimension >  InternalImageType;
  typedef itk::Image<unsigned short,2> OutputImageType;

  typedef itk::RGBPixel<unsigned char>   RGBPixelType;
  typedef itk::Image<RGBPixelType, 2>    RGBImageType;

  typedef itk::ImageFileReader< InternalImageType > ReaderType;
  typedef itk::ImageFileWriter<  RGBImageType  > WriterType;

  
  typedef itk::BinaryThresholdImageFilter< InternalImageType, InternalImageType > ThresholdFilterType;
  typedef itk::ConnectedComponentImageFilter< InternalImageType, OutputImageType > FilterType;
  typedef itk::Functor::ScalarToRGBPixelFunctor<unsigned short>
    ColorMapFunctorType;
  typedef itk::UnaryFunctorImageFilter<OutputImageType,
    RGBImageType, ColorMapFunctorType> ColorMapFilterType;



  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();
  ThresholdFilterType::Pointer threshold = ThresholdFilterType::New();
  FilterType::Pointer filter = FilterType::New();
  ColorMapFilterType::Pointer colormapper = ColorMapFilterType::New();

  
  FilterWatcher watcher(filter);
  watcher.QuietOn();

  reader->SetFileName( argv[1] );

  InternalPixelType threshold_low, threshold_hi;
  threshold_low = atoi( argv[3]);
  threshold_hi = atoi( argv[4]);

  threshold->SetInput (reader->GetOutput());
  threshold->SetInsideValue(itk::NumericTraits<InternalPixelType>::One);
  threshold->SetOutsideValue(itk::NumericTraits<InternalPixelType>::Zero);
  threshold->SetLowerThreshold(threshold_low);
  threshold->SetUpperThreshold(threshold_hi);
  threshold->Update();
  
  filter->SetInput (threshold->GetOutput());

  colormapper->SetInput( filter->GetOutput() );
  
  try
    {
    writer->SetInput (colormapper->GetOutput());
    writer->SetFileName( argv[2] );
    writer->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    }

  return 0;
}
