/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkSampleToHistogramFilterTest5.cxx
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

#include "itkRGBPixel.h"
#include "itkListSample.h"
#include "itkHistogram.h"
#include "itkSampleToHistogramFilter.h"
#include "itkImageToListSampleFilter.h"
#include "itkImageFileReader.h"

int itkSampleToHistogramFilterTest5(int argc, char *argv[] )
{

  const unsigned int imageDimension = 2;

  if( argc < 2 )
    {
    std::cerr << "Missing arguments" << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFilename " << std::endl;
    return EXIT_FAILURE;
    }

  //
  // Note:
  //
  // The purpose of this test is to verify that the
  // SampleToHistogramFilter can be used for generating the
  // histogram of an image.
  //
  typedef unsigned char  VMeasurementType;  // type for the samples
  typedef float          HMeasurementType;  // type for the histogram


  typedef itk::RGBPixel< VMeasurementType > PixelType;

  const unsigned int numberOfComponents = 3;

  typedef itk::Image< PixelType, imageDimension >   ImageType;

  typedef itk::Statistics::ImageToListSampleFilter<
    ImageType > ImageToListSampleFilterType;

  typedef ImageToListSampleFilterType::ListSampleType  SampleType;

  typedef itk::Statistics::Histogram< HMeasurementType,
          itk::Statistics::DenseFrequencyContainer2 > HistogramType;

  typedef itk::Statistics::SampleToHistogramFilter<
    SampleType, HistogramType > FilterType;

  typedef HistogramType::MeasurementVectorType  MeasurementVectorType;

  typedef FilterType::HistogramSizeType      HistogramSizeType;

  typedef itk::ImageFileReader< ImageType >    ReaderType;

  ReaderType::Pointer reader = ReaderType::New();

  ImageToListSampleFilterType::Pointer imageToSampleFilter =
    ImageToListSampleFilterType::New();

  FilterType::Pointer filter = FilterType::New();

  reader->SetFileName( argv[1] );

  imageToSampleFilter->SetInput( reader->GetOutput() );

  filter->SetInput( imageToSampleFilter->GetOutput() );

  // Test exception when calling Update() without having
  // defined the size of the histogram in the filter.
  try
    {
    filter->Update();
    std::cerr << "Failure to throw expected exception due to lack";
    std::cerr << " of calling SetHistogramSize() in the filter ";
    return EXIT_FAILURE;
    }
  catch( itk::ExceptionObject & )
    {
    std::cout << "Expected exception received" << std::endl;
    }


  const HistogramType * histogram = filter->GetOutput();

  if( histogram->Size() != 0 )
    {
    std::cerr << "Histogram Size should have been zero" << std::endl;
    return EXIT_FAILURE;
    }

  HistogramSizeType histogramSize( numberOfComponents );

  histogramSize[0] = 256;
  histogramSize[1] = 256;
  histogramSize[2] = 256;

  filter->SetHistogramSize( histogramSize );

  try
    {
    filter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int expectedHistogramSize =
    histogramSize[0] * histogramSize[1] * histogramSize[2];

  if( histogram->Size() != expectedHistogramSize )
    {
    std::cerr << "Histogram Size should have been " << expectedHistogramSize << std::endl;
    std::cerr << " but it actually is " << histogram->Size() << std::endl;
    return EXIT_FAILURE;
    }


  HistogramType::ConstIterator histogramItr = histogram->Begin();
  HistogramType::ConstIterator histogramEnd = histogram->End();

  typedef itk::NumericTraits< VMeasurementType >::PrintType    PrintType;

  while( histogramItr != histogramEnd )
    {
    if( histogramItr.GetFrequency() != 0 )
      {
      MeasurementVectorType measurementVector = histogramItr.GetMeasurementVector();
      std::cout << static_cast< PrintType >( measurementVector[0] ) << "  ";
      std::cout << static_cast< PrintType >( measurementVector[1] ) << "  ";
      std::cout << static_cast< PrintType >( measurementVector[2] ) << "  ";
      std::cout << histogramItr.GetFrequency() << std::endl;
      }
    ++histogramItr;
    }


  SampleType::ConstPointer sample = imageToSampleFilter->GetOutput();

  SampleType::ConstIterator itr = sample->Begin();
  SampleType::ConstIterator end = sample->End();

  while( itr != end )
    {
    PixelType pixel = itr.GetMeasurementVector();
    std::cout << static_cast< PrintType >( pixel[0] ) << "  ";
    std::cout << static_cast< PrintType >( pixel[1] ) << "  ";
    std::cout << static_cast< PrintType >( pixel[2] ) << std::endl;
    ++itr;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
