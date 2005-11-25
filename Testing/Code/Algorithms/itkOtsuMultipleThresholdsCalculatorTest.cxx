/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkOtsuMultipleThresholdsCalculatorTest.cxx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkWin32Header.h"
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif


#include "itkHistogram.h"
#include "itkOtsuMultipleThresholdsCalculator.h"

int itkOtsuMultipleThresholdsCalculatorTest(int, char*[])
{
  typedef float MeasurementType ;
  typedef itk::Statistics::Histogram< MeasurementType, 1 > HistogramType ;
  HistogramType::Pointer histogram = HistogramType::New() ;

  // initialize histogram
  HistogramType::SizeType size;
  size.Fill(64) ;
  HistogramType::MeasurementVectorType lowerBound ;
  HistogramType::MeasurementVectorType upperBound ;
  lowerBound[0] = 0.0 ;
  upperBound[0] = 64.0 ;

  histogram->Initialize(size, lowerBound, upperBound ) ;
  
  // create vector of values.
  typedef std::vector<MeasurementType> ValuesVectorType;
  ValuesVectorType values;
  values.push_back(8.0);
  values.push_back(16.0);
  values.push_back(32.0);
  values.push_back(48.0);

  MeasurementType range = 2.0;

  // create histogram with samples at values +- range.
  for (HistogramType::Iterator iter = histogram->Begin(); iter != histogram->End(); ++iter)
    {
    HistogramType::MeasurementType measurement = iter.GetMeasurementVector()[0];

    for (ValuesVectorType::const_iterator viter = values.begin(); viter != values.end(); ++viter)
      {
      if (measurement > (*viter-range) && measurement <  (*viter+range))
        {
        iter.SetFrequency(1.0);
        }
      }
    }

  // Compute numberOfValues - 1 thresholds.
  unsigned long numberOfThresholds = values.size() - 1;

  typedef itk::OtsuMultipleThresholdsCalculator<HistogramType>  OtsuMultipleThresholdCalculatorType;

  OtsuMultipleThresholdCalculatorType::Pointer otsuThresholdCalculator = OtsuMultipleThresholdCalculatorType::New();

  otsuThresholdCalculator->SetInputHistogram(histogram.GetPointer());
  otsuThresholdCalculator->SetNumberOfThresholds(numberOfThresholds);

  try
    {
    otsuThresholdCalculator->Update();
    }
  catch(itk::ExceptionObject & exp)
    {
    std::cerr << exp << std::endl;
    }
  otsuThresholdCalculator->Print (std::cout);

  OtsuMultipleThresholdCalculatorType::OutputType otsuThresholds = otsuThresholdCalculator->GetOutput();
  
  bool passed = true;

  // Check if thresholds correctly separate values.
  for (unsigned long j = 0; j<numberOfThresholds; ++j)
    {
    if (otsuThresholds[j] < values[j] || otsuThresholds[j] > values[j+1])
      {
      passed = false;
      break;
      }
    }

  if (!passed)
    {
      std::cout << "Test failed." << std::endl;
      std::cout << otsuThresholdCalculator << std::endl;
      return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
