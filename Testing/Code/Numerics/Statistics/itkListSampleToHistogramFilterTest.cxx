/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkListSampleToHistogramFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkListSample.h"
#include "itkHistogram.h"
#include "itkListSampleToHistogramFilter.h"
#include "itkVector.h"

int itkListSampleToHistogramFilterTest( int, char* [] )
{
  // The following code snippet will create a \code{ListSample} object
  // with two-component int measurement vectors and put the measurement
  // vectors: [1,1] - 1 time, [2,2] - 2 times, [3,3] - 3 times, [4,4] -
  // 4 times, [5,5] - 5 times into the \code{listSample}.

  typedef int MeasurementType ;
  typedef itk::Vector< MeasurementType , 2 > MeasurementVectorType ;
  typedef itk::Statistics::ListSample< MeasurementVectorType > ListSampleType ;

  ListSampleType::Pointer listSample = ListSampleType::New() ;

  MeasurementVectorType mv ;
  for ( unsigned int i = 1 ; i < 6 ; i++ )
    {
    for (unsigned int j = 0 ; j < 2 ; j++ )
      {
      mv[j] = ( MeasurementType ) i ;
      }
    for ( unsigned int j = 0 ; j < i ; j++ )
      {
      listSample->PushBack(mv) ;
      }
    }

  // Here, we create a \code{Histogram} object with equal interval bins
  // using the \code{Initalize(n-dimensional size, n-dimensional lower
  // bound, n-dimensional upper bound)} method.

  typedef float HistogramMeasurementType ;
  typedef itk::Statistics::Histogram< HistogramMeasurementType, 2 >
    HistogramType ;
  HistogramType::Pointer histogram = HistogramType::New() ;

  HistogramType::SizeType size ;
  size.Fill(5) ;
  HistogramType::MeasurementVectorType lowerBound ;
  HistogramType::MeasurementVectorType upperBound ;
  lowerBound[0] = 0.5 ;
  lowerBound[1] = 0.5 ;
  upperBound[0] = 5.5 ;
  upperBound[1] = 5.5 ;

  histogram->Initialize( size, lowerBound, upperBound ) ;

  // The \code{Size()} and \code{GetTotalFrequency()} returns the same
  // values as the \code{sample} does.

  typedef itk::Statistics::ListSampleToHistogramFilter< ListSampleType,
    HistogramType > FilterType ;
  FilterType::Pointer filter = FilterType::New() ;

  filter->SetListSample( listSample ) ;
  filter->SetHistogram( histogram ) ;
  filter->Update() ;

  HistogramType::Iterator iter = histogram->Begin() ;
  while ( iter != histogram->End() )
    {
    HistogramType::MeasurementVectorType hmv = iter.GetMeasurementVector() ;
    if ( (hmv[0] == hmv[1])  && ( iter.GetFrequency() != float( hmv[0] ) ) )
      {
      std::cout << "Test failed" << std::endl ;
      return EXIT_FAILURE ;
      }
    ++iter ;
    }

  std::cout << "Test passed" << std::endl ;
  return EXIT_SUCCESS ;
}
