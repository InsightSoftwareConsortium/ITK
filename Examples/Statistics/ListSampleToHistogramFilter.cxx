/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ListSampleToHistogramFilter.cxx
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

// Software Guide : BeginLatex
//
// \index{Statistics!Importing ListSample to Histogram}
// \index{itk::Statistics::List\-Sample\-To\-Histogram\-Filter}
//
// Sometimes we want to work with a histogram instead of a list of
// measurement vectors (e.g. \subdoxygen{Statistics}{ListSample},
// \subdoxygen{Statistics}{ImageToListAdaptor}, or
// \subdoxygen{Statistics}{PointSetToListSample}) to use less memory or to
// perform a particular type od analysis. In such cases, we can import data
// from a list type sample to a \subdoxygen{Statistics}{Histogram} object
// using the \subdoxygen{Statistics}{ListSampleToHistogramFilter}.
//
// We use a ListSample object as the input for the filter. We include the
// header files for the ListSample and Histogram classes, as well as the
// filter.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkListSample.h"
#include "itkHistogram.h"
#include "itkListSampleToHistogramFilter.h"
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// We need another header for the type of the measurement vectors. We are
// going to use the \doxygen{Vector} class which is a subclass of the
// \doxygen{FixedArray} in this example.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkVector.h"
// Software Guide : EndCodeSnippet

int main()
{
  // Software Guide : BeginLatex
  //
  // The following code snippet creates a ListSample object with
  // two-component \code{int} measurement vectors and put the measurement
  // vectors: [1,1] - 1 time, [2,2] - 2 times, [3,3] - 3 times, [4,4] - 4
  // times, [5,5] - 5 times into the \code{listSample}.
  // 
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef int MeasurementType;
  const unsigned int MeasurementVectorLength = 2;
  typedef itk::Vector< MeasurementType , MeasurementVectorLength > 
                                                               MeasurementVectorType;
  typedef itk::Statistics::ListSample< MeasurementVectorType > ListSampleType;
  ListSampleType::Pointer listSample = ListSampleType::New();
  listSample->SetMeasurementVectorSize( MeasurementVectorLength );

  MeasurementVectorType mv;
  for ( unsigned int i = 1 ; i < 6 ; i++ )
    {
    for ( unsigned int j = 0 ; j < 2 ; j++ )
      {
      mv[j] = ( MeasurementType ) i;
      }
    for ( unsigned int j = 0 ; j < i ; j++ )
      {
      listSample->PushBack(mv);
      }
    }
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // Here, we create a Histogram object with equal interval bins using the
  // \code{Initalize()} method.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef float HistogramMeasurementType;
  typedef itk::Statistics::Histogram< HistogramMeasurementType, 2 >  
    HistogramType;
  HistogramType::Pointer histogram = HistogramType::New();

  HistogramType::SizeType size;
  size.Fill(5);

  HistogramType::MeasurementVectorType lowerBound;
  HistogramType::MeasurementVectorType upperBound;

  lowerBound[0] = 0.5;
  lowerBound[1] = 0.5;

  upperBound[0] = 5.5;
  upperBound[1] = 5.5;

  histogram->Initialize( size, lowerBound, upperBound );
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // The \code{Size()} and \code{GetTotalFrequency()} methods return the same
  // values as the \code{sample} does.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::ListSampleToHistogramFilter< ListSampleType, 
                           HistogramType > FilterType;
  FilterType::Pointer filter = FilterType::New();

  filter->SetListSample( listSample );
  filter->SetHistogram( histogram );
  filter->Update();

  HistogramType::Iterator iter = histogram->Begin();
  while ( iter != histogram->End() )
    {
    std::cout << "Measurement vectors = " << iter.GetMeasurementVector()
              << " frequency = " << iter.GetFrequency() << std::endl;
    ++iter;
    }

  std::cout << "Size = " << histogram->Size() << std::endl;
  std::cout << "Total frequency = " 
            << histogram->GetTotalFrequency() << std::endl;
  // Software Guide : EndCodeSnippet

  return 0;
}
