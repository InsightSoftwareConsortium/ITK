/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ListSampleToHistogramGenerator.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// Software Guide : BeginLatex
//
// \index{Statistics!Convert ListSample to Histogram|textbf}
// \index{itk::Statistics::ListSampleToHistogramGenerator|textbf}
//
// We decribed how to import data from a \code{ListSample} to a
// \code{Histogram} in the section
// \ref{sec:ListSampleToHistogramFilter}. For the similar purpose,
// we can use \subdoxygen{Statistics}{ListSampleToHistogramGenerator}
// without any predefined \code{Histogram} object. With this
// generator, we only provide the size of the histogram and the type
// of the measurement vectors in the histogram. The generator will
// automatically find the lower and upper space bound and create equal
// interval bins in the histogram.
//
// We use a \code{ListSample} object as the input for the filter. We
// include the header files for the \code{ListSample},
// \code{Histogram}, and the filter.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkListSample.h"
#include "itkHistogram.h"
#include "itkListSampleToHistogramGenerator.h"
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
// We need another header for measurement vectors. We are going to use
// the \doxygen{Vector} class which is a subclass of the \doxygen{FixedArray}
// in this example. 
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkVector.h"
// Software Guide : EndCodeSnippet

int main()
{
  // Software Guide : BeginLatex
  // The following code snippet will create a \code{ListSample} object
  // with two-component int measurement vectors and put the measurement
  // vectors: [1,1] - 1 time, [2,2] - 2 times, [3,3] - 3 times, [4,4] -
  // 4 times, [5,5] - 5 times into the \code{listSample}.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
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
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // The \code{generator} will find the lower bound and upper bound
  // from the input sample and create equal interval bins. Since a
  // \code{Histogram} object does not include the upper bound value
  // and we want to include [5,5] measurement vector, we increase the
  // upper-bound by the calculated bin interval / 10.0 (divider). The
  // divider is set by the \code{SetMarginalScale(float)} method. If
  // you want to create an notunifrom histogram, you should use the
  // \subdoxygen{Statistics}{ListSampleToHistogramFilter} (see section
  // \ref{sec:ListSampleToHistogramFilter}). The filter is not
  // creating a \code{Histogram} object. Instead, users should create
  // an \code{Histogram} object with varying intervals and use the
  // filter to fill the \code{Histogram} objects with the measurement
  // vectors from a \code{ListSample} object.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef float HistogramMeasurementType ;
  typedef itk::Statistics::ListSampleToHistogramGenerator< ListSampleType, 
    HistogramMeasurementType > GeneratorType ;
  
  GeneratorType::Pointer generator = GeneratorType::New() ;

  GeneratorType::HistogramType::SizeType size ;
  size.Fill(5) ;

  generator->SetListSample( listSample ) ;
  generator->SetNumberOfBins(size) ;
  generator->SetMarginalScale( 10.0 ) ;
  generator->Update() ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // The following code prints out the content of the resulting
  // histogram.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  GeneratorType::HistogramType::Pointer histogram = generator->GetOutput() ;
  GeneratorType::HistogramType::Iterator iter = histogram->Begin() ;
  while ( iter != histogram->End() )
    {
    std::cout << "Measurement vectors = " << iter.GetMeasurementVector()
              << " frequency = " << iter.GetFrequency() << std::endl ;
    ++iter ;
    }

  std::cout << "Size = " << histogram->Size() << std::endl ;
  std::cout << "Total frequency = " 
            << histogram->GetTotalFrequency() << std::endl ;
  // Software Guide : EndCodeSnippet

  return 0 ;
}
