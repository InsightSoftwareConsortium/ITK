/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ListSampleToHistogramGenerator.cxx
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
// \index{Statistics!Convert ListSample to Histogram}
// \index{itk::Statistics::List\-Sample\-To\-Histogram\-Generator}
//
// In previous sections (Section~\ref{sec:ListSampleToHistogramFilter} we
// decribed how to import data from a \subdoxygen{Statistics}{ListSample} to
// a \subdoxygen{Statistics}{Histogram}. An alternative way of creating a
// histogram is to use
// \subdoxygen{Statistics}{ListSampleToHistogramGenerator}.  With this
// generator, we only provide the size of the histogram and the type of the
// measurement vectors in the histogram. The generator will automatically
// find the lower and upper space bound and create equal interval bins in the
// histogram.
//
// We use a ListSample object as the input for the filter. We include the
// header files for the ListSample, Histogram, and the filter itself.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkListSample.h"
#include "itkHistogram.h"
#include "itkListSampleToHistogramGenerator.h"
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// We need another header for measurement vectors. We are going to use
// the \doxygen{Vector} class which is a subclass of the \doxygen{FixedArray}
// in this example. 
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkVector.h"
// Software Guide : EndCodeSnippet

int main()
{
  // Software Guide : BeginLatex
  //
  // The following code snippet will create a ListSample object
  // with two-component int measurement vectors and put the measurement
  // vectors: [1,1] - 1 time, [2,2] - 2 times, [3,3] - 3 times, [4,4] -
  // 4 times, [5,5] - 5 times into the ListSample.
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
  // The ListSampleToHistogramGenerator will find the lower and upper bound
  // from the input sample and create equal interval bins. Since a Histogram
  // object does not include the upper bound value and we want to include
  // [5,5] measurement vector, we increase the upper-bound by the calculated
  // bin interval/10.0 (divider). The divider is set by the
  // \code{SetMarginalScale(float)} method. If you want to create a
  // non-uniform histogram, you should use the ListSampleToHistogramFilter
  // (see Section~\ref{sec:ListSampleToHistogramFilter}). The filter does not
  // create a Histogram object. Instead, users should create a
  // Histogram object with varying intervals and use the filter to
  // fill the Histogram objects with the measurement vectors from a
  // ListSample object.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef float HistogramMeasurementType;
  typedef itk::Statistics::ListSampleToHistogramGenerator< ListSampleType, 
          HistogramMeasurementType, 
          itk::Statistics::DenseFrequencyContainer< float >,
          MeasurementVectorLength >             GeneratorType;
  GeneratorType::Pointer generator = GeneratorType::New();

  GeneratorType::HistogramType::SizeType size;
  size.Fill(5);

  generator->SetListSample( listSample );
  generator->SetNumberOfBins( size );
  generator->SetMarginalScale( 10.0 );
  generator->Update();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //  
  // The following code prints out the content of the resulting
  // histogram.
  //  
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  GeneratorType::HistogramType::ConstPointer histogram = generator->GetOutput();
  GeneratorType::HistogramType::ConstIterator iter = histogram->Begin();
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
