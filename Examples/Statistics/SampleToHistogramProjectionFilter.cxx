/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    SampleToHistogramProjectionFilter.cxx
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
// \index{Statistics!Projecting mesurement vectors to 1-D histogram|textbf}
// \index{itk::Statistics::SampleToHistogramProjectionFilter|textbf}
//
// This filter projects measurement vectors of a sample onto a vector
// and fills up a 1-D \subdoxygen{Statistics}{Histogram}. The histram
// will be formed around the mean value set by the \code{SetMean}
// method. The histogram's measurement values are the distance
// between the mean and the projected measurement vectors normalized
// by the standard deviation set by the \code{SetStandardDeviation}.
// Such histogram can be used to analyze the multi-dimensional
// distribution or examine the goodness-of-fit of a projected
// distribution (histogram) with its expected distribution.
//
// We will use the \code{ListSample} as the input sample.
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkListSample.h"
#include "itkSampleToHistogramProjectionFilter.h"
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
  typedef itk::Statistics::ListSample< MeasurementVectorType > SampleType ;

  SampleType::Pointer sample = SampleType::New() ;

  MeasurementVectorType mv ;
  for ( unsigned int i = 1 ; i < 6 ; i++ )
    {
      for (unsigned int j = 0 ; j < 2 ; j++ )
        {
          mv[j] = ( MeasurementType ) i ;
        }
      for ( unsigned int j = 0 ; j < i ; j++ )
        {
          sample->PushBack(mv) ;
        }
    }
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // We create a histogram that has six bins. The histogram's range is
  // [-2, 2). Since the \code{sample} has measurement vectors between
  // [1, 1] and [5,5], The histogram does not seem to cover the whole
  // range. However, the \code{SampleToHistogramProjectionFilter}
  // normalizes the measurement vectors with the given mean and the
  // standard deviation. Therefore, the projected value is approximately
  // the distance between the measurement vector and the mean divided by
  // the standard deviation. 
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::Histogram< float, 1 > HistogramType ;

  HistogramType::Pointer histogram = HistogramType::New() ;

  HistogramType::SizeType size ;
  size.Fill(6) ;
  HistogramType::MeasurementVectorType lowerBound ;
  HistogramType::MeasurementVectorType upperBound ;
  lowerBound[0] = -2 ;
  upperBound[0] = 2 ;

  histogram->Initialize( size, lowerBound, upperBound ) ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // We use the \code{SetInputSample(sample*)} and the
  // \code{SetHistogram(histogram*)} methods to set the input
  // sample and the output histogram that have been created.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::SampleToHistogramProjectionFilter<
    SampleType, float > ProjectorType ;
  
  ProjectorType::Pointer projector = ProjectorType::New() ;
  projector->SetInputSample( sample ) ;
  projector->SetHistogram( histogram ) ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // As mentioned above, this class projects measurement vectors onto the
  // projection axis with normalization using the mean and standard
  // deviation. 
  // \begin{equation}
  // y = \frac{\sum^{d}_{i=0} (x_{i} - \mu_{i})\alpha_{i}}{\sigma}
  // \end{equation}
  // where, y is the projected value, $x$ is the $i$th component of the
  // measurement vector, $\mu_{i}$ is the $i$th component of the mean vector,
  // $\alpha_{i}$ is the $i$th component of the projection axis (a
  // vector), and the $\sigma$ is the standard deviation. 
  //
  // If the bin overlap value is set by the SetHistogramBinOverlap method and 
  // greater than 0.001, the frequency will be weighted based on its closeness
  // of the bin boundaries. In other words, even if a measurment
  // vector falls into a bin, depending on its closeness to the
  // adjacent bins, the frequencies of the adjacent bins will be also
  // updated with weights. If we do not want to use the bin overlapping
  // function, we do not call the \code{SetHistogramBinOverlap(double)}
  // method. The defalut value for the histogram bin overlap is zero, so
  // without calling the method, the filter will not use bin
  // overlapping \cite{Aylward1997a} \cite{Aylward1997b}.
  // Software Guide : EndLatex
  
  // Software Guide : BeginCodeSnippet
  ProjectorType::MeanType mean ;
  mean[0] = 3.66667 ;
  mean[1] = 3.66667 ;

  double standardDeviation = 3 ;

  ProjectorType::ArrayType projectionAxis ;
  projectionAxis[0] = 1;
  projectionAxis[1] = 1 ;

  projector->SetMean( &mean ) ;
  projector->SetStandardDeviation( &standardDeviation ) ;
  projector->SetProjectionAxis( &projectionAxis ) ;
  projector->SetHistogramBinOverlap( 0.25 ) ;
  projector->Update() ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // We print out the updated histogram after the projection.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  float fSum = 0.0 ;
  HistogramType::Iterator iter = histogram->Begin() ;
  while ( iter != histogram->End() )
    {
    std::cout << "instance identifier = " << iter.GetInstanceIdentifier() 
              << "\t measurement vector = " 
              << iter.GetMeasurementVector() 
              << "\t frequency = " 
              << iter.GetFrequency() << std::endl ;
    fSum += iter.GetFrequency() ;
    ++iter ;
    }
  std::cout << " sum of frequency = " << fSum << std::endl ;
  // Software Guide : EndCodeSnippet

  return 0 ;
}
