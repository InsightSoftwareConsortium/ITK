/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    Histogram.cxx
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
// This example shows how to create an \subdoxygen{Statistics}{Histogram}
// object and use it.
//
// \index{itk::Sample!Histogram|textbf}
// we call an instance in a \code{Histogram} object a bin.  The
// \code{Histogram} differs from the
// \subdoxygen{Statistics}{ListSample},
// \subdoxygen{Statistics}{ImageToListAdaptor}, or
// \subdoxygen{Statistics}{PointSetToListAdaptor} in significant ways.
// \code{Histogram} can have varying frequency value (\code{float}
// type) for each measurement vector, while the three other classes
// have a fixed value (one) for all measurement vectors in them. Also
// those array-type containers can have multiple instances (data
// elements) that have identical measurement vector values. However,
// in a \code{Histogram} object, there is one unique instance.for any
// given measurement vector value.
//
// \begin{figure}
// \centering
// \includegraphics[width=0.4\textwidth]{Histogram.eps}
// \itkcaption[Histogram]{Conceptual histogram data structure}
// \protect\label{fig:StatHistogram}
// \end{figure}
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkHistogram.h"
// Software Guide : EndCodeSnippet

int main() 
{
  // Software Guide : BeginLatex
  // Here we create a \code{Histogram} object with 2-component measurement vectors.
  // Software Guide : EndLatex 
 
  // Software Guide : BeginCodeSnippet
  typedef float MeasurementType ;
  typedef itk::Statistics::Histogram< MeasurementType, 2 > HistogramType ;
  HistogramType::Pointer histogram = HistogramType::New() ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // We initialize it as a 3 x 3 histogram with equal size intervals.
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  HistogramType::SizeType size ;
  size.Fill(3) ;
  HistogramType::MeasurementVectorType lowerBound ;
  HistogramType::MeasurementVectorType upperBound ;
  lowerBound[0] = 1.1 ;
  lowerBound[1] = 2.6 ;
  upperBound[0] = 7.1 ;
  upperBound[1] = 8.6 ;

  histogram->Initialize(size, lowerBound, upperBound ) ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // Now the histogram is ready for storing frequency values. We will
  // fill the each bin's frequency according to the previous figure
  // \ref{fig:StatHistogram}. There are three ways of accessing data
  // elements in the \code{histogram}:
  // \begin{itemize}
  //   \item using instance identifiers - just like any other
  // \code{Sample} object
  //   \item using n-dimensional indexes - just like an \doxygen{Image} object
  //   \item using an iterator - just like any other \code{Sample}
  // object.
  // \end{itemize}
  // In this example, the index (0, 0) refers the same bin as the instance
  // identifier (0) refers. The instance identifier of the index (0,
  // 1) is (3), (0, 2) is (6), (2, 2) is (8), and so on. 
  // Software Guide : EndLatex 
  
  // Software Guide : BeginCodeSnippet
  histogram->SetFrequency(0UL, 0.0) ; 
  histogram->SetFrequency(1UL, 2.0) ; 
  histogram->SetFrequency(2UL, 3.0) ; 
  histogram->SetFrequency(3UL, 2.0) ; 
  histogram->SetFrequency(4UL, 0.5) ; 
  histogram->SetFrequency(5UL, 1.0) ; 
  histogram->SetFrequency(6UL, 5.0) ; 
  histogram->SetFrequency(7UL, 2.5) ; 
  histogram->SetFrequency(8UL, 0.0) ; 
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // Let us examine if the frequency is set correctly by calling the
  // \code{GetFrequency(index)} method. We can use the
  // \code{GetFrequency(instance identifier)} method for the same
  // purpose.
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  HistogramType::IndexType index ;
  index[0] = 0 ;
  index[1] = 2 ;
  std::cout << "Frequency of the bin at index  " << index
            << " is " << histogram->GetFrequency(index) 
            << ", and the bin's instance identifier is " 
            << histogram->GetInstanceIdentifier(index) << std::endl ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // For the test purpose, we create a measurement vector and an index
  // that belong to the center bin.
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  HistogramType::MeasurementVectorType mv ;
  mv[0] = 4.1 ;
  mv[1] = 5.6 ;
  index.Fill(1) ;
  // Software Guide : EndCodeSnippet
  
  // Software Guide : BeginLatex
  // We retrieve the measurement vector at the index value (1, 1), the center
  // bin's measurement vector. The output is [4.1, 5.6]. 
  // Software Guide : EndLatex 
 
  // Software Guide : BeginCodeSnippet
  std::cout << "Measurement vector at the center bin is " 
            << histogram->GetMeasurementVector(index) << std::endl ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // Since all the measurement vectors are unique in a \code{Histogram}
  // object, we can determine the index from a measurement vector.
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  std::cout << "Index of the measurement vector " << mv 
            << " is " << histogram->GetIndex(mv) << std::endl ;
  // Software Guide : EndCodeSnippet
  
  // Software Guide : BeginLatex
  // In a similar way, we can get the instance identifier from the index.
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  std::cout << "Instance identifier of index " << index
            << " is " << histogram->GetInstanceIdentifier(index) 
            << std::endl ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // If we want check if an index is valid one, we use 
  // \code{IsIndexOutOfBounds(index)}. The following code snippet fills
  // the index variable with (100, 100). It is obviously not a valid index.
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  index.Fill(100) ;
  
  if ( histogram->IsIndexOutOfBounds(index) )
    {
    std::cout << "Index " << index << "is out of bounds." << std::endl ;
    }
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // From the following code snippets, it is clear that the return
  // values from the \code{Size()} and \code{GetSize()} methods.
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  std::cout << "Number of bins = " << histogram->Size()
            << " Total frequency = " << histogram->GetTotalFrequency()
            << " Dimension sizes = " << histogram->GetSize() << std::endl ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // The \code{Histogram} class has a quantile calculation method,
  // \code{Quantile(dimension, percent)}. The following code returns 50th
  // percentile along the first dimension. Note that the quatile calculation
  // considers only one dimension.
  // Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  std::cout << "50th percentile along the first dimension = " 
            << histogram->Quantile(0, 0.5) << std::endl ;
  // Software Guide : EndCodeSnippet

  return 0 ;
}



