/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

// Software Guide : BeginLatex
//
// This example shows how to create an \subdoxygen{Statistics}{Histogram}
// object and use it.
//
// \index{itk::Sample!Histogram}
//
// We call an instance in a \code{Histogram} object a \emph{bin}.  The
// Histogram differs from the
// \subdoxygen{Statistics}{ListSample},
// \subdoxygen{Statistics}{ImageToListSampleAdaptor}, or
// \subdoxygen{Statistics}{PointSetToListSampleAdaptor} in significant ways.
// Histograms can have a variable number of values (\code{float}
// type) for each measurement vector, while the three other classes
// have a fixed value (one) for all measurement vectors. Also
// those array-type containers can have multiple instances (data
// elements) with identical measurement vector values. However,
// in a Histogram object, there is one unique instance for any
// given measurement vector.
//
// \begin{figure}
// \centering
// \includegraphics[width=0.4\textwidth]{Histogram}
// \itkcaption[Histogram]{Conceptual histogram data structure.}
// \protect\label{fig:StatHistogram}
// \end{figure}
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkHistogram.h"
#include "itkDenseFrequencyContainer2.h"
// Software Guide : EndCodeSnippet

int main()
{
  // Software Guide : BeginLatex
  //
  // Here we create a histogram with dense frequency containers. In
  // this example we will not have any zero-frequency measurements,
  // so the dense frequency container is the appropriate choice. If
  // the histogram is expected to have many empty (zero) bins, a sparse
  // frequency container would be the better option. Here we also set
  // the size of the measurement vectors to be 2 components.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef float                                         MeasurementType;
  typedef itk::Statistics::DenseFrequencyContainer2     FrequencyContainerType;
  typedef FrequencyContainerType::AbsoluteFrequencyType FrequencyType;

  const unsigned int numberOfComponents = 2;
  typedef itk::Statistics::Histogram< MeasurementType,
    FrequencyContainerType > HistogramType;

  HistogramType::Pointer histogram = HistogramType::New();
  histogram->SetMeasurementVectorSize( numberOfComponents );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // We initialize it as a $3\times3$ histogram with equal size intervals.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  HistogramType::SizeType size( numberOfComponents );
  size.Fill(3);
  HistogramType::MeasurementVectorType lowerBound( numberOfComponents );
  HistogramType::MeasurementVectorType upperBound( numberOfComponents );
  lowerBound[0] = 1.1;
  lowerBound[1] = 2.6;
  upperBound[0] = 7.1;
  upperBound[1] = 8.6;

  histogram->Initialize(size, lowerBound, upperBound );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Now the histogram is ready for storing frequency values. We will
  // fill each bin's frequency according to the Figure
  // \ref{fig:StatHistogram}. There are three ways of accessing data
  // elements in the histogram:
  // \begin{itemize}
  //   \item using instance identifiers---just like any other Sample object;
  //   \item using n-dimensional indices---just like an Image object;
  //   \item using an iterator---just like any other Sample object.
  // \end{itemize}
  // In this example, the index $(0, 0)$ refers the same bin as the instance
  // identifier (0) refers to. The instance identifier of the index (0,
  // 1) is (3), (0, 2) is (6), (2, 2) is (8), and so on.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  histogram->SetFrequency(0UL, static_cast<FrequencyType>(0.0));
  histogram->SetFrequency(1UL, static_cast<FrequencyType>(2.0));
  histogram->SetFrequency(2UL, static_cast<FrequencyType>(3.0));
  histogram->SetFrequency(3UL, static_cast<FrequencyType>(2.0f));
  histogram->SetFrequency(4UL, static_cast<FrequencyType>(0.5f));
  histogram->SetFrequency(5UL, static_cast<FrequencyType>(1.0f));
  histogram->SetFrequency(6UL, static_cast<FrequencyType>(5.0f));
  histogram->SetFrequency(7UL, static_cast<FrequencyType>(2.5f));
  histogram->SetFrequency(8UL, static_cast<FrequencyType>(0.0f));
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Let us examine if the frequency is set correctly by calling the
  // \code{GetFrequency(index)} method. We can use the
  // \code{GetFrequency(instance identifier)} method for the same purpose.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  HistogramType::IndexType index( numberOfComponents );
  index[0] = 0;
  index[1] = 2;
  std::cout << "Frequency of the bin at index  " << index
            << " is " << histogram->GetFrequency(index)
            << ", and the bin's instance identifier is "
            << histogram->GetInstanceIdentifier(index) << std::endl;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // For test purposes, we create a measurement vector and an index
  // that belongs to the center bin.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  HistogramType::MeasurementVectorType mv( numberOfComponents );
  mv[0] = 4.1;
  mv[1] = 5.6;
  index.Fill(1);
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // We retrieve the measurement vector at the index value (1, 1), the center
  // bin's measurement vector. The output is [4.1, 5.6].
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  std::cout << "Measurement vector at the center bin is "
            << histogram->GetMeasurementVector(index) << std::endl;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Since all the measurement vectors are unique in the Histogram class, we
  // can determine the index from a measurement vector.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  HistogramType::IndexType resultingIndex;
  histogram->GetIndex(mv,resultingIndex);
  std::cout << "Index of the measurement vector " << mv
            << " is " << resultingIndex << std::endl;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // In a similar way, we can get the instance identifier from the index.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  std::cout << "Instance identifier of index " << index
            << " is " << histogram->GetInstanceIdentifier(index)
            << std::endl;
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // If we want to check if an index is valid, we use the method
  // \code{IsIndexOutOfBounds(index)}. The following code snippet fills the
  // index variable with (100, 100). It is obviously not a valid index.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  index.Fill(100);
  if ( histogram->IsIndexOutOfBounds(index) )
    {
    std::cout << "Index " << index << " is out of bounds." << std::endl;
    }
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // The following code snippets show how to get the histogram size
  // and frequency dimension.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  std::cout << "Number of bins = " << histogram->Size()
            << " Total frequency = " << histogram->GetTotalFrequency()
            << " Dimension sizes = " << histogram->GetSize() << std::endl;
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // The Histogram class has a quantile calculation method,
  // \code{Quantile(dimension, percent)}. The following code returns the 50th
  // percentile along the first dimension. Note that the quantile calculation
  // considers only one dimension.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  std::cout << "50th percentile along the first dimension = "
            << histogram->Quantile(0, 0.5) << std::endl;
  // Software Guide : EndCodeSnippet
  return EXIT_SUCCESS;
}
