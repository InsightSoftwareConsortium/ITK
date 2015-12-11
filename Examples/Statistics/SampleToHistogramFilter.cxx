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
// \index{Statistics!Importing ListSample to Histogram}
// \index{itk::Statistics::List\-Sample\-To\-Histogram\-Filter}
//
// Sometimes we want to work with a histogram instead of a list of
// measurement vectors (e.g. \subdoxygen{Statistics}{ListSample},
// \subdoxygen{Statistics}{ImageToListSampleAdaptor}, or
// \subdoxygen{Statistics}{PointSetToListSample}) to use less memory or to
// perform a particular type od analysis. In such cases, we can import data
// from a sample type to a \subdoxygen{Statistics}{Histogram} object
// using the \subdoxygen{Statistics}{SampleToHistogramFiler}.
//
// We use a ListSample object as the input for the filter. We include the
// header files for the ListSample and Histogram classes, as well as the
// filter.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkListSample.h"
#include "itkHistogram.h"
#include "itkSampleToHistogramFilter.h"
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
  for (unsigned int i = 1; i < 6; ++i)
    {
    for (unsigned int j = 0; j < 2; ++j)
      {
      mv[j] = ( MeasurementType ) i;
      }
    for (unsigned int j = 0; j < i; ++j)
      {
      listSample->PushBack(mv);
      }
    }
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // Here, we set up the size and bound of the output histogram.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef float HistogramMeasurementType;
  const unsigned int numberOfComponents = 2;
  typedef itk::Statistics::Histogram< HistogramMeasurementType >
    HistogramType;

  HistogramType::SizeType size( numberOfComponents );
  size.Fill(5);

  HistogramType::MeasurementVectorType lowerBound( numberOfComponents );
  HistogramType::MeasurementVectorType upperBound( numberOfComponents );

  lowerBound[0] = 0.5;
  lowerBound[1] = 0.5;

  upperBound[0] = 5.5;
  upperBound[1] = 5.5;
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // Now, we set up the \code{SampleToHistogramFilter} object by passing
  // \code{listSample} as the input and initializing the histogram size
  // and bounds with the \code{SetHistogramSize()},
  // \code{SetHistogramBinMinimum()}, and \code{SetHistogramBinMaximum()}
  // methods. We execute the filter by calling the \code{Update()}
  // method.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::SampleToHistogramFilter< ListSampleType,
                           HistogramType > FilterType;
  FilterType::Pointer filter = FilterType::New();

  filter->SetInput( listSample );
  filter->SetHistogramSize( size );
  filter->SetHistogramBinMinimum( lowerBound );
  filter->SetHistogramBinMaximum( upperBound );
  filter->Update();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The \code{Size()} and \code{GetTotalFrequency()} methods return the same
  // values as the \code{sample} does.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet

  const HistogramType* histogram = filter->GetOutput();

  HistogramType::ConstIterator iter = histogram->Begin();
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

  return EXIT_SUCCESS;
}
