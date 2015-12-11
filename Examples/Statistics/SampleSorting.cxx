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
// \index{Statistics!Order statistics}
// \index{Statistics!Sorting}
// \index{Statistics!Insert sort}
// \index{Statistics!Heap sort}
// \index{Statistics!Introspective sort}
// \index{Statistics!Quick select}
//
// \index{itk::Statistics::Subsample}
// \index{itk::Statistics::InsertSort}
// \index{itk::Statistics::HeapSort}
// \index{itk::Statistics::IntrospectiveSort}
// \index{itk::Statistics::QuickSelect}
//
// Sometimes we want to sort the measurement vectors in a sample. The sorted
// vectors may reveal some characteristics of the sample.  The \emph{insert
// sort}, the \emph{heap sort}, and the \emph{introspective sort} algorithms
// \cite{Musser1997} for samples are implemented in ITK. To learn pros and
// cons of each algorithm, please refer to \cite{Duda2000}. ITK also
// offers the \emph{quick select} algorithm.
//
// Among the subclasses of the \subdoxygen{Statistics}{Sample}, only the
// class \subdoxygen{Statistics}{Subsample} allows users to change the order
// of the measurement vector. Therefore, we must create a Subsample to do any
// sorting or selecting.
//
// We include the header files for the \subdoxygen{Statistics}{ListSample}
// and the \code{Subsample} classes.
//
// Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkListSample.h"
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// The sorting and selecting related functions are in the include file
// \code{itkStatisticsAlgorithm.h}. Note that all functions in this file
// are in the \code{itk::Statistics::Algorithm} namespace.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkStatisticsAlgorithm.h"
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// We need another header for measurement vectors. We are going to use
// the \doxygen{Vector} class which is a subclass of the \doxygen{FixedArray}
// in this example.
//
// We define the types of the measurement vectors, the sample, and the
// subsample.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkVector.h"
// Software Guide : EndCodeSnippet

typedef int                               MeasurementType;
typedef itk::Vector< MeasurementType, 2 > MeasurementVectorType;
typedef itk::Statistics::ListSample< MeasurementVectorType >
                                          SampleType;
typedef itk::Statistics::Subsample< SampleType >
                                          SubsampleType;

// Software Guide : BeginLatex
//
// We define two functions for convenience. The first one clears the content
// of the subsample and fill it with the measurement vectors from the
// sample.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
void initializeSubsample(SubsampleType* subsample, SampleType* sample)
{
  subsample->Clear();
  subsample->SetSample(sample);
  subsample->InitializeWithAllInstances();
}
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// The second one prints out the content of the subsample using the
// Subsample's iterator interface.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
void printSubsample(SubsampleType* subsample, const char* header)
{
  std::cout << std::endl;
  std::cout << header << std::endl;
  SubsampleType::Iterator iter = subsample->Begin();
  while ( iter != subsample->End() )
    {
    std::cout << "instance identifier = " << iter.GetInstanceIdentifier()
              << " \t measurement vector = "
              << iter.GetMeasurementVector()
              << std::endl;
    ++iter;
    }
}
// Software Guide : EndCodeSnippet

int main()
{
  // Software Guide : BeginLatex
  //
  // The following code snippet will create a ListSample object
  // with two-component int measurement vectors and put the measurement
  // vectors: [5,5] - 5 times, [4,4] - 4 times, [3,3] - 3 times, [2,2] -
  // 2 times,[1,1] - 1 time into the \code{sample}.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  SampleType::Pointer sample = SampleType::New();

  MeasurementVectorType mv;
  for (unsigned int i = 5; i > 0; --i )
    {
    for (unsigned int j = 0; j < 2; ++j)
      {
      mv[j] = ( MeasurementType ) i;
      }
    for (unsigned int j = 0; j < i; ++j)
      {
      sample->PushBack(mv);
      }
    }
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // We create a Subsample object and plug-in the \code{sample}.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  SubsampleType::Pointer subsample = SubsampleType::New();
  subsample->SetSample(sample);
  initializeSubsample(subsample, sample);
  printSubsample(subsample, "Unsorted");
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The common parameters to all the algorithms are the
  // Subsample object (\code{subsample}), the dimension
  // (\code{activeDimension}) that will be considered for the sorting or
  // selecting (only the component belonging to the dimension of the
  // measurement vectors will be considered), the beginning index, and
  // the ending index of the measurement vectors in the
  // \code{subsample}. The sorting or selecting algorithms are applied
  // only to the range specified by the beginning index and the ending
  // index. The ending index should be the actual last index plus one.
  //
  // The \doxygen{InsertSort} function does not require any other optional
  // arguments. The following function call will sort the all measurement
  // vectors in the \code{subsample}. The beginning index is \code{0}, and
  // the ending index is the number of the measurement vectors in the
  // \code{subsample}.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  int activeDimension = 0;
  itk::Statistics::Algorithm::InsertSort< SubsampleType >( subsample,
                              activeDimension, 0, subsample->Size() );
  printSubsample(subsample, "InsertSort");
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // We sort the \code{subsample} using the heap sort algorithm. The
  // arguments are identical to those of the insert sort.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  initializeSubsample(subsample, sample);
  itk::Statistics::Algorithm::HeapSort< SubsampleType >( subsample,
                              activeDimension, 0, subsample->Size() );
  printSubsample(subsample, "HeapSort");
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // The introspective sort algorithm needs an additional argument that
  // specifies when to stop the introspective sort loop and sort the fragment
  // of the sample using the heap sort algorithm. Since we set the threshold
  // value as \code{16}, when the sort loop reach the point where the number of
  // measurement vectors in a sort loop is not greater than \code{16}, it will
  // sort that fragment using the insert sort algorithm.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  initializeSubsample(subsample, sample);
  itk::Statistics::Algorithm::IntrospectiveSort< SubsampleType >
                      ( subsample, activeDimension, 0, subsample->Size(), 16 );
  printSubsample(subsample, "IntrospectiveSort");
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // We query the median of the measurements along the
  // \code{activeDimension}. The last argument tells the algorithm that we
  // want to get the \code{subsample->Size()/2}-th element along the
  // \code{activeDimension}. The quick select algorithm changes the order of
  // the measurement vectors.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  initializeSubsample(subsample, sample);
  SubsampleType::MeasurementType median =
          itk::Statistics::Algorithm::QuickSelect< SubsampleType >( subsample,
                                                         activeDimension,
                                                         0, subsample->Size(),
                                                         subsample->Size()/2 );
  std::cout << std::endl;
  std::cout << "Quick Select: median = " << median << std::endl;
  // Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
