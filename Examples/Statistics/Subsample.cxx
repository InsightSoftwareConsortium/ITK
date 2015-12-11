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
// \index{itk::Statistics::Subsample}
//
// The \subdoxygen{Statistics}{Subsample} is a derived sample. In other
// words, it requires another \subdoxygen{Statistics}{Sample} object for
// storing measurement vectors. The Subsample class stores a subset of
// instance identifiers from another Sample object.  \emph{Any} Sample's
// subclass can be the source Sample object. You can create a Subsample
// object out of another Subsample object. The Subsample class is useful for
// storing classification results from a test Sample object or for just
// extracting some part of interest in a Sample object. Another good use of
// Subsample is sorting a Sample object. When we use an \doxygen{Image}
// object as the data source, we do not want to change the order of data
// elements in the image. However, we sometimes want to sort or
// select data elements according to their order. Statistics algorithms for
// this purpose accepts only Subsample objects as inputs. Changing the order
// in a Subsample object does not change the order of the source sample.
//
// To use a Subsample object, we include the header files for
// the class itself and a Sample class. We will use the
// \subdoxygen{Statistics}{ListSample} as the input sample.
//
// Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkListSample.h"
#include "itkSubsample.h"
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// We need another header for measurement vectors. We are going to use
// the \doxygen{Vector} class in this example.
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
  // with three-component float measurement vectors and put three
  // measurement vectors into the list.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Vector< float, 3 > MeasurementVectorType;
  typedef itk::Statistics::ListSample< MeasurementVectorType > SampleType;
  SampleType::Pointer sample = SampleType::New();
  MeasurementVectorType mv;
  mv[0] = 1.0;
  mv[1] = 2.0;
  mv[2] = 4.0;

  sample->PushBack(mv);

  mv[0] = 2.0;
  mv[1] = 4.0;
  mv[2] = 5.0;
  sample->PushBack(mv);

  mv[0] = 3.0;
  mv[1] = 8.0;
  mv[2] = 6.0;
  sample->PushBack(mv);
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // To create a Subsample instance, we define the type of the Subsample with
  // the source sample type, in this case, the previously defined
  // \code{SampleType}. As usual, after that, we call the \code{New()} method
  // to create an instance. We must plug in the source sample, \code{sample},
  // using the \code{SetSample()} method. However, with regard to data
  // elements, the Subsample is empty. We specify which data elements, among
  // the data elements in the Sample object, are part of the
  // Subsample. There are two ways of doing that. First, if we want to
  // include every data element (instance) from the sample, we simply call
  // the \code{InitializeWithAllInstances()} method like the following:
  //
  // \small
  // \begin{verbatim}
  //   subsample->InitializeWithAllInstances();
  // \end{verbatim}
  // \normalsize
  //
  // This method is useful when we want to create a Subsample
  // object for sorting all the data elements in a Sample
  // object. However, in most cases, we want to include only a subset of
  // a Sample object. For this purpose, we use the
  // \code{AddInstance(instance identifier)} method in this example. In
  // the following code snippet, we include only the first and last
  // instance in our subsample object from the three instances
  // of the Sample class.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::Subsample< SampleType > SubsampleType;
  SubsampleType::Pointer subsample = SubsampleType::New();
  subsample->SetSample( sample );

  subsample->AddInstance( 0UL );
  subsample->AddInstance( 2UL );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The Subsample is ready for use. The following code snippet
  // shows how to use \code{Iterator} interfaces.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  SubsampleType::Iterator iter = subsample->Begin();
  while ( iter != subsample->End() )
    {
    std::cout << "instance identifier = " << iter.GetInstanceIdentifier()
              << "\t measurement vector = "
              << iter.GetMeasurementVector()
              << "\t frequency = "
              << iter.GetFrequency()
              << std::endl;
    ++iter;
    }
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // As mentioned earlier, the instances in a Subsample can be sorted without
  // changing the order in the source Sample. For this purpose, the Subsample
  // provides an additional instance indexing scheme. The indexing scheme is
  // just like the instance identifiers for the Sample. The index is an
  // integer value starting at 0, and the last value is one less than the
  // number of all instances in a Subsample. The \code{Swap(0, 1)} method,
  // for example, swaps two instance identifiers of the first data element
  // and the second element in the Subsample. Internally, the \code{Swap()}
  // method changes the instance identifiers in the first and second
  // position. Using indices, we can print out the effects of the
  // \code{Swap()} method. We use the
  // \code{GetMeasurementVectorByIndex(index)} to get the measurement vector
  // at the index position. However, if we want to use the common methods of
  // Sample that accepts instance identifiers, we call them after we get
  // the instance identifiers using \code{GetInstanceIdentifier(index)}
  // method.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  subsample->Swap(0, 1);

  for ( int index = 0; index < subsample->Size(); ++index )
    {
    std::cout << "instance identifier = "
              << subsample->GetInstanceIdentifier(index)
              << "\t measurement vector = "
              << subsample->GetMeasurementVectorByIndex(index)
              << std::endl;
    }
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Since we are using a ListSample object as the source sample, the
  // following code snippet will return the same value (2) for the
  // \code{Size()} and the \code{GetTotalFrequency()} methods. However, if we
  // used a Histogram object as the source sample, the two return
  // values might be different because a Histogram allows varying
  // frequency values for each instance.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  std::cout << "Size = " << subsample->Size() << std::endl;
  std::cout << "Total frequency = "
            << subsample->GetTotalFrequency() << std::endl;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // If we want to remove all instances that are associated with the
  // Subsample, we call the \code{Clear()} method. After this invocation, the
  // \code{Size()} and the \code{GetTotalFrequency()} methods return 0.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  subsample->Clear();
  std::cout << "Size = " << subsample->Size() << std::endl;
  std::cout << "Total frequency = "
            << subsample->GetTotalFrequency() << std::endl;
  // Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;
}
