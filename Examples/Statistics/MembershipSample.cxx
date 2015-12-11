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
// \index{itk::Statistics::MembershipSample}
//
// The \subdoxygen{Statistics}{MembershipSample} is derived from the class
// \subdoxygen{Statistics}{Sample} that associates a class label with each
// measurement vector. It needs another Sample object for storing measurement
// vectors. A \code{MembershipSample} object stores a subset of instance
// identifiers from another Sample object.  \emph{Any} subclass of Sample can
// be the source Sample object.  The MembershipSample class is useful for
// storing classification results from a test Sample object. The
// MembershipSample class can be considered as an associative container that
// stores measurement vectors, frequency values, and \emph{class labels}.
//
// To use a MembershipSample object, we include the header files for the
// class itself and the Sample class. We will use the
// \subdoxygen{Statistics}{ListSample} as the input sample.  We need another
// header for measurement vectors. We are going to use the \doxygen{Vector}
// class which is a subclass of the \doxygen{FixedArray}.
//
// Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkListSample.h"
#include "itkMembershipSample.h"
#include "itkVector.h"
// Software Guide : EndCodeSnippet

int main()
{
  // Software Guide : BeginLatex
  //
  // The following code snippet will create a \code{ListSample} object
  // with three-component float measurement vectors and put three
  // measurement vectors in the \code{ListSample} object.
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
  // To create a MembershipSample instance, we define the type of the
  // MembershipSample using the source sample type using the previously
  // defined \code{SampleType}. As usual, after that, we call the
  // \code{New()} method to create an instance. We must plug in the source
  // sample, Sample, using the \code{SetSample()} method. We provide class
  // labels for data instances in the Sample object using the
  // \code{AddInstance()} method. As the required initialization step for the
  // \code{membershipSample}, we must call the \code{SetNumberOfClasses()}
  // method with the number of classes. We must add all instances in the
  // source sample with their class labels. In the following code snippet, we
  // set the first instance' class label to 0, the second to 0, the third
  // (last) to 1. After this, the \code{membershipSample} has two
  // \code{Subsample} objects. And the class labels for these two
  // \code{Subsample} objects are 0 and 1. The $0$ class \code{Subsample}
  // object includes the first and second instances, and the $1$ class
  // includes the third instance.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::MembershipSample< SampleType >
    MembershipSampleType;

  MembershipSampleType::Pointer membershipSample =
    MembershipSampleType::New();

  membershipSample->SetSample(sample);
  membershipSample->SetNumberOfClasses(2);

  membershipSample->AddInstance(0U, 0UL );
  membershipSample->AddInstance(0U, 1UL );
  membershipSample->AddInstance(1U, 2UL );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The \code{Size()} and \code{GetTotalFrequency()} returns the same
  // information that Sample does.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  std::cout << "Total frequency = "
            << membershipSample->GetTotalFrequency() << std::endl;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The \code{membershipSample} is ready for use. The following code snippet
  // shows how to use the \code{Iterator} interface. The
  // MembershipSample's \code{Iterator} has an additional method
  // that returns the class label (\code{GetClassLabel()}).
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  MembershipSampleType::ConstIterator iter = membershipSample->Begin();
  while ( iter != membershipSample->End() )
    {
    std::cout << "instance identifier = " << iter.GetInstanceIdentifier()
              << "\t measurement vector = "
              << iter.GetMeasurementVector()
              << "\t frequency = "
              << iter.GetFrequency()
              << "\t class label = "
              << iter.GetClassLabel()
              << std::endl;
    ++iter;
    }
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // To see the numbers of instances in each class subsample, we use
  // the \code{Size()} method of the \code{ClassSampleType} instance
  // returned by the \code{GetClassSample(index)} method.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  std::cout << "class label = 0 sample size = "
            << membershipSample->GetClassSample(0)->Size() << std::endl;
  std::cout << "class label = 1 sample size = "
            << membershipSample->GetClassSample(1)->Size() << std::endl;
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // We call the \code{GetClassSample()} method to get the
  // class subsample in the \code{membershipSample}. The
  // \code{MembershipSampleType::ClassSampleType} is actually a
  // specialization of the \subdoxygen{Statistics}{Subsample}. We print
  // out the instance identifiers, measurement vectors, and frequency
  // values that are part of the class. The output will be two lines for
  // the two instances that belong to the class $0$.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  MembershipSampleType::ClassSampleType::ConstPointer classSample =
                                  membershipSample->GetClassSample( 0 );

  MembershipSampleType::ClassSampleType::ConstIterator c_iter =
                                                    classSample->Begin();

  while ( c_iter != classSample->End() )
    {
    std::cout << "instance identifier = " << c_iter.GetInstanceIdentifier()
              << "\t measurement vector = "
              << c_iter.GetMeasurementVector()
              << "\t frequency = "
              << c_iter.GetFrequency() << std::endl;
    ++c_iter;
    }
  // Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;

}
