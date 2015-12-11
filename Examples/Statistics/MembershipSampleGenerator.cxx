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
// \index{itk::Statistics::List\-Sample\-To\-Histogram\-Filter}
// \index{itk::Statistics::List\-Sample\-To\-Histogram\-Generator}
// \index{itk::Statistics::Neighborhood\-Sampler}
// \index{itk::Statistics::Sample\-To\-Histogram\-Projection\-Filter}
// \index{itk::Statistics::Selective\-Subsample\-Generator}
// \index{itk::Statistics::Membership\-Sample\-Generator}
//
// To use, an \code{MembershipSample} object, we include the header files for
// the class itself and a \code{Sample} class. We will use the
// \code{ListSample} as the input sample.
//
// Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkListSample.h"
#include "itkMembershipSample.h"
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
  // To create a \code{MembershipSample} instance, we define the type of the
  // \code{MembershipSample} with the source sample type, in this case,
  // previously defined \code{SampleType}. As usual, after that, we call
  // \code{New()} method to instantiate an instance. We must plug in the
  // source sample, \code{sample} object using the
  // \code{SetSample(source sample)} method. However, in regard to
  // \textbf{class labels}, the \code{membershipSample} is empty. We
  // provide class labels for data instances in the \code{sample} object
  // using the \code{AddInstance(class label, instance identifier)}
  // method. As the required initialization step for the
  // \code{membershipSample}, we must call the
  // \code{SetNumberOfClasses(number of classes)} method with the number
  // of classes. We must add all instances in the source sample with
  // their class labels. In the following code snippet, we set the first
  // instance class label to 0, the second to 0, the third (last) to
  // 1. After this, the \code{membershipSample} has two \code{Subclass}
  // objects. And the class labels for these two \code{Subclass} are 0
  // and 1. The \textbf{0} class \code{Subsample} object includes the
  // first and second instances, and the \textbf{1} class includes the
  // third instance.
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
  // The \code{Size()} and \code{GetTotalFrequency()} methods return the same
  // values as the \code{sample}.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  std::cout << "Size = " << membershipSample->Size() << std::endl;
  std::cout << "Total frequency = "
            << membershipSample->GetTotalFrequency() << std::endl;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The \code{membershipSample} is ready for use. The following code snippet
  // shows how to use \code{Iterator} interfaces. The
  // \code{MembershipSample} \code{Iterator} has an additional method
  // that returns the class label (\code{GetClassLabel()}).
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  MembershipSampleType::Iterator iter = membershipSample->Begin();
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
  // the \code{GetClassSampleSize(class label)} method.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  std::cout << "class label = 0 sample size = "
            << membershipSample->GetClassSampleSize(0) << std::endl;
  std::cout << "class label = 1 sample size = "
            << membershipSample->GetClassSampleSize(0) << std::endl;
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // We call the \code{GetClassSample(class label)} method to get the
  // class subsample in the \code{membershipSample}. The
  // \code{MembershipSampleType::ClassSampleType} is actually an
  // specialization of the \subdoxygen{Statistics}{Subsample}. We print
  // out the instance identifiers, measurement vectors, and frequency
  // values that are part of the class. The output will be two lines for
  // the two instances that belong to the class \textbf{0}.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  MembershipSampleType::ClassSampleType::Pointer classSample =
    membershipSample->GetClassSample(0);
  MembershipSampleType::ClassSampleType::Iterator c_iter =
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
