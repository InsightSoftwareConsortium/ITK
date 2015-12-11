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
// This example illustrates the common interface of the \code{Sample} class
// in Insight.
//
// \index{itk::Sample!Interfaces}
// \index{itk::Statistics::ListSample}
//
// Different subclasses of \subdoxygen{Statistics}{Sample} expect different
// sets of template arguments. In this example, we use the
// \subdoxygen{Statistics}{ListSample} class that requires the type of
// measurement vectors. The ListSample uses
// \href{http://www.sgi.com/tech/stl/}{STL} \code{vector} to store
// measurement vectors. This class conforms to the common interface of Sample.
// Most methods of the Sample class interface are for retrieving measurement
// vectors, the size of a container, and the total frequency. In this
// example, we will see those information retrieving methods in addition to
// methods specific to the ListSample class for data input.
//
// To use the ListSample class, we include the header file for the class.
//
// We need another header for measurement vectors. We are going to use
// the \doxygen{Vector} class which is a subclass of the \doxygen{FixedArray}
// class.
//
// Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkListSample.h"
#include "itkVector.h"
// Software Guide : EndCodeSnippet

int main()
{
  // Software Guide : BeginLatex
  //
  // The following code snippet defines the measurement vector type as a
  // three component \code{float} \doxygen{Vector}. The
  // \code{MeasurementVectorType} is the measurement vector type in the
  // \code{SampleType}. An object is instantiated at the third line.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Vector< float, 3 > MeasurementVectorType;
  typedef itk::Statistics::ListSample< MeasurementVectorType > SampleType;
  SampleType::Pointer sample = SampleType::New();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // In the above code snippet, the namespace
  // specifier for ListSample is \code{itk::Statistics::} instead of the
  // usual namespace specifier for other ITK classes, \code{itk::}.
  //
  // The newly instantiated object does not have any data in it.  We
  // have two different ways of storing data elements. The first method
  // is using the \code{PushBack} method.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  MeasurementVectorType mv;
  mv[0] = 1.0;
  mv[1] = 2.0;
  mv[2] = 4.0;

  sample->PushBack(mv);
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The previous code increases the size of the container by one and
  // stores \code{mv} as the first data element in it.
  //
  // The other way to store data elements is calling the \code{Resize} method
  // and then calling the \code{SetMeasurementVector()} method with a
  // measurement vector. The following code snippet increases the size of the
  // container to three and stores two measurement vectors at the second and
  // the third slot. The measurement vector stored using the \code{PushBack}
  // method above is still at the first slot.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  sample->Resize(3);

  mv[0] = 2.0;
  mv[1] = 4.0;
  mv[2] = 5.0;
  sample->SetMeasurementVector(1, mv);

  mv[0] = 3.0;
  mv[1] = 8.0;
  mv[2] = 6.0;
  sample->SetMeasurementVector(2, mv);
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // We have seen how to create an ListSample object and store
  // measurement vectors using the ListSample-specific interface. The
  // following code shows the common interface of the Sample class. The
  // \code{Size} method returns the number of measurement vectors in the
  // sample.  The primary data stored in Sample subclasses are measurement
  // vectors. However, each measurement vector has its associated frequency
  // of occurrence within the sample. For the
  // ListSample and the adaptor classes (see Section
  // \ref{sec:SampleAdaptors}), the frequency value is always one.
  // \subdoxygen{Statistics}{Histogram} can have a varying frequency
  // (\code{float} type) for each measurement vector. We retrieve measurement
  // vectors using the \code{GetMeasurementVector(unsigned long instance
  // identifier)}, and frequency using the \code{GetFrequency(unsigned long
  // instance identifier)}.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  for ( unsigned long i = 0; i < sample->Size(); ++i )
    {
    std::cout << "id = " << i
              << "\t measurement vector = "
              << sample->GetMeasurementVector(i)
              << "\t frequency = "
              << sample->GetFrequency(i)
              << std::endl;
    }
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The output should look like the following:\newline
  // \code{id = 0   measurement vector = 1  2  4     frequency = 1}\newline
  // \code{id = 1   measurement vector = 2  4  5     frequency = 1}\newline
  // \code{id = 2   measurement vector = 3  8  6     frequency = 1}\newline
  //
  // We can get the same result with its iterator.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  SampleType::Iterator iter = sample->Begin();

  while( iter != sample->End() )
    {
    std::cout << "id = " << iter.GetInstanceIdentifier()
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
  // The last method defined in the Sample class
  // is the \code{GetTotalFrequency()} method that returns the sum of
  // frequency values associated with every measurement vector in a
  // container.  In the case of ListSample and the
  // adaptor classes, the return value should be exactly the same as that of
  // the \code{Size()} method, because the frequency values are always one
  // for each measurement vector. However, for the
  // \subdoxygen{Statistics}{Histogram}, the frequency values can vary.
  // Therefore, if we want to develop a general algorithm to calculate the
  // sample mean, we must use the \code{GetTotalFrequency()} method instead of
  // the \code{Size()} method.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  std::cout << "Size = " << sample->Size() << std::endl;
  std::cout << "Total frequency = "
            << sample->GetTotalFrequency() << std::endl;
  // Software Guide : EndCodeSnippet
  return EXIT_SUCCESS;
}
