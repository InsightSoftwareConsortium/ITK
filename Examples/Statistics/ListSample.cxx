/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ListSample.cxx
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
// This example illustrates the common interface of the \code{Sample} classes
// in Insight.
//
// \index{itk::Sample!Interfaces|textbf}
// \index{itk::Statistics::ListSample|textbf}
//
// Different subclasses of \subdoxygen{Statistics}{Sample} expect
// different sets of template arguments. In this example, we use the
// \subdoxygen{Statistics}{ListSample} class that requires the type of
// measurement vectors. The \subdoxygen{Statistics}{ListSample} uses STL
// \code{vector} to store measurement vectors. This class confirms to
// the common iterface of \subdoxygen{Statistics}{Sample}.  Most methods
// of the \subdoxygen{Statistics}{Sample} class interface are for
// retrieving measurement vectors, the size of a container, and the
// total frequency. In this example, we will see those information
// retrieving methods in addition to methods specific to the
// \subdoxygen{Statistics}{ListSample} class for data input.
//
// To use an \subdoxygen{Statistics}{ListSample} object, we include the
// header file for the class.
//
// We need another header for measurement vectors. We are going to use
// the \doxygen{Vector} class which is a subclass of the \doxygen{FixedArray}
// in this example. 
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkListSample.h"
#include "itkVector.h"
// Software Guide : EndCodeSnippet

int main()
{
  // Software Guide : BeginLatex
  // The following code snippet defines the measurement vector type as
  // three component \code{float} \doxygen{Vector}. The
  // \code{MeasurementVectorType} is the measurement vector type in the
  // \code{SampleType}. An object is instantiated at the third line. 
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Vector< float, 3 > MeasurementVectorType ;
  typedef itk::Statistics::ListSample< MeasurementVectorType > SampleType ;
  SampleType::Pointer sample = SampleType::New() ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // In the above code snippet, the namespace specifier for 
  // \subdoxygen{Statistics}{ListSample} is \code{itk::Statistics::} 
  // instead of the usual
  // namespace specifier for other Insight classes, \code{itk::}.
  //
  // The newly instantiated object does not have any data in it.  We
  // have two different ways of storing data elements. The first method
  // is using the \code{PushBack} method.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  MeasurementVectorType mv ;
  mv[0] = 1.0 ;
  mv[1] = 2.0 ;
  mv[2] = 4.0 ;
  
  sample->PushBack(mv) ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // The previous code increases the size of the container by one and
  // stores \code{mv} as the first data element in it.
  //
  // The other way to store data elements is calling the \code{Resize} method 
  // and then calling the \code{SetMeasurementVector} method with a measurement
  // vector. The following code snippet increases the size of the container 
  // to three and stores two measurement vectors at the second and the third
  // slot. The measurement vector stored using the \code{PushBack} method above
  // is still at the first slot.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  sample->Resize(3) ;

  mv[0] = 2.0 ;
  mv[1] = 4.0 ;
  mv[2] = 5.0 ;
  sample->SetMeasurementVector(1, mv) ;
  
  mv[0] = 3.0 ;
  mv[1] = 8.0 ;
  mv[2] = 6.0 ;
  sample->SetMeasurementVector(2, mv) ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // Now that we have seen how to create an
  // \subdoxygen{Statistics}{ListSample} object and store measurement
  // vectors using \subdoxygen{Statistics}{ListSample}-specific
  // interface. The following codes show the common interface of the
  // \subdoxygen{Statistics}{Sample}. The \code{Size} method returns
  // the number of measurement vectors in the sample.  The primary
  // data stored in an \subdoxygen{Statistics}{Sample} subclasses are
  // measurement vectors. However, each measurement vector has its
  // associated frequency of occurence within the sample. For the
  // \subdoxygen{Statistics}{ListSample} and the adaptor classes (see
  // \ref{sec:SampleAdaptors}), the frequency value is always one.
  // \subdoxygen{Statistics}{Histogram} can have a varying frequency
  // (\code{float} type) for each measurement vector. We retrieve
  // measurement vectors using the \code{GetMeasurementVector(unsigned
  // long instance identifier)}, and frequency using the
  // \code{GetFrequency(unsigned long instance identifier)}.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  for ( unsigned long i = 0 ; i < sample->Size() ; ++i )
    {
      std::cout << "id = " << i 
                << "\t measurement vector = " 
                << sample->GetMeasurementVector(i) 
                << "\t frequency = " 
                << sample->GetFrequency(i) 
                << std::endl ;
    }
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // The output should look like the following:\newline
  // \code{id = 0   measurement vector = 1  2  4     frequency = 1}\newline
  // \code{id = 1   measurement vector = 2  4  5     frequency = 1}\newline
  // \code{id = 2   measurement vector = 3  8  6     frequency = 1}\newline
  //
  // We can get the same result with its iterator.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  SampleType::Iterator iter = sample->Begin() ;

  while( iter != sample->End() )
    {
      std::cout << "id = " << iter.GetInstanceIdentifier()  
                << "\t measurement vector = " 
                << iter.GetMeasurementVector() 
                << "\t frequency = " 
                << iter.GetFrequency() 
                << std::endl ;
      ++iter ;
    }
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // The last method defined in the \subdoxygen{Statistics}{Sample} is
  // the \code{GetTotalFrequency(void)} method that returns the sum of
  // frequency values associated with every measurement vector in a
  // container.  In the case of \subdoxygen{Statistics}{ListSample}
  // and the adaptor classes, the return value should be exactly same
  // as that of \code{Size()} method, because the frequency values are
  // always one for each measurement vector. However, for the
  // \subdoxygen{Statistics}{Histogram}, the frequency values can
  // vary.  Therefore, if we want to develop a general algorithm to
  // calculate the sample mean, we must use the
  // \code{GetTotalFrequency} method instead of the \code{Size}
  // method.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  std::cout << "Size = " << sample->Size() << std::endl ;
  std::cout << "Total frequency = " 
            << sample->GetTotalFrequency() << std::endl ;
  // Software Guide : EndCodeSnippet
  return 0 ;
}
