/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    Subsample.cxx
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
// \index{itk::Statistics::Subsample|textbf}
//
// The \subdoxygen{Statistics}{Subsample} is a derived sample. In
// other words, it needs another \code{Sample} object for storing
// measurment vectors. A \code{Subsample} object stores subset of
// instance identifiersis of another \code{Sample} object.
// \textbf{Any} \code{Sample}'s subclass can be the source
// \code{Sample} object. You can create a \code{Subsample} object out
// of another \code{Subsample} object. The \code{Subsample} class is
// useful for storing classification results from a test \code{Sample}
// object or for just extracting some part of interest in a
// \code{Sample} object. Another good use of \code{Subsample} is
// sorting a \code{Sample} object. When we use an \code{Image} object
// as the data source, we do not want to change the order of data
// element in that \code{Image} object. However, we sometimes want to
// sort or select data elements according to their order. Statistics
// algorithms for this purpose accepts only \code{Subsample} objects
// as inputs. Changing the order in a \code{Subsample} object does
// not change the order of the source sample.
//
// To use a \code{Subsample} object, we include the header files for
// the class itself and a \code{Sample} class. We will use the
// \code{ListSample} as the input sample.
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkListSample.h"
#include "itkSubsample.h"
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
// We need another header for measurement vectors. We are going to use
// the \doxygen{Vector} class in this example. 
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkVector.h"
// Software Guide : EndCodeSnippet

int main()
{
  // Software Guide : BeginLatex
  // The following code snippet will create a \code{ListSample} object
  // with three-component float measurement vectors and put three
  // measurement vectors in the \code{ListSample} object.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Vector< float, 3 > MeasurementVectorType ;
  typedef itk::Statistics::ListSample< MeasurementVectorType > SampleType ;
  SampleType::Pointer sample = SampleType::New() ;
  MeasurementVectorType mv ;
  mv[0] = 1.0 ;
  mv[1] = 2.0 ;
  mv[2] = 4.0 ;
  
  sample->PushBack(mv) ;

  mv[0] = 2.0 ;
  mv[1] = 4.0 ;
  mv[2] = 5.0 ;
  sample->PushBack(mv) ;
  
  mv[0] = 3.0 ;
  mv[1] = 8.0 ;
  mv[2] = 6.0 ;
  sample->PushBack(mv) ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // To create a \code{Subsample} instance, we define the type of the
  // \code{Subsample} with the source sample type, in this case, the
  // previously defined \code{SampleType}. As usual, after that, we
  // call the \code{New()} method to instantiate an instance. We must
  // plug in the source sample, \code{sample}, using the
  // \code{SetSample} method. However, with regard to data elements,
  // the \code{subsample} is empty. We specify which data elements,
  // among the data elements in the \code{sample} object, are part of
  // the \code{subsample}. There are two ways of doing that. First, if
  // we want to include every data element (instance) from the
  // \code{sample}, we simply call the
  // \code{InitializeWithAllInstances()} method like the following:
  //
  // \code{subsample->InitializeWithAllInstances() ;}
  //
  // This method is useful when we want to create a \code{Subsample}
  // object for sorting all the data elements in a \code{Sample}
  // object. However, in most cases, we want to include only a subset of
  // a \code{Sample} object. For this purpose, we use the
  // \code{AddInstance(instance identifier)} method in this example. In
  // the following code snippet, we include only the first and last
  // instance in our \code{subsample} object among the three instances
  // of the \code{sample}.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Statistics::Subsample< SampleType > SubsampleType ;
  SubsampleType::Pointer subsample = SubsampleType::New() ;
  subsample->SetSample( sample ) ;
  
  subsample->AddInstance( 0UL ) ;
  subsample->AddInstance( 2UL ) ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // The \code{subsample} is ready for use. The following code snippet
  // shows how to use \code{Iterator} interfaces. 
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  SubsampleType::Iterator iter = subsample->Begin() ;
  while ( iter != subsample->End() )
    {
    std::cout << "instance identifier = " << iter.GetInstanceIdentifier() 
              << "\t measurement vector = " 
              << iter.GetMeasurementVector() 
              << "\t frequency = " 
              << iter.GetFrequency()
              << std::endl ;
    ++iter ;
    }
  // Software Guide : EndCodeSnippet

  
  // Software Guide : BeginLatex
  // As mentioned earlier, the instances in a \code{Subsample} can be
  // sorted without changing the order in the source
  // \code{Sample}. For this purpose, the \code{Subsample} provides an
  // additional instance indexing scheme. The indexing scheme is just
  // like the instance identifiers for the \code{Sample}. The index is
  // an integer value starts at 0, and the last value is one less than
  // the number of all instances in a \code{Subsample}. The
  // \code{Swap(0, 1)} method, for instasnce, swaps two instance
  // identifiers of the first data element and the second element in
  // the \code{subsample}. Internally, the \code{Swap} function
  // changes the instance identifiers in the first and second
  // position. Using indexes, we can print out the effects of the
  // \code{Swap} function. We use the
  // \code{GetMeasurementVectorByIndex(index)} to get the measurement
  // vector at the index position. However, if we want to use the
  // common methods of the \code{Sample} that accepts instance
  // identifiers, we call them after we get the instance identifiers
  // using \code{GetInstanceIdentifier(index)} method.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  subsample->Swap(0, 1) ;

  for ( int index = 0 ; index < subsample->Size() ; ++index )
    {
    std::cout << "instance identifier = " 
              << subsample->GetInstanceIdentifier(index) 
              << "\t measurement vector = " 
              << subsample->GetMeasurementVectorByIndex(index) 
              << std::endl ;
    }
  // Software Guide : EndCodeSnippet
  
  // Software Guide : BeginLatex
  // Since we are using a \code{ListSample} object as the souce sample,
  // the following code snippet will return the same value (2) for the
  // \code{Size()} and the \code{GetTotalFrequency()} methods. However,
  // if we used a \code{Histogram} object as the source sample, the two
  // return values might be different because a \code{Histogram} allows
  // varying frequency values for each instance.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  std::cout << "Size = " << subsample->Size() << std::endl ;
  std::cout << "Total frequency = " 
            << subsample->GetTotalFrequency() << std::endl ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // If we want to remove all instances that are already included in
  // the \code{subsample}, we call the \code{Clear()} method. After
  // this function call, the \code{Size()} and the
  // \code{GetTotalFrequency()} methods return 0.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  subsample->Clear() ;
  std::cout << "Size = " << subsample->Size() << std::endl ;
  std::cout << "Total frequency = " 
            << subsample->GetTotalFrequency() << std::endl ;
  // Software Guide : EndCodeSnippet
  
  return 0 ;
}
