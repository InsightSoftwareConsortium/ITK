/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    SampleSorting.cxx
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
// \index{Statistics!Order statistics|textbf}
// \index{Statistics!Sorting|textbf}
// \index{Statistics!Insert sort|textbf}
// \index{Statistics!Heap sort|textbf}
// \index{Statistics!Introspective sort|textbf}
// \index{Statistics!Quick select|textbf}
//
// \index{itk::Statistics::Subsample|textbf}
// \index{itk::Statistics::InsertSort|textbf}
// \index{itk::Statistics::HeapSort|textbf}
// \index{itk::Statistics::IntrospectiveSort|textbf}
// \index{itk::Statistics::QuickSelect|textbf}
//
// Sometimes, we want to sort the measurement vectors in a sample. The
// sorted sample might reveal some characteristics of the sample. In
// Insight, we have the insert sort, the heap sort, the introspective
// sort algorithms \cite{Musser1997} implemented. To learn pros and
// cons of each algorihtm, please refer to \cite{Duda2000}. We
// also have the quick select algorithm.
//
// Among the subclasses of the \subdoxygen{Statistics}{Sample}, only the
// \subdoxygen{Statistics}{Subsample} allows users to change the order
// of the measurement vector. Therefore, we must create a
// \code{Subsample} to do any sorting or selecting.
//
// We include the header files for the \code{ListSample} and the
// \code{Subsample} classes.
//
// Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkListSample.h"
#include "itkSubsample.h"
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex 
// The sorting and selecting related functions are in the
// \code{itkStatisticsAlgorithm.h}. 
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet 
#include "itkStatisticsAlgorithm.h"
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
// We need another header for measurement vectors. We are going to use
// the \doxygen{Vector} class which is a subclass of the \doxygen{FixedArray}
// in this example. 
//
// We define the types of the measurement vectors, the sample, and the
// subsample.  
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkVector.h"
// Software Guide : EndCodeSnippet

typedef int MeasurementType ;
typedef itk::Vector< MeasurementType , 2 > MeasurementVectorType ;
typedef itk::Statistics::ListSample< MeasurementVectorType > SampleType ;
typedef itk::Statistics::Subsample< SampleType > SubsampleType ;

// Software Guide : BeginLatex
// We define two functions for convenience. The first one clears the
// content of the \code{subsample} and fill it with the
// measurement vectors from the \code{sample}.
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
void initializeSubsample(SubsampleType* subsample, SampleType* sample)
{
  subsample->Clear() ;
  subsample->SetSample(sample) ;
  subsample->InitializeWithAllInstances() ;
}
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
// The second one prints out the content of the \code{subsample} using the
// \code{Subsample}'s iterator interface.
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
void printSubsample(SubsampleType* subsample, const char* header)
{
  std::cout << std::endl ;
  std::cout << header << std::endl ;
  SubsampleType::Iterator iter = subsample->Begin() ;
  while ( iter != subsample->End() )
    {
    std::cout << "instance identifier = " << iter.GetInstanceIdentifier() 
              << "\t measurement vector = " 
              << iter.GetMeasurementVector() 
              << std::endl ;
    ++iter ;
    }
}
// Software Guide : EndCodeSnippet

int main()
{
  // Software Guide : BeginLatex
  // The following code snippet will create a \code{ListSample} object
  // with two-component int measurement vectors and put the measurement
  // vectors: [5,5] - 5 times, [4,4] - 4 times, [3,3] - 3 times, [2,2] -
  // 2 times,[1,1] - 1 time  into the \code{sample}. 
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  SampleType::Pointer sample = SampleType::New() ;

  MeasurementVectorType mv ;
  for ( unsigned int i = 5 ; i > 0 ; --i )
    {
    for (unsigned int j = 0 ; j < 2 ; j++ )
      {
      mv[j] = ( MeasurementType ) i ;
      }
    for ( unsigned int j = 0 ; j < i ; j++ )
      {
      sample->PushBack(mv) ;
      }
    }
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // We create a \code{Subsample} object and plug-in the \code{sample}.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  SubsampleType::Pointer subsample = SubsampleType::New() ;
  subsample->SetSample(sample) ;
  initializeSubsample(subsample, sample) ;
  printSubsample(subsample, "Unsorted") ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // The common arguments for the all algorithms are the
  // \code{Subsample} object (\code{subsample}), the dimension
  // (\code{activeDimension}) that will be considered for the sorting or
  // selecting (only the component belonging to the dimension of the
  // measurement vectors will be considered), the beginning index, and
  // the ending index of the measurement vectors in the
  // \code{subsample}. The sorting or selecting algorithms are applied
  // only to the range specified by the beginning index and the ending
  // index. The ending index should be the actual last index plus one.
  // 
  // The \subdoxygen{Statistics}{InsertSort} function does not require any
  // other optional argument. The following function call will sort the
  // all measurement vectors in the \code{subsample}. The beginning
  // index is \code{0}, and the ending index is the number of the
  // measurement vectors in the \code{subsample}.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  int activeDimension = 0 ; 

  itk::Statistics::InsertSort< SubsampleType >( subsample, activeDimension,
                                                0, subsample->Size() ) ;
  printSubsample(subsample, "InsertSort") ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // We sort the \code{subsample} using the heap sort algorithm. The
  // arguments are identical to those of the insert sort.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  initializeSubsample(subsample, sample) ;
  itk::Statistics::HeapSort< SubsampleType >( subsample, activeDimension,
                                              0, subsample->Size() ) ;
  printSubsample(subsample, "HeapSort") ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // The introspective sort needs an additional argument that specifies
  // when to stop sort introspective sort loop and sort the fragment of
  // the sample using the heap sort algorithm. Since we set the
  // threshold value as \code{16}, when the sort loop reach the point
  // where the number of measurement vectors in a sort loop is not
  // greater than 16, it will sort that fragment using the insert sort
  // algorithm.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  initializeSubsample(subsample, sample) ;
  itk::Statistics::IntrospectiveSort< SubsampleType >
    ( subsample, activeDimension, 0, subsample->Size(), 16 ) ;
  printSubsample(subsample, "IntrospectiveSort") ;
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  // We query the median of the measurements along the
  // \code{activeDimension}. The last argument tells the algorithm that
  // we want to get the \code{subsample->Size()/2}-th element along the
  // \code{activeDimension}. The quick select algorithm changes the
  // order of the measurment vectors.
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  initializeSubsample(subsample, sample) ;
  SubsampleType::MeasurementType median = 
    itk::Statistics::QuickSelect< SubsampleType >( subsample, activeDimension, 
                                                   0, subsample->Size(),
                                                   subsample->Size()/2 ) ;
  std::cout << std::endl ;
  std::cout << "Quick Select: median = " << median << std::endl ;
  // Software Guide : EndCodeSnippet

  return 0 ;
}
